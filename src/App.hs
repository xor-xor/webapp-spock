{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App (runApp, appToTest) where

import Control.Monad (when)
import qualified Data.Configurator as C
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid
import Network.HTTP.Types.Status
import Network.Wai (Application)
import Network.Wai.Middleware.Static (staticPolicy, addBase, (>->), noDots)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Spock.Simple
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg


-------------------------------------------------------------------------------
-- Types, instances etc.

data AppCfg = AppCfg
    { cfgDbName      :: String
    , cfgDbUser      :: String
    , cfgDbPassword  :: String
    , cfgAppUser     :: Text
    , cfgAppPassword :: Text
    , cfgAppPort     :: Int
    } deriving (Show)

data AppState = AppState
    { stAppCfg :: AppCfg
    } deriving (Show)

data MySession = MySession
    { visitedCounter :: Integer
    , loggedInAs     :: Text
    , message        :: Maybe Text
    , messageType    :: Maybe MessageType
    } deriving (Show)

data MessageType = MsgError | MsgInfo deriving Show

setMessage :: MessageType -> Text -> MySession -> MySession
setMessage t m s = MySession (visitedCounter s) (loggedInAs s) (Just m) (Just t)

clearMessage :: SpockAction Pg.Connection MySession AppState ()
clearMessage = do
  sess <- readSession
  writeSession (MySession (visitedCounter sess) (loggedInAs sess) Nothing Nothing)

data Credentials = Credentials
    { user :: Text
    , pass :: Text
    } deriving (Show)

type EmployeeID = Int

data Employee = Employee
    { employeeId     :: EmployeeID
    , employeeName   :: Text
    , employeeSalary :: Double
    } deriving (Show)

data EmployeeDetails = EmployeeDetails
    { edId       :: EmployeeID
    , edName     :: Text
    , edPosition :: Text
    , edEmplDate :: LocalTime
    , edUnit     :: Text
    } deriving (Show)

instance Pg.FromRow Employee where
  fromRow = do
    eId    <- Pg.field
    name   <- Pg.field
    salary <- Pg.field
    return (Employee eId name salary)

instance Pg.FromRow EmployeeDetails where
  fromRow = do
      eId       <- Pg.field
      name      <- Pg.field
      position  <- Pg.field
      empl_date <- Pg.field
      unit      <- Pg.field
      return (EmployeeDetails eId name position empl_date unit)


-------------------------------------------------------------------------------
-- Configuration, sessions etc.

parseConfig :: FilePath -> IO AppCfg
parseConfig cfgFile = do
  cfg         <- C.load [C.Required cfgFile]
  dbName      <- C.require cfg "dbName"
  dbUser      <- C.require cfg "dbUser"
  dbPassword  <- C.require cfg "dbPassword"
  appUser     <- C.require cfg "appUser"
  appPassword <- C.require cfg "appPassword"
  port        <- C.require cfg "port"
  return (AppCfg dbName dbUser dbPassword appUser appPassword port)

getSpockCfg :: AppCfg -> SpockCfg Pg.Connection MySession AppState
getSpockCfg appCfg = SpockCfg { spc_initialState = AppState appCfg
                              , spc_database = getDbConn appCfg
                              , spc_sessionCfg = getSessionConfig
                              , spc_maxRequestSize = Nothing
                              }


getSessionConfig :: SessionCfg MySession
getSessionConfig = defaultSessionCfg emptySession

emptySession :: MySession
emptySession = MySession { visitedCounter = 0
                         , loggedInAs     = ""
                         , message        = Nothing
                         , messageType    = Nothing
                         }

getDbConn :: AppCfg -> PoolOrConn Pg.Connection
getDbConn appCfg = PCConn (ConnBuilder createConn destroyConn poolCfg)
    where createConn = Pg.connect connectInfo
          destroyConn = Pg.close
          poolCfg = PoolCfg stripes resPerStripe keepOpentime
          stripes = 5
          resPerStripe = 5
          keepOpentime = 60
          connectInfo = Pg.defaultConnectInfo { Pg.connectUser = cfgDbUser appCfg
                                              , Pg.connectPassword = cfgDbPassword appCfg
                                              , Pg.connectDatabase = cfgDbName appCfg
                                              }

-------------------------------------------------------------------------------
-- Main, definitions of routes etc.

runApp :: IO ()
runApp = do
  c <- parseConfig "app.cfg"
  runSpock (cfgAppPort c) (middleware' c)
      where middleware' cfg = spock (getSpockCfg cfg) app

app :: SpockM Pg.Connection MySession AppState ()
app = do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "static")
  get "/" $ redirect "/login"
  get "/login" login
  post "/login" processLogin
  get "/logout" logout
  get "/employees" getEmployees
  get ("employee" <//> ":id") getEmployeeDetails
  get "/health" healthCheck

-- | Same as @app@, used only for tests with Hspec.
appToTest :: String -> String -> IO Application
appToTest testUser testPassword = do
  appCfg <- parseConfig "app.cfg"
  -- XXX I really don't like this boilerplate below
  let testAppCfg = AppCfg { cfgDbName      = cfgDbName appCfg
                          , cfgDbUser      = cfgDbUser appCfg
                          , cfgDbPassword  = cfgDbPassword appCfg
                          , cfgAppUser     = pack testUser
                          , cfgAppPassword = pack testPassword
                          , cfgAppPort     = cfgAppPort appCfg
                          }
  spockAsApp $ spock (getSpockCfg testAppCfg) app


-------------------------------------------------------------------------------
-- Handlers for routes and some helpers functions.

login :: SpockAction Pg.Connection MySession AppState ()
login = do
  s <- readSession
  clearMessage
  lucid (pageTemplate
         (fieldset_ (do h1_ "Please log in"
                        case getMsgBarClass s of
                          Just c -> p_ [class_ c] $ toHtml (fromMaybe "" (message s)) -- XXX I don't like it
                          _      -> return ()
                        form_ [name_ "login", action_ "/login", method_ "post"] formContent)))
    where formContent = do label_ [for_ "username"] "Username:"
                           input_ [type_ "text", name_ "username", required_ ""]
                           label_ [for_ "password"] "Password:"
                           input_ [type_ "password", name_ "password", required_ ""]
                           input_ [type_ "submit", class_ "login", value_ "Login"]
                           input_ [type_ "reset", value_ "Reset"]

getMsgBarClass :: MySession -> Maybe Text
getMsgBarClass s = case messageType s of
    Just MsgError -> Just "error"
    Just MsgInfo  -> Just "info"
    _             -> Nothing

authUser :: AppCfg -> Credentials -> Bool
authUser a c = user c == cfgAppUser a && pass c == cfgAppPassword a

getCredsFromParams :: [(Text, Text)] -> Credentials
getCredsFromParams = foldl getCreds Credentials { user = "", pass = "" }
    where getCreds c ("username", username) = Credentials { user = username, pass = pass c }
          getCreds c ("password", password) = Credentials { pass = password, user = user c }
          getCreds c _ = c

processLogin :: SpockAction Pg.Connection MySession AppState ()
processLogin = do
  p <- params
  s <- getState
  if authUser (stAppCfg s) (getCredsFromParams p)
  then do
    writeSession MySession { visitedCounter = 0
                           , loggedInAs = cfgAppUser (stAppCfg s)
                           , message = Nothing
                           , messageType = Nothing
                           }
    redirect "/employees"
  else do
    modifySession (setMessage MsgError "Incorrect login/password.")
    redirect "/login"

logout :: SpockAction Pg.Connection MySession AppState ()
logout = do
  modifySession (setMessage MsgInfo "You've successfully logged out.")
  redirect "/login"

-- Not really necessary for anything, but...
healthCheck :: SpockAction Pg.Connection MySession AppState ()
healthCheck = do
  setStatus status200
  text "OK"

getEmployees :: SpockAction Pg.Connection MySession AppState ()
getEmployees = do
   requireLogin
   s <- modifyReadSession incVisitedCounter
   allEmployees <- runQuery fetchAllEmployees
   lucid (pageTemplate
          (do p_ (do "You're logged in as: "
                     span_ [class_ "user"] $ toHtml $ loggedInAs s
                     br_ []
                     toHtml $ "You've accessed this page " ++
                            show (visitedCounter s) ++ " times."
                     br_ []
                     link "/logout" "Logout")
              renderEmployees allEmployees
              br_ []
              "("
              span_ [class_ "gt"] "green"
              " - above average; "
              span_ [class_ "le"] "red"
              " - below/equal)"))

incVisitedCounter :: MySession -> MySession
incVisitedCounter s = MySession (visitedCounter s + 1) (loggedInAs s) (message s) (messageType s)

requireLogin :: SpockAction Pg.Connection MySession AppState ()
requireLogin = do
  s <- readSession
  when (loggedInAs s == "") $ redirect "/login"

getEmployeeDetails :: SpockAction Pg.Connection MySession AppState ()
getEmployeeDetails = do
  eId <- param' "id"
  employeeDetails <- runQuery $ fetchEmployeeDetails eId
  case employeeDetails of
    Nothing -> do
      setStatus status404
      text "Not found"
    Just x  -> lucid (pageTemplate $ renderEmployeeDetails x)


-------------------------------------------------------------------------------
-- Lucid.

pageTemplate :: Html () -> Html ()
pageTemplate contents =
  doctypehtml_ (do head_ (do title_ "Employees Web App"
                             meta_ [charset_ "utf-8"]
                             link_ [ rel_ "stylesheet"
                                   , type_ "text/css"
                                   , href_ "/css/stylesheet.css"
                                   ])
                   body_ contents)

link :: Text -> Html () -> Html ()
link url = a_ [href_ url]

lucid :: Html () -> SpockAction database MySession AppState ()
lucid document = html (toStrict (renderText document))

renderEmployees :: [Employee] -> Html ()
renderEmployees employees =
  table_ (do thead_ (tr_ (do th_ "Id"
                             th_ "Name"
                             th_ "Salary"))
             tbody_ (foldMap employeeToRow' employees))
  where employeeToRow' = employeeToRow $ getAvgSalary employees

getAvgSalary :: [Employee] -> Double
getAvgSalary es = sum (map employeeSalary es) / fromIntegral (length es)

employeeToRow :: Double -> Employee -> Html ()
employeeToRow avg employee =
  tr_ (do td_ (toHtml eId)
          td_ (link (pack $ "/employee/" ++ eId) (toHtml eName))
          td_ (span_ [class_ $ getClass eSalary] (toHtml $ show eSalary)))
    where getClass s = if s > avg then "gt" else "le"
          eName = employeeName employee
          eId = show $ employeeId employee
          eSalary = employeeSalary employee

renderEmployeeDetails :: EmployeeDetails -> Html ()
renderEmployeeDetails ed =
  do table_ (tbody_ (do tr_ (do td_ "ID_EMPLOYEE"
                                td_ . toHtml . show $ edId  ed)
                        tr_ (do td_ "NAME"
                                td_ . toHtml $ edName ed)
                        tr_ (do td_ "POSITION"
                                td_ . toHtml $ edPosition ed)
                        tr_ (do td_ "EMPLOYED_FROM"
                                td_ . toHtml . formatDate $ edEmplDate ed)
                        tr_ (do td_ "UNIT"
                                td_ . toHtml $ edUnit ed)))
     link "/employees" (span_ [style_ "margin-right:10px"] "Back")
     link "/logout" "Logout"
         where formatDate = formatTime defaultTimeLocale "%Y/%m/%e"


-------------------------------------------------------------------------------
-- DB querying.

fetchAllEmployees :: Pg.Connection -> IO [Employee]
fetchAllEmployees dbConn_ = Pg.query_ dbConn_ sqlListAllEmployees

sqlListAllEmployees :: Pg.Query
sqlListAllEmployees =
  [sql| SELECT id, name, salary
        FROM employee
        ORDER BY id |]

fetchEmployeeDetails :: EmployeeID -> Pg.Connection -> IO (Maybe EmployeeDetails)
fetchEmployeeDetails eId dbConn_ = do
  employeeDetailsList <- Pg.query dbConn_ sqlGetEmployeeDetails [eId]
  case employeeDetailsList of
    []    -> return Nothing
    (x:_) -> return (Just x)

sqlGetEmployeeDetails :: Pg.Query
sqlGetEmployeeDetails =
  [sql| SELECT e.id, e.name, p.name, e.employed_from, u.name
        FROM employee AS e
        LEFT JOIN unit AS u ON e.unit = u.id
        LEFT JOIN position AS p ON e.position = p.id
        WHERE e.id = ? |]
