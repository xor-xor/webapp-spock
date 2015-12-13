# webapp_scotty

This is a Haskell version of
[webapp_flask](https://github.com/xor-xor/webapp_flask) that I wrote
some time ago. It is pretty basic - I use it as a playground for
tinkering with different concepts that come with Haskell and
[Spock](https://github.com/agrafix/Spock) framework. I'm going to
extend and polish it a bit it in the future (use `persistent` and
`esqueleto` instead of raw SQL queries, `Web.Spock.Safe` instead of
`Web.Spock.Simple`, add some more tests and some additional
functionality etc.).

In order to use it, you need `postgresql` server running. I've
included `db_dump.sql` which creates necessary tables and fills them
with some data (you need to manually create database `employees`
first, though). Since I'm running NixOS, I'm just providing
`shell.nix` file instead of its `.cabal` equivalent (sorry for that!).

After setting everything up, you just need to edit `app.cfg` file and
fill it with credentials for your database and this app. By default,
it will run on port 8000 (which of course can be changed too).