# webapp-spock

A pretty basic webapp which was my playground for tinkering with different
concepts that come with Haskell and [Spock](https://github.com/agrafix/Spock)
framework.

In order to use it, you need `postgresql` server running. I've
included `db_dump.sql` which creates necessary tables and fills them
with some data (you need to manually create database `employees`
first, though).

This app uses `stack` for fetching dependencies, building binaries,
testing and executing them - please refer to
[Stack's docs](http://docs.haskellstack.org/en/stable/README.html) if
you are not familiar with this tool. Remember to set correct paths for
`postgresql` and `zlib` libs/headers in `stack.yaml`
(with `extra-include-dirs` and `extra-lib-dirs`)!

After setting everything up , you just need to edit `app.cfg` file and
fill it with credentials for your database and this app. By default,
it will run on port 8000 (which of course can be changed too).
