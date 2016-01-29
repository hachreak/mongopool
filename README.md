mongopool
=========

mongopool: mongodb + poolboy

A MongoDB connection pool server application.


Configuration
-------------

```erlang
[
  {mongopool, [
    {pools, [
      {mypool, [
        {size, 10},
        {max_overflow, 30}
      ], [
        {database, <<"mydb">>},
        {hostname, mongohostname},
        {username, "dbuser"},
        {password, "dbpassword"},
        {w_mode, safe}
      ]}
    ]}
  ]},
]
```

Note: if you doesn't enable the authorization on mongodb, you don't need to
specify `username` and `password`.

Note: you can specify all the pool that you need.

Usage
-----

To start the pool:

```erlang
application:ensure_all_started(mongopool).
```

The API available is simply a proxy to mongodb`-erlang` API.
Instead of pass the `Connection`, you pass the pool name.

E.g. to find inside a collection:

```erlang
MyDocument = mongopool_app:find_one(
    mypool, Collection, #{<<"_id">> => <<"mydocument_id">>}).
```

Build
-----

    $ utils/rebar3 compile


Deprecated
----------

`MongoDB-Erlang >= 0.8.2` change API and inglobe the use of `poolboy`.
So, after this version, `mongopool` became incompatible and useless.

For that reason, if you want use a recent release of the driver, please use
directly [MongoDB-Erlang](https://github.com/comtihon/mongodb-erlang).

Thanks for use `MongoPool`.
