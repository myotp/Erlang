{application, tcp_server,
 [{description, "A nice parallel TCP server"},
  {vsn, "1.0.0"},
  {modules, [ ts_app
            , ts_sup
            , ts_server
            ]},
  {registered, [ts_sup]},
  {applications, [kernel, stdlib]},
  {mod, {ts_app, []}}
 ]
}.
