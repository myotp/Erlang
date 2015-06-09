{application, time_server,
 [{description, "A simple time server OTP application"},
  {vsn, "1.0.0"},
  {modules, [ ts_app
            , ts_sup
            , ts_listen_server
            , ts_http_sup
            , ts_http_server
            ]},
  {registered, [ts_sup, ts_http_sup]},
  {applications, [kernel, stdlib]},
  {mod, {ts_app, []}}
 ]
}.
