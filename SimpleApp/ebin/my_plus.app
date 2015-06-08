{application, my_plus,
 [{description, "A simple demo OTP application"},
  {vsn, "1.0.0"},
  {modules, [plus_app,
             plus_sup,
             plus_server]},
  {registered, [plus_sup]},
  {applications, [kernel, stdlib]},
  {mod, {plus_app, []}}
 ]
}.
