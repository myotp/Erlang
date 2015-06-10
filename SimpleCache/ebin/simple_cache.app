{application, simple_cache,
 [{description, "A simple caching system"},
  {vsn, "0.1.0"},
  {modules, [ sc_app
            , sc_sup
            , sc_element
            , sc_store
            , simple_cache
            ]},
  {registered, [sc_sup]},
  {applications, [kernel, stdlib]},
  {mod, {sc_app, []}}
]}.
