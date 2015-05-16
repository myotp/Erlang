%% Feel free to use, reuse and abuse the code in this file.

{application, rest_hello_world, [
	{description, "Cowboy REST Hello World example."},
	{vsn, "1"},
	{modules, ['rest_hello_world_app', 'rest_hello_world_sup', 'toppage_handler']},
	{registered, [rest_hello_world_sup]},
	{applications, [
		kernel,
		stdlib,
		cowboy
	]},
	{mod, {rest_hello_world_app, []}},
	{env, []}
]}.
