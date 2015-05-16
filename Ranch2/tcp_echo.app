%% Feel free to use, reuse and abuse the code in this file.

{application, tcp_echo, [
	{description, "Ranch TCP echo example."},
	{vsn, "1"},
	{modules, ['echo_protocol', 'tcp_echo_app', 'tcp_echo_sup']},
	{registered, [tcp_echo_sup]},
	{applications, [
		kernel,
		stdlib,
		ranch
	]},
	{mod, {tcp_echo_app, []}},
	{env, []}
]}.
