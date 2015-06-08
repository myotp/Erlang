-module(url_filters).

-compile(export_all).

skip_file_types() ->
    [".jpg", ".jpeg", ".pdf", ".css", ".js"].

