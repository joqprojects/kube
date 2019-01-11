%% This is the application resource file (.app file) for the 'base'
%% application.
{application, catalog,
[{description, "catalog  " },
{vsn, "1.0.0" },
{modules, 
	  [catalog_lib,
	   catalog_app,catalog_sup,catalog]},
{registered,[catalog]},
{applications, [kernel,stdlib]},
{mod, {catalog_app,[]}},
{start_phases, []}
]}.
