%% This is the application resource file (.app file) for the 'base'
%% application.
{application, log,
[{description, "log  " },
{vsn, "1.0.0" },
{modules, 
	  [log_app,log_sup,log]},
{registered,[log]},
{applications, [kernel,stdlib]},
{mod, {log_app,[]}},
{start_phases, []}
]}.
