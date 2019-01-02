%% This is the application resource file (.app file) for the 'base'
%% application.
{application, dns,
[{description, "dns  " },
{vsn, "1.0.0" },
{modules, 
	  [dns_app,dns_sup,dns,dns_lib]},
{registered,[dns]},
{applications, [kernel,stdlib]},
{mod, {dns_app,[]}},
{start_phases, []}
]}.
