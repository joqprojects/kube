%% This is the application resource file (.app file) for the 'base'
%% application.
{application, nfvi,
[{description, "nfvi  " },
{vsn, "1.0.0" },
{modules, 
	  [nfvi_app,nfvi_sup,nfvi,nfvi_lib]},
{registered,[nfvi]},
{applications, [kernel,stdlib]},
{mod, {nfvi_app,[]}},
{start_phases, []}
]}.
