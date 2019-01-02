%% This is the application resource file (.app file) for the 'base'
%% application.
{application, vim,
[{description, "vim  " },
{vsn, "1.0.0" },
{modules, 
	  [vim_app,vim_sup,vim,vim_lib]},
{registered,[vim]},
{applications, [kernel,stdlib]},
{mod, {vim_app,[]}},
{start_phases, []}
]}.
