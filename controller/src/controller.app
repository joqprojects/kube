%% This is the application resource file (.app file) for the 'base'
%% application.
{application, controller,
[{description, "controller  " },
{vsn, "1.0.0" },
{modules,[josca,
	controller_app,controller_sup,controller,controller_lib]},
{registered,[controller]},
{applications, [kernel,stdlib]},
{mod, {controller_app,[]}},
{start_phases, []}
]}.
