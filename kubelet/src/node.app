%% This is the application resource file (.app file) for the 'base'
%% application.
{application, node,
[{description, "node  " },
{vsn, "1.0.0" },
{modules, 
	  [node_app,node_sup,node,node_lib]},
{registered,[node]},
{applications, [kernel,stdlib]},
{mod, {node_app,[]}},
{start_phases, []}
]}.
