%% This is the application resource file (.app file) for the 'base'
%% application.
{application, lib,
[{description, "lib funtions  " },
{vsn, "1.0.0" },
{modules, 
	[cmn,repo_cmn,dbase_dets,tcp,if_dns,if_log,
	 lib_app,lib_sup,lib,lib_lib
	]},
{registered,[lib]},
{applications, [kernel,stdlib]},
{mod, {lib_app,[]}},
{start_phases, []}
]}.
