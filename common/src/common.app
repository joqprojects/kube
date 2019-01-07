%% This is the application resource file (.app file) for the 'base'
%% application.
{application, common,
[{description, "common funtions  " },
{vsn, "1.0.0" },
{modules, 
	[addr_mgr,cmn,repo_mgr,dbase_dets,tcp,if_dns,if_log,misc,
	 dns_app,dns_sup,dns,dns_lib
	]},
{registered,[dns]},
{applications, [kernel,stdlib]},
{mod, {dns_app,[]}},
{start_phases, []}
]}.
