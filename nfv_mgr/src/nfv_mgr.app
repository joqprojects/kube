%% This is the application resource file (.app file) for the 'base'
%% application.
{application, nfv_mgr,
[{description, "nfv_mgr  " },
{vsn, "1.0.0" },
{modules, 
	  [nfv_mgr_app,nfv_mgr_sup,nfv_mgr,nfv_mgr_lib]},
{registered,[nfv_mgr]},
{applications, [kernel,stdlib]},
{mod, {nfv_mgr_app,[]}},
{start_phases, []}
]}.
