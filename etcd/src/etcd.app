%% This is the application resource file (.app file) for the 'base'
%% application.
{application, etcd,
[{description, "key vaulue store for kube  " },
{vsn, "1.0.0" },
{modules, 
	  [etcd_app,etcd_sup,etcd,etcd_lib]},
{registered,[etcd]},
{applications, [kernel,stdlib]},
{mod, {etcd_app,[]}},
{start_phases, []}
]}.
