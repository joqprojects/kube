%% This is the application resource file (.app file) for the 'base'
%% application.
{application, repo,
[{description, "repo  " },
{vsn, "1.0.0" },
{modules, 
	  [if_repo,repo_lib,
	   repo_app,repo_sup,repo]},
{registered,[template]},
{applications, [kernel,stdlib]},
{mod, {repo_app,[]}},
{start_phases, []}
]}.
