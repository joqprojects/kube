%% This is the application resource file (.app file) for the 'base'
%% application.
{application, template,
[{description, "template  " },
{vsn, "1.0.2" },
{modules, 
	  [if_template,template_lib,
	   template_app,template_sup,template]},
{registered,[template]},
{applications, [kernel,stdlib]},
{mod, {template_app,[]}},
{start_phases, []}
]}.
