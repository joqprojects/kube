-record(artifact,
	{service_id,
	 vsn,
	 appfile,
	 modules
	}).
	  
-record(artifact_record,
	{service_id,
	 latest_vsn,
	 vsn_list
	}).
