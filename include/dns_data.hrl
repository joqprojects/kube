%% service_info
%% Information of the servie

-record (dns_info, 
         {
	   time_stamp="not_initiaded_time_stamp",    % un_loaded, started
	   service_id = "not_initiaded_service_id",
	   vsn = "not_initiaded_vsn",
	   ip_addr="not_initiaded_ip_addr",
	   port="not_initiaded_port"
	 }).
