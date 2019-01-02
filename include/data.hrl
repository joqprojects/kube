%% service_info
%% Information of the servie

-record (service_info, 
         {
	   status = un_loaded,    % un_loaded, started
	   service_id = "service_id",
	   vsn = "vsn",
	   public={"ip_addr",port},
	   local={"localhost",port}
	 }).
-record (node_info,
	 {
	   status = no_service,  % no_Service, in_service
	   node_id = "node_id",
	   public={"ip_addr",port},
	   local={"localhost",port},
	   capability = [capability],
	   zone="zone",
	   port_range = {first_port_nr,last_port_nr}
	 }).

%% 
-record (cluster,      
	 {
	   all_nodes=[],         % node_info
	   avalible_nodes=[]     % node_info
	 }).

-record (applications ,
	 {
	   status = un_loaded,    % un_loaded, started
	   app_id = "app_id",
	   vsn = "vsn",
	   services=[]            % service_info
	 }).
-record (services ,
	 {
	   service_id="service_id",
	   vsn = "vsn",
	   instances = [{node_info,service_info}] % Status of allcated  
	 }).
-record (josca_spec,
	 {
	   specification = spec,
	   description ="infor string",
	   vsn = "vsn",
	   exported_services = [{"service_id","vsn"}],
	   num_instances = glurk,
	   zone = ["zone"],
	   geo_red = glurk,
	   needed_capabilities = [needed_capabilities],
	   dependencies = ["josca_file"]
	 }).

% josca_file_name convention = service-vsn.josca or application-vsn.josca
% 
% %etcd id: Key = {josca,service_id | app_id,vsn}
%         Value = josca_spec

-record (image,
	 {
	   service_id="service_id",
	   vsn = "vsn",
	   tar_binary = tar_file
	 }).
% name convention = service-vsn.tar
%etcd id: Key = {image,service_id,vsn}
%         Value = tar_file
