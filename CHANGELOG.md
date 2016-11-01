# Release notes for project onepanel


CHANGELOG
---------

### 3.0.0-rc10

* minor changes and improvements


### 3.0.0-rc9

* VFS-2550 Remove NIF libraries


### 3.0.0-rc8

* VFS-2550 Template runner_wait_process
* VFS-2550 Make add_storages operation idempotent
* VFS-2550 Make register operation idempotent
* VFS-2550 Make add_users operation idempotent


### 3.0.0-rc7

* VFS-2550 Make configure operation idempotent


### 3.0.0-rc6

* VFS-2180 Add 'sync' bucket for oneprovider release
* VFS-2525 Update onepanel_gui ref
* VFS-2525 Fix docs generation
* VFS-2390 Upgrade rebar to version 3


### 3.0.0-rc5

* VFS-2527 Add certs setup step to cluster worker deployment
* VFS-2156 Add repeats to provider registration process
* VFS-2468 Add step attempts in case of failure
* VFS-2468 Make couchbase buckets configurable


### 3.0.0-rc4

* VFS-2156 Add repeats to provider registration process


### 3.0.0-RC3

* VFS-2156 Make '/tasks' endpoint unauthorized
* VFS-2156 Add couchbase server and bucket quota
* VFS-2156 Accept application/x-yaml content type
* VFS-2156 Build package
* VFS-2156 Add onepanel_gui
* Releasing new version 3.0.0-RC2
* VFS-2269 Enable Symmetric Multiprocessing
* VFS-1847 Add documentation
* Releasing new version 3.0.0-RC1
* VFS-1847 Integrate swagger REST API description
* VFS-1847 Add REST requests parser
* VFS-1847 Add NIF libraries
* VFS-1847 Add REST handlers for all services
* VFS-1847 Add onedata_user REST handler
* VFS-1847 Add onezone service
* VFS-1847 Add oneprovider service
* VFS-1847 Add cluster_manager and cluster_worker services
* VFS-1847 Add couchbase service
* VFS-1847 Add onedata_user logic
* VFS-1847 Add db models
* VFS-1847 Add vm config editor.
* VFS-1847 Start onepanel core project.


### 3.0.0-RC2

* VFS-2269 Enable Symmetric Multiprocessing


### 3.0.0-RC1

* minor changes and improvements


### 3.0.0-beta8

* minor changes and improvements


### 3.0.0-beta7

* VFS-2163 Allow user deletion
* Update erlang tls
* VFS-2163 onepanel user REST endpoint
* VFS-2072 Add admin credentials to helpers args
* VFS-2072 Remove user credentials from space support


### 3.0.0-beta4

* VFS-1969 Add border to installation.gif.
* VFS-1969 Update README.


### 3.0.0-beta3

* VFS-1969 Update README.


### 3.0.0-beta2

* VFS-1804 Enable couchbase memory quota configuration.


### 3.0.0-beta1

* VFS-1804 Add success message to batch mode script.
* VFS-1804 Remove logotype footer.
* FS-1804 Add dns config option to batch mode config file.
* VFS-1804 Add redirection point to oneprovider config.
* VFS-1804 Use environmental variables to configure onedata services.
* VFS-1598 Fix provider key/cert names.
* VFS-1598 Fix username change and license display.
* VFS-1598 Fix onepanel admin script.
* VFS-1598 Adjust onepanel to onezone configuration.


### 3.0.1

* VFS-1603 Disable generation of start_clean.boot file in bin directory of a release.
* VFS-1603 Rename gr_panel to oz_panel.
* VFS-1603 Add package_rel_type to pkg.vars.config and use it during package build.
* VFS-1603 Extend package post installation scripts for Global Registry release.
* VFS-1528 Use monotonic time instead of system time
* VFS-1603 Add ONEPANEL_MULTICAST_ADDRESS and ONEPANEL_AUTOSTART environment variables to post package install script.
* VFS-1603 Rename from CCM to CM.
* VFS-1603 Increase RPC call timeout. Add retries to storage configuration process.
* VFS-1603 Update package dependencies.
* VFS-1603 Add distribution check to package rule in Makefile.
* VFS-1528 Remove deprecated use of erlang:now/0


### 3.0.0

* Dependencies management update
* VFS-1472 Adjust storage and spaces management pages to Amazon S3 storage helper.
* VFS-1428 Adjust storage and spaces management pages to multiple storage helpers.
* VFS-1428 Update provider database on space support.
* VFS-1428 Update page storage and space management.
* VFS-1428 Change page storage.
* VFS-1455 Do not sed cookie of installed node, rename oneprovider_node to op_worker.
* VFS-1455 Initialize storage when worker is started.
* VFS-1193 switch to couchbase db
* VFS-1142 Check return code during node startup.
* VFS-1142 Add rpm depends and post install scripts.
* VFS-1134 Rename op-onepanel -> op-panel, gr-onepanel -> gr-panel.
* VFS-1150 Fix installer db multiple installations in row.
* VFS-1150 Add deregistration during deinstallation to onepanel admin script.
* VFS-1150 Templating onepanel admin script.
* VFS-1150 Fix onepanel admin addition to package.
* VFS-1150 Adjust op_onepanel_admin script.
* VFS-1129 Add post install hostname sed and daemon startup.
* VFS-1129 Add build dependency to pkg-config.
* VFS-1129 Add deb build dependencies.
* VFS-1100 Change default riak port
* VFS-1053 add node_package to gr_onepanel
* VFS-937 Showing provider name in GUI
* VFS-937 Push channel management.
* VFS-950 Checking ports during installation.
* VFS-953 Sending space size in create/support requests.
* VFS-937 Saving provider ID in CCM state.
* VFS-937 Connecting/disconnecting to Global Registry during registration/deregistration.
* VFS-915 Add html encoding.
* VFS-915 Add breadcrumbs.
* VFS-915 Add info about NAT/PAT in case of connection error.

### 2.0.0

* Service command may be used after installation
* Registration in globalregistry hidden

### 1.0.0

* provide GUI for oneprovider installation and update.

________

Generated by sr-release. 
