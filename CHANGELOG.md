# Release notes for project onepanel


CHANGELOG
---------

### 18.02.0-beta5

* VFS-4335 Return map when checking cluster IPs
* VFS-3703 Switched from mochiweb JSON parsing to jiffy
* VFS-4067 Fix setting cluster IPs in onezone
* VFS-3745 Seperate Let's Encrypt client from oneprovdier
* VFS-4267 Update images in services test suite
* Update web-client and ctool refs
* VFS-4267 Adjust code to erl 20, update deps
* VFS-4273 - add rest endpoint for invalidating luma cache in provider


### 18.02.0-beta4

* Releasing new version 18.02.0-beta4


### 18.02.0-beta3

* Updating GUI, including: VFS-4259 * VFS-4259 Fixing not working create new cluster button
* Updating GUI, including: VFS-4229 * VFS-4229 Merged recent changes of onedata-gui-common library (ia. improved mobile view, styles improvements)
* VFS-4236 Check if storage exists before creating it
* VFS-4222 Change op_worker function triggering oz connection
* VFS-4222 Don't trigger Let's Encrypt on every provider modification
* VFS-4222 Explicitly check oz connection after registration
* VFS-3745 Generate test web cert only once
* Updating GUI, including: VFS-4027 * VFS-4027 Added support for peta-, exa-, zetta- and yottabytes
* Updating GUI, including: VFS-4206 * VFS-4206 Added capability to display speed in bps (ported)
* VFS-4207 Move provider listener restarting logic to provider
* Updating GUI, including: VFS-4194 * VFS-4197 Added "storage path type" option for storages
* VFS-4067 Add test for GET and PATCH on cluster ips
* Updating GUI, including: VFS-4097 * VFS-4097 Added cluster IPs configuration step and view for changing IPs after deployment
* VFS-4067 Move common function to utils
* VFS-4067 Set default value for Let's Encrypt in batch config
* VFS-4067 Throw on unimplemented delete resource method
* VFS-4067 Mark IPs as configured in batch config
* VFS-4067 Do not set provider IP before registration in onezone
* VFS-4067 Fixes for cluster_ips API and style improvements
* VFS-4067 Allow setting cluster IPs with batch config
* VFS-4067 Implement GET/PATCH of cluster_ips
* VFS-4067 Notify workers after modifying IPs
* VFS-4067 Add OZ endpoint as a method of determining IP
* VFS-4067 Add step for writing external IP to node


### 18.02.0-beta2

* VFS-4178 Fixed generated rest_model
* VFS-4178 Added default storage types when adding storage from REST
* VFS-4036 Added support for storage path type
* VFS-4036 Updated rest model from onepanel-swagger
* disable http2
* VFS-4126 Loosen cert validation during local service status checks via nagios
* VFS-4126 Fix some default values of app.config envs
* Updating GUI, including: VFS-4125 * VFS-4125 Fixed lack of integrity check for CSS files
* VFS-3704 update cowboy to version 2.2.2
* VFS-4120 Get provider configuration fail now causes HTTP 500


### 18.02.0-beta1

* VFS-3978 Do not distribute Onedata Test CA in packages   
* VFS-3978 add untrusted web certificates for localhost as default   
* VFS-3978 autogenerate certs option now always overwrites existing certs   
* VFS-3978 unify paths and env names related to certs with those in op and oz
* VFS-3765 Use provided admin email in provider registration and Let's Encrypt cert procedure
* VFS-4015 Use PBKDF2 rather than bcrypt for user password hashing
* VFS-3622 Allow to use custom commands for starting underlying services and to pass overlay config upon onepanel start
* VFS-3751 Use provider macaroons rather than certificates, don't fetch provider details unless it is registered
* VFS-3790 Automatically generate web certs on startup if not present, use secure connections where possible
* VFS-3635 Distribute OZ CA cert during registration
* Overwrite worker cert paths with onepanel certs
* VFS-3609 Implement ACME protocol (Let's Encrypt) client
* VFS-3614 Modify API to handle subdomain delegation
* VFS-3526 Combine provider urls and redirection_point
* VFS-3606 GUI: Subdomain delegation functionality for provider
* VFS-3968 GUI: Update to EmberJS 2.18.0
* VFS-3985 GUI: Added a bar chart, that shows storage usage for a space
* VFS-4016 GUI: Added NullDevice storage support
* VFS-3986 GUI: Added Let’s Encrypt certificate setup step in provider cluster deployment; improved error backend error descriptions
* VFS-3955 GUI: Better truncating of too long names in sidebar; internal libs update
* VFS-3619 GUI: Refactor of login page components
* VFS-3205 GUI: Improvements in displaying deployment steps
* VFS-3636 GUI: Fix for invalid date in synchronization statistics charts
* VFS-3202 GUI: Try to detect unfinished cluster deployment on page refresh
* VFS-3870 GUI: Show notify after space support settings change
* VFS-3706 GUI: Do not allow to enter improper routes in panel (fixes also VFS-3895: PATCH request after provider deregistration)
* VFS-3928 GUI: Less restrictive validation of provider/onezone domain name
* VFS-3677 GUI: Fix for tooltip positioning in mobile view
* VFS-3592 GUI: Added common favicon
* VFS-3883 GUI: Porting recent improvements in common components and utils from op-gui-default
* VFS-3741 GUI: Fix for import chart tooltip positioning and overflow handling in mobile view
* VFS-3677 GUI: Fix for tooltip positioning in mobile view
* VFS-3741 GUI: Fix for import chart tooltip positioning and overflow handling in mobile view
* VFS-3882 GUI: Fixed space auto-cleaning report status tooltips; deregister provider message update


### 17.06.2

* Releasing new version 17.06.2


### 17.06.1

* Releasing new version 17.06.1


### 17.06.0-rc9

* VFS-4004 Update ctool to include safe ciphers in TLS
* VFS-3972 Fix attach-direct consoles in releases not being run with xterm terminal
* VFS-3911 - adapt onepanel to changes in op-worker storage_sync_monitoring module
* fix error when geolocation is integer


### 17.06.0-rc8

* Releasing new version 17.06.0-rc8


### 17.06.0-rc7

* VFS-3815 Added erlang-observer as RPM build dependency
* VFS-3686 allow to start space cleaning manually, update ctool
* Updating GUI, including: VFS-3685, VFS-3661 - VFS-3685 Added space files popularity and space auto cleaning views - VFS-3661 Improvements in presenting loading state of views and errors
* Updating GUI, including: VFS-3710 - VFS-3710 Using binary prefixes for size units (IEC format: MiB, GiB, TiB, etc.)
* Updating GUI, including: VFS-3737, VFS-3639 - VFS-3737 Change Bower server to registry.bower.io - VFS-3639 Fix and restore loaders in new GUIs
* Updating GUI, including: VFS-3608 - VFS-3608 Table component for showing details about support size
* VFS-3686 configure autocleaning in onepanel


### 17.06.0-rc6

* Releasing new version 17.06.0-rc6


### 17.06.0-rc5

* fix fetching luma_config


### 17.06.0-rc4

* Releasing new version 17.06.0-rc4


### 17.06.0-rc3

* VFS-3639 Updating GUI ref
* VFS-3449 add syncAcl flag to storage import and storage update configuration


### 17.06.0-rc2

* VFS-3506 Fix space support with missing storage ID


### 17.06.0-rc1

* VFS-3448 Use single 'onedata' bucket
* VFS-3384 add translation of luma configuration error
* VFS-3384 setup luma when adding storage
* VFS-3417 Increase default RAM quota of couchbase buckets


### 17.06.0-beta6

* VFS-3417 Change listeners restart strategy
* VFS-3356 Extend REST GET space/storage responses
* VFS-3289 - change according to change in op_worker storage_sync_monitoring API
* VFS-3289 - storage_sync metrics API


### 17.06.0-beta4

* VFS-3362 Update web-client


### 17.06.0-beta3

* Releasing new version 17.06.0-beta3


### 17.06.0-beta2

* VFS-3345 Updating GUI ref (development) - added missing data-options in cluster-host-table-row (for testing purposes) - show glusterfs in storage type select dropdown without scroll
* VFS-3280 Remove etls.
* VFS-3250 Added GlusterFS support


### 3.0.0-rc16

* VFS-3216 Update REST API
* VFS-3207 Using new GUI ref
* VFS-3216 Make GET method for onepanel hosts endpoint auth
* VFS-3216 Use only storage ID in space support request
* HOTFIX added gui-static dependency on package target in Makefile
* VFS-3186 Change provider deregistration behaviour
* VFS-3186 Update provider name after modify
* VFS-3186 Return deployment name with cluster configuration
* VFS-3163 Add session REST endpoint
* VFS-3165 Add current password to user modify request
* VFS-3118 Change default env value for custom gui root
* VFS-3118 Add gui override and livereload to panel.up
* VFS-3118 Add gui repo, serve static files using default gui logic, add templates for session and router plugins
* VFS-3118 Serve index.html on root path
* VFS-3118 Remove onepanel_gui and serve static files


### 3.0.0-rc15

* VFS-3233 Add support for sig v2 to AWS S3 helper
* VFS-3213 Rename storage verification error codes
* VFS-3213 Create new test files while verifying storage availability
* VFS-3188 Remove space-storage mapping on space support revoke


### 3.0.0-rc14

* Releasing new version 3.0.0-rc14


### 3.0.0-rc13

* VFS-3117 Allow integer for provider geo long/lat
* VFS-3086 Add cookie authentication


### 3.0.0-rc12

* VFS-2907 Add mount in root and readonly options
* VFS-3073 Set worker_num for deployment
* Add service watcher
* VFS-2931 Reduce number of kept rotated log files
* VFS-2910 Adjust code to LUMA refactoring
* VFS-2620 Enable storage helper args update
* VFS-2802 Add nagios proxy


### 3.0.0-rc11

* VFS-2733 Standarize app listeners


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
