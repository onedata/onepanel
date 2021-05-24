Release notes for project onepanel
==================================

CHANGELOG
---------

### 21.02.0-alpha9

### 21.02.0-alpha8

-   **VFS-7510** Add API for browsing dataset structures, separately for
    datasets in attached and detached state. The datasets can be listed
    using batches of requested size, with desired starting point and
    offset.

### 21.02.0-alpha7

### 21.02.0-alpha6

### 21.02.0-alpha5

-   **VFS-7294** Added publicly available REST endpoints for fetching
    information and data of shared files/directories. The data-related
    endpoints are offered by Onezone, which redirects to a suitable
    Oneprovider so that a guest user does not need any knowledge of the
    environment to access the data. Improved the Web GUI\'s shares view
    to present the public endpoints in an easy-to-use manner.

### 21.02.0-alpha4

### 21.02.0-alpha3

-   **VFS-6638** Fixed handling duplicate clusters states on clusters
    list in GUI.

### 21.02.0-alpha2

-   **VFS-7280** Fixed page reload after Let\'s Encrypt certificate
    generation via Onepanel GUI.
-   **VFS-7165** Add a workaround for Erlang\'s SSL implementation that
    would not reload server certificate chain when it is changed (e.g.
    after Let\'s Encrypt certificate regeneration).
-   **VFS-6566** Improved UX and fixed minor issues in share views.
    Fixed inability to open share hosted by Oneprovider 19.02.x using
    Onezone 20.02.x.

### 20.02.9

### 20.02.8

### 20.02.7

-   **VFS-7294** Added publicly available REST endpoints for fetching
    information and data of shared files/directories. The data-related
    endpoints are offered by Onezone, which redirects to a suitable
    Oneprovider so that a guest user does not need any knowledge of the
    environment to access the data. Improved the Web GUI's shares view
    to present the public endpoints in an easy-to-use manner.
-   **VFS-7280** Fixed page reload after Let's Encrypt certificate
    generation via Onepanel GUI.
-   **VFS-7165** Add a workaround for Erlang's SSL implementation that
    would not reload server certificate chain when it is changed (e.g.
    after Let's Encrypt certificate regeneration).
-   **VFS-6638** Fixed handling duplicate clusters states on clusters
    list in GUI.
-   **VFS-6566** Improved UX and fixed minor issues in share views.
    Fixed inability to open share hosted by Oneprovider 19.02.x using
    Onezone 20.02.x.

### 20.02.6

-   **VFS-6802** Added visual QoS expression editor with live matching
    storages evaluation.

### 20.02.5

-   **VFS-7124** Fixed adding user mapping to LUMA local feed on POSIX
    incompatible storages.
-   **VFS-6999** Improve error reporting in entrypoints of
    oneprovider/onezone dockers, always dump application logs to stdout
    in case of failures during batch deployment.
-   **VFS-6858** Added support for cancelling storage auto-cleaning run
    using GUI.
-   **VFS-6745** Added new view with token templates in tokens creator
    GUI.

### 20.02.4

-   **VFS-7003** Added new counters presenting progress of current/last
    finished scan of the storage import mechanism - number of all
    processed files (\`Processed files\`) and the total number of files
    residing on the storage during the scan (\`Total storage files\`).
-   **VFS-6841** Introduce unified time management in all Onedata
    components - all clusters now regularly synchronize their clocks
    with the Onezone service, the process is managed by Onepanel's
    master node.


### 20.02.3

### 20.02.2

-   **VFS-6853** Matching session cookie is now required to verify a GUI
    access tokens (they are used behind the scenes by the Onedata web
    applications), which increases security.
-   **VFS-6732** New JSON and RDF metadata editor based on Ace Editor.
-   **VFS-6338** Enhanced API of the mechanism for importing existing
    data into Onedata spaces without need for copying the data. The
    mechanism is now called "storage import". Introduced modes of
    storage import: "manual" which allows for manual registration of
    files and "auto" which enables automatic detection and import of
    files from the storage. Introduced possibility to forcefully
    start/stop scans of auto storage import. Redesigned GUI related to
    storage import, adjusted to the new features.


### 20.02.1

-   **VFS-6568** Introduced concept of readonly storage. If enabled,
    Oneprovider will block any operation that writes, modifies or
    deletes data on the storage. Such storage can only be used to import
    data into the space. Mandatory to ensure proper behaviour if the
    backend storage is actually configured as readonly.
-   **VFS-6535** Updated S3 SDK library to 1.8.7.
-   **VFS-6504** Added HTTP storage helper allowing registration of HTTP
    and HTTPS servers as storage sources for Onedata Spaces.
-   **VFS-6474** Added initial support for XRootD storage, including
    direct access to XRootD storages and importing of legacy data sets
    stored on XRootD or EOS servers.
-   **VFS-6401** All authentication errors are now wrapped in
    UNAUTHORIZED error and map to 401 HTTP code to avoid ambiguity when
    reporting token related errors - tokens can be used for
    authentication as well as input data for some operations (e.g.
    invite tokens).
-   **VFS-6378** Onepanel GUI and REST API now explicitly block
    supporting a space with more than one imported storage (globally) -
    such operation was possible in the past but was never supported by
    the internal storage import logic and led to incoherent view on
    space data.
-   **VFS-6346** GUI improvements: added Oneprovider GUI notifications,
    better file selection, additional error handling, better file
    manager refresh UX, fixed overflow of context menu in file browser,
    fixes in responsive layout.
-   **VFS-6344** GUI: showing information if QoS requirement is
    impossible to be fulfilled.
-   **VFS-6343** Added delete account feature in GUI.
-   **VFS-6184** Added the space owner concept. Space owner works like
    \"root\" within the space - such user is allowed to perform all
    file/API operations, regardless of the assigned privileges and file
    permissions / ACLs. Ownership can be assigned to any number of
    users, and it is forbidden to leave a space without an owner -
    ownership must be transferred first.
-   **VFS-5648** Extended QoS expression to allow comparators (\<, \>,
    \<=, \>=) and numeric values. Changed \"-\" operator to \"\\\".
    Space characters (\" \"), dashes (\"-\") and underscores (\"\_\")
    are now allowed in QoS parameters. Added more details to invalid QoS
    expression errors.
-   **VFS-4760** Added implicit API caveats that limit access tokens
    used by Onedata GUIs behind the scenes for authentication and
    authorization. Different services in the system are presented with
    user\'s access token with power limited to bare minimum required for
    the service to handle user requests. For example, Oneproviders do
    not have access to APIs that could alter or delete user data and
    memberships.

### 20.02.0-beta4

### 20.02.0-beta3

-   VFS-6105 Introduced node invite tokens. They can be generated by
    cluster members for use by other nodes to join this cluster. Such
    tokens expire after configurable period but within time limit can be
    used any number of times.

-   VFS-4777 Introduced Ceph cluster deployment. Onepanel can
    orchestrate Ceph deployment among Onedata nodes and use the cluster
    as Oneprovider\'s storage backend.

-   VFS-4777 REST endpoint getTaskStatus now includes total number of
    steps to be executed.

-   VFS-5841 The Onepanel REST API now uses the same error classes as
    other Onedata services. Each error has a distinct id and may have
    well-structured detailed information.

-   VFS-5819 Changed `mount_in_root` to be a storage parameter and
    renamed it to `import_existing_data`. Storage with this option
    selected can support only one space and has data import enabled.

-   VFS-5901 Application config can now be customized with arbitrary
    number of config files added to config.d directory in /etc.

-   VFS-5838 Significant internal logic refactor. Includes minor API
    changes:

    -   Endpoints creating a long-running task return a JSON body with
        the task id in addition to the Location header and use HTTP code
        202 Accepted.
    -   POST requests \'support\_space\' and
        \'add\_onezone\_user\' now use the 201 Created HTTP code,
        return the id of created resource in the response body, and
        provider the Location header.
    -   PATCH request modifying space support no longer returns the
        space id in the response body.
    -   All endpoints may return 401 Unauthorized if conditions for
        credential-less access are not met.

-   VFS-4777 Creation of storage type `ceph` is no longer possible. Use
    `cephrados` instead. Existing storages of this type will continue to
    work.

-   VFS-5899 GUI update \* New tokens gui

-   VFS-6158 Fix return value in ceph\_cli:stop\_with\_timeout

-   VFS-6158 Copy policies as part of the autogenerated config

-   VFS-6158 Fix onepanel\_env:import\_generated\_from\_node

-   VFS-6158 Add ability to add oz\_worker nodes

-   VFS-6158 Use snake\_case atoms for zone policies

-   VFS-6158 Move generic parts of add\_nodes to
    service\_cluster\_worker

-   VFS-6076 Remove unused macros

-   VFS-6154 Add REST field listing all cluster nodes

-   VFS-5682 Allow changing fileMode and dirMode in webdav storage
    update

-   VFS-6076 Improve storage creation error reporting

-   VFS-6076 Do not restart couchbase when adding op worker node

-   VFS-6076 Update status cache immediately after oneprovider restart

-   VFS-6076 Make sure healthchceck restarts only the local service
    instance

-   VFS-6076 Correctly configure transfers mock on new op worker node

-   VFS-6076 Fix action resetting node after changing to JIT host
    resolving

-   VFS-6076 Better action error reporting

-   VFS-6076 Forbid deploy requests for hosts already having a service

-   VFS-6076 Simplify a get\_steps function

-   VFS-6076 Rely on JIT host resolving when adding op worker

-   VFS-6076 Improve readability

-   VFS-6076 Use ctool\'s map utils

-   VFS-6076 Resolve step hosts just before execution

-   VFS-6076 Implement op\_worker:add\_nodes

-   VFS-6104 Fix request validation logic

-   VFS-6104 Documentation improvements

-   VFS-6104 Enforce presence of zone domain and name during deployment

-   VFS-6104 Define type spec for \#service.ctx for Ceph services

-   VFS-6104 Define \#service.ctx typespecs for Onedata services

-   VFS-6104 Describe possible keys in oneprovider \#service.ctx

-   VFS-6076 Inform op\_worker about onezone domain after registration

-   VFS-6076 Do not require onezone domain to be stored before
    registration request

-   VFS-6076 Separate ctx type for steps and for \#service

-   VFS-6076 Remove unused task\_delay step ctx field

-   VFS-6076 Store onezone domain in service oneprovider ctx

-   VFS-6076 Do not expect erlang cookie to come in step ctx

-   VFS-6076 Improve naming consistency in service\_cluster\_worker
    module

-   VFS-6076 Always use autogenerated config file in onepanel\_env:write

-   VFS-6076 Handle undeployed service in GET all\_host\_status

-   VFS-6076 More explicit typing for onepanel\_utils:convert/2

-   VFS-6076 Use maps for http\_client headers

-   VFS-6076 Do not treat config variables as defaults for request
    values

-   VFS-6110 Update service status cache after wait\_for\_init

-   VFS-6110 Filter hosts used in service healthcheck

-   VFS-6110 Add overview of the service actions mechanism

-   VFS-6110 Use records for action execution history everywhere

-   VFS-6110 Define service\_executor messages as records

-   VFS-6108 Bump version to 20.02.0-beta1

-   VFS-6108 Fix access token verification procedure to use
    onepanel\'s serviceToken

-   VFS-6108 Update ctool ref, adjust to access and identity tokens
    being two separate types

-   VFS-5988 GUI update \* Added shares sidebar entry

-   VFS-6061 Add x-onedata-service-token header to requests to Onezone
    to ensure compatibility with Onezone v. 20.02.\*

-   VFS-6061 Add op-panel service to token verification context

-   VFS-6109 GUI update \* Added trimming to token inputs

-   VFS-6061 Update ctool ref, remove onepanel\_maps module

-   VFS-6061 Update ctool ref, adjust to changes in tokens (reworked
    cv\_audience and cv\_authorization\_none caveats)

-   Update onedata-documentation ref

-   VFS-6075 Ensure only one Let\'s Encrypt cron job

-   VFS-6075 Improve docs and type specs

-   VFS-6075 Describe cluster extension flow

-   VFS-6075 Rename functions in onepanel\_env

-   VFS-6075 Copy autogenerated config variables when adding cluster
    node

-   VFS-6075 Allow using single node in onepanel\_rpc

-   VFS-6075 Add better comment to service\_letsencrypt

-   VFS-6075 Remove unused function

-   VFS-6075 Use erlang:raise/3 to rethrow errors

-   VFS-6075 Copy certificates to new cluster nodes

-   VFS-6075 Remove unused export from letsencrypt\_api

-   Adjust token creation to changes in ctool

-   VFS-6006 Update onedata-documentation ref to include the newest
    compatibility table

-   VFS-5830 reformat code, update rest\_model.erl

-   VFS-5830 add endpoint for cancelling auto-cleaning run in given
    space

-   VFS-6056 GUI update Added resetting navigation state on logout

-   VFS-6041 GUI update Added info about deprecation of Ceph storage

-   VFS-6035 Add VM option that forbids terminating the node with Ctrl +
    C

### 19.02.5

### 19.02.4

### 19.02.3

-   Releasing new version 19.02.3

### 19.02.2

-   VFS-6299 GUI update \* Fixed page reload after certificate
    generation
-   VFS-6041 GUI update Added info about deprecation of Ceph storage
-   VFS-6035 Add VM option that forbids terminating the node with Ctrl +
    C

### 19.02.1

-   VFS-5994 Make \'production\' Let\'s Encrypt mode the default
-   VFS-5940 Rename oz-worker\'s GUI package verification envs to more
    intuitive

### 19.02.0-rc2

-   VFS-5708 Implement gui message management
-   VFS-1891 GUI update Added setting privacy policy, cookie consent
    notification and sign-in notification
-   VFS-5755 Retry erlang pings when joining a cluster

### 19.02.0-rc1

-   VFS-5499 GUI update Fixed displaying errors, when oz/op-worker
    service is not working
-   VFS-5500 Use 503 code when action fails because of unavailable
    Onezone
-   VFS-5687 Added fallback when rpc fails during service compatibility
    check
-   VFS-5500 Use op\_worker\'s graph sync to fetch space details
-   VFS-5500 Persist provider details
-   VFS-5400 Use compatibility reference json in op
-   VFS-5635 Ensure Onezone is configured with lowercase domain
-   VFS-5635 Ensure Oneprovider is configured with lowercase domain
-   VFS-5658 Use `service restart` to repeat couchbase startup
-   VFS-5658 Log when service status exits with non-zero code
-   VFS-5658 Handle unicode in error logs
-   VFS-5658 Fix logging of service restart errors
-   VFS-5658 Restart couchbase after failed wait for init
-   VFS-5657 Enabled Ubuntu distribution package tag
-   VFS-5498 Do not verify oz\_panel GUI package
-   VFS-5598 Allow set\_remote of policies to fail
-   VFS-5598 Configure zone policies before starting the worker
-   VFS-5508 Rename debugMode field in GUI context to browserDebugLogs
-   VFS-5629 Enable unknown type warnings in dialyzer
-   VFS-5597 Added s3 sync helper params
-   VFS-4698 Added test\_image endpoint
-   VFS-5619 Fix incorrect usage of onepanel\_lists:typed\_find
-   VFS-5598 Enforce enum values in rest\_model
-   VFS-5598 Add provider registartion policy switch
-   VFS-5598 Add endpoint for toggling rtransfer mock
-   VFS-5107 Returns credentialsType in webdav details
-   VFS-5545 Verify storage verification passed before creating storage
-   VFS-4473 Rework letsencypt\_api module to use ACMEv2 protocol
-   VFS-4992 Make onepanel\_cron run job for the first time after delay
-   VFS-5107 Update rest models from swagger
-   VFS-5107 Use storage test errors returned by op-worker
-   VFS-5107 Adjust to changes in op\_worker RPC API
-   VFS-5107 Rely on op\_worker to transform helper args
-   VFS-5107 Implement storage modification

### 18.02.3

-   Releasing new version 18.02.3

### 18.02.2

-   Fix is\_registered check
-   increase webdav connectionPoolSize

### 18.02.1

-   VSF-5198 Renew Let\'s Encrypt certs 30 days before expiration
-   Updating GUI, including: VFS-5187-spaces-support-fixes \* VFS-5187
    Fixed some graphical issues in supported spaces view; onepanel
    client library update
-   VFS-5121 hotfix, enabled field should be optional in
    space\_file\_popularity\_configuration
-   Updating GUI, including: VFS-5114-dynamic-auto-cleaning-reports \*
    VFS-5114 Infinite-scroll list of auto-cleaning reports,
    file-popularity options and major refactor of space support views
-   VFS-5121 fix missing field in space\_sync\_stats\_model generated
    from swagger
-   VFS-5161 Add isRegistered and zoneName to configuration endpoint
-   VFS-5161 Harden configuration endpoint against workers being down
-   VFS-5161 Fix detection whether oneprovider is registered
-   VFS-5161 Use polymorphism to describe configuration endpoint
-   VFS-5161 Add configuration endpoint to swagger-described path
-   Updating GUI, including: VFS-5153-dns-autodetect-option \* VFS-5153
    Added DNS autodetect option
-   VFS-5146 Handle \'hostname -i\' returning multiple addresses
-   VFS-5146 Add builtInDnsServer switch to batch config
-   VFS-5146 Allow use of default DNS for NS records check
-   VFS-5146 Store oz domain before using it for IP detection
-   VFS-5146 Use DNS for external IP detection
-   VFS-5146 Allow empty list of DNS check servers
-   VFS-5121 change description of a selective rule
-   VFS-5159 Add boost lib to RPM dependencies
-   VFS-5159 Add missing folly lib to package deps and update ctool
-   VFS-5121 changes files-popularity name to file-popularity
-   VFS-5121 update file-popularity API
-   VFS-5121 add defaults to infinite-scroll over auto-cleaning reports
    request params
-   VFS-5023 implement backend for infinite scroll over auto-cleaning
    reports
-   VFS-5023 handle errors returned by autocleaning\_api:force\_start
    function
-   VFS-5023 update auto\_cleaning rule names in communication with
    oneprovider
-   VFS-5023 handle cases when file-popularity and autocleaning
    mechanisms are disabled when forcing start of auto-cleaning
-   VFS-5023 update rpc calls to oneprovider, add end\_per\_suite
    functions
-   Updating GUI, including: VFS-5070-auto-cleaning-new-options \*
    VFS-5070 Added new conditions and using refactored API for
    auto-cleaning
-   VFS-5010 Make user session longer
-   VFS-5010 Fill in default values for webdav storage params
-   VFS-5044 Convert storage details on GET
-   VFS-5044 Use valid module name for checking TXT record

### 18.02.0-rc13

-   Updating GUI, including: VFS-4919-webdav-storage,
    VFS-4873-unify-onedata-addons *VFS-4919 Added support for WebDAV
    storage* VFS-4873 Refactoring of common addons
-   VFS-4902 Added WebDAV helper storage
-   VFS-4902 Updated builder and worker images
-   VFS-4902 Updated rest model with WebDAV storage definition
-   VFS-4952 Use graph sync to modify provider data
-   Updating GUI, including:
    VFS-4870-representation-of-effective-groups \* VFS-4870 New style
    for modals
-   Updating GUI, including: VFS-4798-refresh-provider-name \* VFS-4798
    Fixed redirect modal not shown after provider domain change and
    provider data refresh after cluster aspect change
-   VFS-4936 Use graph sync for adding supported spaces
-   Updating GUI, including: VFS-4865-space-resize \* VFS-4865 Added
    space support size change feature
-   VFS-4936 Use common errors API for space support change
-   VFS-4936 Check if space exists when handling REST call
-   VFS-4936 Move storages endpoint handling to rest\_oneprovider
-   VFS-4936 Add space support resizing
-   VFS-4707 remove soft quota limit

### 18.02.0-rc12

-   Releasing new version 18.02.0-rc12

### 18.02.0-rc11

-   VFS-4623 Adjust to change in dns config structure in oz worker
-   VFS-4029 Better certificate hostname verification

### 18.02.0-rc10

-   VFS-4724 Use cephrados config when setting up storage
-   VFS-4667 Created configuration endpoint
-   Updating GUI, including: VFS-4663-onezone-web-cert \* VFS-4663 Web
    certificate management
-   VFS-4029 Update API description from swagger
-   VFS-4029 Support http Let\'s Encrypt challenge in OZ and OP
-   Updating GUI, including: VFS-4677-cephrados \* VFS-4677 Added Ceph
    RADOS storage support
-   Updating GUI, including: VFS-4463-show-storage-id,
    VFS-4233-use-submodules *VFS-4463 Showing storage ID on storages
    view* VFS-4233 Using submodules for own libs on separate repos
-   VFS-4656 Added cephrados helper
-   Updating GUI, including: VFS-4629-fix-translations \* VFS-4463
    Showing storage ID on storages view
-   VFS-4634 Import missing API config to make domainName optional
-   VFS-4529 Display log about correct kernel settings for rtransfer
-   VFS-4631 Introduce artificial delay in couchbase startup
-   VFS-4631 Remove password from couchbase CLI logs
-   VFS-4634 Allow empty domainName in cluster configuration
-   VFS-4203 Remove implicit space creation on support
-   VFS-4474 Print verbose logs on steps error
-   VFS-4474 Throw clear error on onepanel\_env:get failure
-   VFS-4474 Do not write command output to cmd.log
-   VFS-4474 Create error type for shell failures
-   VFS-4474 Log output of failed shell commands
-   VFS-4474 Detect calling non-onepanel nodes with onepanel\_rpc
-   VFS-4491 Allow custom command env variable to be missing
-   VFS-4491 Added entries for start/stop/status command in app config

### 18.02.0-rc9

-   VFS-4532 Include command output in error reason on service start
    failure
-   VFS-4532 Use separate variable for oz\_worker init delay
-   VFS-4532 Use migrated app config values in runtime
-   Updating GUI, including: VFS-4559-fix-actions-display-in-onepanel \*
    VFS-4559 Fixed global actions display in mobile view
-   VFS-4532 Update node\_package vars for all platforms
-   VFS-4532 Use separate config files for generated config
-   Updating GUI, including: VFS-4587 \* VFS-4587 Fixing lack of space
    occupancy bar by updating onepanel client
-   VFs-4586 Updated rest model with space occupancy field from swagger
-   VFS-4560 Detect existing Let\'s Encrypt certificates
-   VFS-4570 Fix endpoints for leaving cluster and querying nagios
-   VFS-4367 Add rest callback accept\_possible
-   VFS-4367 Refactor oneprovider rest to use batch argument copying
-   Updating GUI, including: VFS-4424 \* VFS-4424 Improved sidebar
    content presentation and animation
-   VFS-4504 Set min and max port for distributed erlang

### 18.02.0-rc8

-   Releasing new version 18.02.0-rc8

### 18.02.0-rc7

-   VFS-4474 Describe recovery from the subdomain not available error

### 18.02.0-rc6

-   Releasing new version 18.02.0-rc6

### 18.02.0-rc5

-   VFS-3953 Add default config values for REST listener
-   VFS-3953 Integrate new GUI static backend

### 18.02.0-rc4

-   VFS-4278 Rename singleton Key to ID in onepanel\_deployment
-   VFS-4278 Return code 503 on nagios endpoint for stopped service
-   VFS-4278 Properly invoke service action to reload webcert
-   VFS-4278 Add \\\"resume\\\" steps used for getting up existing
    cluster
-   VFS-4278 Add wait for onepanel workers to start
-   VFS-4278 Migrate to onepanel\_deployment model
-   VFS-4278 Change PUT to POST in test names to match implementaton
-   VFS-4278 Rename onepanel\_milestones to deployment marks
-   VFS-4278 Change \\\"ready\\\" to \\\"configured\\\" in GET
    /configuration
-   VFS-4278 Create steps for managing cluster restart
-   VFS-4278 Block repeated POSTS on /configuration
-   VFS-4278 Start configured cluster services on startup
-   VFS-4278 Wait indefinitely for other mnesia nodes
-   VFS-4278 Create model for storing configuration milestones
-   VFS-4278 Add field \'master\' to REST configuration details
-   VFS-4278 Introduce Master node
-   VFS-4278 Introduce field \\\"ready\\\" in cluster configuration GET
-   VFS-4278 Endpoint for starting oneprovider service
-   VFS-4370 Add information about finished deployment in REST
-   Added rebar profiles for oz\_rel, op\_rel and default\_rel

### 18.02.0-rc3

-   VFS-4449 Restart rtransfer on certificates change
-   VFS-3953 Update lager version and log formatter

### 18.02.0-rc2

-   VFS-4446 Updated dockers config
-   VFS-4446 Updated jiffy ref
-   VFS-4443 Fixed generation of source archive with submodules
-   Updating GUI, including: VFS-4381, VFS-4380, VFS-4356 *VFS-4381
    Removed signature version option from S3 storage form, because we
    support only V4* VFS-4380 Added new parameters in null device form:
    simulated filesystem paramers and grow speed \* VFS-4356
    Improvements in navigation related to incorrect URLs
-   VFS-4295 Changed subtrees to submodules
-   Updating GUI, including: VFS-4241 \* VFS-4241 Polling for
    synchronization statistics with higher frequency.
-   VFS-4241 update ctool, load iso8061 module, update fetching sync
    metrics from op

### 18.02.0-rc1

-   VFS-4374 Increase inactivity timeout in rest listener
-   VFS-4374 Make LE dns servers configurable and improve logs
-   VFS-4374 Remove LE account on failure and txt record on success
-   VFS-4374 Check if cert file is writtable before staging LE run
-   VFS-4374 Retry checking TXT record at onezone
-   VFs-4374 Ensure LE TXT presence by querying global DNS servers
-   VFS-4374 Log verbose error on cert renewal chack failure
-   VFS-4374 Prevent too eager disabling of Let\'s Encrypt
-   VFS-4374 Add verification of set txt record in letsencrypt
-   VFS-4374 Delete Let\'s Encrypt account on error
-   VFS-4374 Increase LE retries and decrease dns TTL to improve
    stability
-   VFS-4374 Add option to retry the ensure\_webcert step
-   VFS-2021 Added dockers.config
-   VFS-4280 Added simulated filesystem options to null device helper
-   VFS-4280 Updated null helper rest model
-   VFS-4368 Remove unused Context argument in letsencrypt service
-   VFS-4368 Fix node IPs being set on nodes without op-worker
-   VFS-4368 Fix webcert being reset only on one node

### 18.02.0-beta6

-   VFS-4357 Register letsencrypt in service watcher after restart
-   VFS-4357 Add default values for letsencrypt app config
-   VFS-4357 Set onepanel hosts even on registered provider
-   Updating GUI, including: VFS-4210 \* VFS-4210 New layout for main
    menu in desktop view mode

### 18.02.0-beta5

-   VFS-4335 Return map when checking cluster IPs
-   VFS-3703 Switched from mochiweb JSON parsing to jiffy
-   VFS-4067 Fix setting cluster IPs in onezone
-   VFS-3745 Seperate Let\'s Encrypt client from oneprovdier
-   VFS-4267 Update images in services test suite
-   Update web-client and ctool refs
-   VFS-4267 Adjust code to erl 20, update deps
-   VFS-4273 - add rest endpoint for invalidating luma cache in provider

### 18.02.0-beta4

-   Releasing new version 18.02.0-beta4

### 18.02.0-beta3

-   Updating GUI, including: VFS-4259 \* VFS-4259 Fixing not working
    create new cluster button
-   Updating GUI, including: VFS-4229 \* VFS-4229 Merged recent changes
    of onedata-gui-common library (ia. improved mobile view, styles
    improvements)
-   VFS-4236 Check if storage exists before creating it
-   VFS-4222 Change op\_worker function triggering oz connection
-   VFS-4222 Don\'t trigger Let\'s Encrypt on every provider
    modification
-   VFS-4222 Explicitly check oz connection after registration
-   VFS-3745 Generate test web cert only once
-   Updating GUI, including: VFS-4027 \* VFS-4027 Added support for
    peta-, exa-, zetta- and yottabytes
-   Updating GUI, including: VFS-4206 \* VFS-4206 Added capability to
    display speed in bps (ported)
-   VFS-4207 Move provider listener restarting logic to provider
-   Updating GUI, including: VFS-4194 \* VFS-4197 Added \\\"storage path
    type\\\" option for storages
-   VFS-4067 Add test for GET and PATCH on cluster ips
-   Updating GUI, including: VFS-4097 \* VFS-4097 Added cluster IPs
    configuration step and view for changing IPs after deployment
-   VFS-4067 Move common function to utils
-   VFS-4067 Set default value for Let\'s Encrypt in batch config
-   VFS-4067 Throw on unimplemented delete resource method
-   VFS-4067 Mark IPs as configured in batch config
-   VFS-4067 Do not set provider IP before registration in onezone
-   VFS-4067 Fixes for cluster\_ips API and style improvements
-   VFS-4067 Allow setting cluster IPs with batch config
-   VFS-4067 Implement GET/PATCH of cluster\_ips
-   VFS-4067 Notify workers after modifying IPs
-   VFS-4067 Add OZ endpoint as a method of determining IP
-   VFS-4067 Add step for writing external IP to node

### 18.02.0-beta2

-   VFS-4178 Fixed generated rest\_model
-   VFS-4178 Added default storage types when adding storage from REST
-   VFS-4036 Added support for storage path type
-   VFS-4036 Updated rest model from onepanel-swagger
-   disable http2
-   VFS-4126 Loosen cert validation during local service status checks
    via nagios
-   VFS-4126 Fix some default values of app.config envs
-   Updating GUI, including: VFS-4125 \* VFS-4125 Fixed lack of
    integrity check for CSS files
-   VFS-3704 update cowboy to version 2.2.2
-   VFS-4120 Get provider configuration fail now causes HTTP 500

### 18.02.0-beta1

-   VFS-3978 Do not distribute Onedata Test CA in packages
-   VFS-3978 add untrusted web certificates for localhost as default
-   VFS-3978 autogenerate certs option now always overwrites existing
    certs
-   VFS-3978 unify paths and env names related to certs with those in op
    and oz
-   VFS-3765 Use provided admin email in provider registration and
    Let\'s Encrypt cert procedure
-   VFS-4015 Use PBKDF2 rather than bcrypt for user password hashing
-   VFS-3622 Allow to use custom commands for starting underlying
    services and to pass overlay config upon onepanel start
-   VFS-3751 Use provider macaroons rather than certificates, don\'t
    fetch provider details unless it is registered
-   VFS-3790 Automatically generate web certs on startup if not present,
    use secure connections where possible
-   VFS-3635 Distribute OZ CA cert during registration
-   Overwrite worker cert paths with onepanel certs
-   VFS-3609 Implement ACME protocol (Let\'s Encrypt) client
-   VFS-3614 Modify API to handle subdomain delegation
-   VFS-3526 Combine provider urls and redirection\_point
-   VFS-3606 GUI: Subdomain delegation functionality for provider
-   VFS-3968 GUI: Update to EmberJS 2.18.0
-   VFS-3985 GUI: Added a bar chart, that shows storage usage for a
    space
-   VFS-4016 GUI: Added NullDevice storage support
-   VFS-3986 GUI: Added Let\'s Encrypt certificate setup step in
    provider cluster deployment; improved error backend error
    descriptions
-   VFS-3955 GUI: Better truncating of too long names in sidebar;
    internal libs update
-   VFS-3619 GUI: Refactor of login page components
-   VFS-3205 GUI: Improvements in displaying deployment steps
-   VFS-3636 GUI: Fix for invalid date in synchronization statistics
    charts
-   VFS-3202 GUI: Try to detect unfinished cluster deployment on page
    refresh
-   VFS-3870 GUI: Show notify after space support settings change
-   VFS-3706 GUI: Do not allow to enter improper routes in panel (fixes
    also VFS-3895: PATCH request after provider deregistration)
-   VFS-3928 GUI: Less restrictive validation of provider/onezone domain
    name
-   VFS-3677 GUI: Fix for tooltip positioning in mobile view
-   VFS-3592 GUI: Added common favicon
-   VFS-3883 GUI: Porting recent improvements in common components and
    utils from op-gui-default
-   VFS-3741 GUI: Fix for import chart tooltip positioning and overflow
    handling in mobile view
-   VFS-3677 GUI: Fix for tooltip positioning in mobile view
-   VFS-3741 GUI: Fix for import chart tooltip positioning and overflow
    handling in mobile view
-   VFS-3882 GUI: Fixed space auto-cleaning report status tooltips;
    deregister provider message update

### 17.06.2

-   Releasing new version 17.06.2

### 17.06.1

-   Releasing new version 17.06.1

### 17.06.0-rc9

-   VFS-4004 Update ctool to include safe ciphers in TLS
-   VFS-3972 Fix attach-direct consoles in releases not being run with
    xterm terminal
-   VFS-3911 - adapt onepanel to changes in op-worker
    storage\_sync\_monitoring module
-   fix error when geolocation is integer

### 17.06.0-rc8

-   Releasing new version 17.06.0-rc8

### 17.06.0-rc7

-   VFS-3815 Added erlang-observer as RPM build dependency
-   VFS-3686 allow to start space cleaning manually, update ctool
-   Updating GUI, including: VFS-3685, VFS-3661 - VFS-3685 Added space
    files popularity and space auto cleaning views - VFS-3661
    Improvements in presenting loading state of views and errors
-   Updating GUI, including: VFS-3710 - VFS-3710 Using binary prefixes
    for size units (IEC format: MiB, GiB, TiB, etc.)
-   Updating GUI, including: VFS-3737, VFS-3639 - VFS-3737 Change Bower
    server to registry.bower.io - VFS-3639 Fix and restore loaders in
    new GUIs
-   Updating GUI, including: VFS-3608 - VFS-3608 Table component for
    showing details about support size
-   VFS-3686 configure autocleaning in onepanel

### 17.06.0-rc6

-   Releasing new version 17.06.0-rc6

### 17.06.0-rc5

-   fix fetching luma\_config

### 17.06.0-rc4

-   Releasing new version 17.06.0-rc4

### 17.06.0-rc3

-   VFS-3639 Updating GUI ref
-   VFS-3449 add syncAcl flag to storage import and storage update
    configuration

### 17.06.0-rc2

-   VFS-3506 Fix space support with missing storage ID

### 17.06.0-rc1

-   VFS-3448 Use single \'onedata\' bucket
-   VFS-3384 add translation of luma configuration error
-   VFS-3384 setup luma when adding storage
-   VFS-3417 Increase default RAM quota of couchbase buckets

### 17.06.0-beta6

-   VFS-3417 Change listeners restart strategy
-   VFS-3356 Extend REST GET space/storage responses
-   VFS-3289 - change according to change in op\_worker
    storage\_sync\_monitoring API
-   VFS-3289 - storage\_sync metrics API

### 17.06.0-beta4

-   VFS-3362 Update web-client

### 17.06.0-beta3

-   Releasing new version 17.06.0-beta3

### 17.06.0-beta2

-   VFS-3345 Updating GUI ref (development) - added missing data-options
    in cluster-host-table-row (for testing purposes) - show glusterfs in
    storage type select dropdown without scroll
-   VFS-3280 Remove etls.
-   VFS-3250 Added GlusterFS support

### 3.0.0-rc16

-   VFS-3216 Update REST API
-   VFS-3207 Using new GUI ref
-   VFS-3216 Make GET method for onepanel hosts endpoint auth
-   VFS-3216 Use only storage ID in space support request
-   HOTFIX added gui-static dependency on package target in Makefile
-   VFS-3186 Change provider deregistration behaviour
-   VFS-3186 Update provider name after modify
-   VFS-3186 Return deployment name with cluster configuration
-   VFS-3163 Add session REST endpoint
-   VFS-3165 Add current password to user modify request
-   VFS-3118 Change default env value for custom gui root
-   VFS-3118 Add gui override and livereload to panel.up
-   VFS-3118 Add gui repo, serve static files using default gui logic,
    add templates for session and router plugins
-   VFS-3118 Serve index.html on root path
-   VFS-3118 Remove onepanel\_gui and serve static files

### 3.0.0-rc15

-   VFS-3233 Add support for sig v2 to AWS S3 helper
-   VFS-3213 Rename storage verification error codes
-   VFS-3213 Create new test files while verifying storage availability
-   VFS-3188 Remove space-storage mapping on space support revoke

### 3.0.0-rc14

-   Releasing new version 3.0.0-rc14

### 3.0.0-rc13

-   VFS-3117 Allow integer for provider geo long/lat
-   VFS-3086 Add cookie authentication

### 3.0.0-rc12

-   VFS-2907 Add mount in root and readonly options
-   VFS-3073 Set worker\_num for deployment
-   Add service watcher
-   VFS-2931 Reduce number of kept rotated log files
-   VFS-2910 Adjust code to LUMA refactoring
-   VFS-2620 Enable storage helper args update
-   VFS-2802 Add nagios proxy

### 3.0.0-rc11

-   VFS-2733 Standarize app listeners

### 3.0.0-rc10

-   minor changes and improvements

### 3.0.0-rc9

-   VFS-2550 Remove NIF libraries

### 3.0.0-rc8

-   VFS-2550 Template runner\_wait\_process
-   VFS-2550 Make add\_storages operation idempotent
-   VFS-2550 Make register operation idempotent
-   VFS-2550 Make add\_users operation idempotent

### 3.0.0-rc7

-   VFS-2550 Make configure operation idempotent

### 3.0.0-rc6

-   VFS-2180 Add \'sync\' bucket for oneprovider release
-   VFS-2525 Update onepanel\_gui ref
-   VFS-2525 Fix docs generation
-   VFS-2390 Upgrade rebar to version 3

### 3.0.0-rc5

-   VFS-2527 Add certs setup step to cluster worker deployment
-   VFS-2156 Add repeats to provider registration process
-   VFS-2468 Add step attempts in case of failure
-   VFS-2468 Make couchbase buckets configurable

### 3.0.0-rc4

-   VFS-2156 Add repeats to provider registration process

### 3.0.0-RC3

-   VFS-2156 Make \'/tasks\' endpoint unauthorized
-   VFS-2156 Add couchbase server and bucket quota
-   VFS-2156 Accept application/x-yaml content type
-   VFS-2156 Build package
-   VFS-2156 Add onepanel\_gui
-   Releasing new version 3.0.0-RC2
-   VFS-2269 Enable Symmetric Multiprocessing
-   VFS-1847 Add documentation
-   Releasing new version 3.0.0-RC1
-   VFS-1847 Integrate swagger REST API description
-   VFS-1847 Add REST requests parser
-   VFS-1847 Add NIF libraries
-   VFS-1847 Add REST handlers for all services
-   VFS-1847 Add onedata\_user REST handler
-   VFS-1847 Add onezone service
-   VFS-1847 Add oneprovider service
-   VFS-1847 Add cluster\_manager and cluster\_worker services
-   VFS-1847 Add couchbase service
-   VFS-1847 Add onedata\_user logic
-   VFS-1847 Add db models
-   VFS-1847 Add vm config editor.
-   VFS-1847 Start onepanel core project.

### 3.0.0-RC2

-   VFS-2269 Enable Symmetric Multiprocessing

### 3.0.0-RC1

-   minor changes and improvements

### 3.0.0-beta8

-   minor changes and improvements

### 3.0.0-beta7

-   VFS-2163 Allow user deletion
-   Update erlang tls
-   VFS-2163 onepanel user REST endpoint
-   VFS-2072 Add admin credentials to helpers args
-   VFS-2072 Remove user credentials from space support

### 3.0.0-beta4

-   VFS-1969 Add border to installation.gif.
-   VFS-1969 Update README.

### 3.0.0-beta3

-   VFS-1969 Update README.

### 3.0.0-beta2

-   VFS-1804 Enable couchbase memory quota configuration.

### 3.0.0-beta1

-   VFS-1804 Add success message to batch mode script.
-   VFS-1804 Remove logotype footer.
-   FS-1804 Add dns config option to batch mode config file.
-   VFS-1804 Add redirection point to oneprovider config.
-   VFS-1804 Use environmental variables to configure onedata services.
-   VFS-1598 Fix provider key/cert names.
-   VFS-1598 Fix username change and license display.
-   VFS-1598 Fix onepanel admin script.
-   VFS-1598 Adjust onepanel to onezone configuration.

### 3.0.1

-   VFS-1603 Disable generation of start\_clean.boot file in bin
    directory of a release.
-   VFS-1603 Rename gr\_panel to oz\_panel.
-   VFS-1603 Add package\_rel\_type to pkg.vars.config and use it during
    package build.
-   VFS-1603 Extend package post installation scripts for Global
    Registry release.
-   VFS-1528 Use monotonic time instead of system time
-   VFS-1603 Add ONEPANEL\_MULTICAST\_ADDRESS and ONEPANEL\_AUTOSTART
    environment variables to post package install script.
-   VFS-1603 Rename from CCM to CM.
-   VFS-1603 Increase RPC call timeout. Add retries to storage
    configuration process.
-   VFS-1603 Update package dependencies.
-   VFS-1603 Add distribution check to package rule in Makefile.
-   VFS-1528 Remove deprecated use of erlang:now/0

### 3.0.0

-   Dependencies management update
-   VFS-1472 Adjust storage and spaces management pages to Amazon S3
    storage helper.
-   VFS-1428 Adjust storage and spaces management pages to multiple
    storage helpers.
-   VFS-1428 Update provider database on space support.
-   VFS-1428 Update page storage and space management.
-   VFS-1428 Change page storage.
-   VFS-1455 Do not sed cookie of installed node, rename
    oneprovider\_node to op\_worker.
-   VFS-1455 Initialize storage when worker is started.
-   VFS-1193 switch to couchbase db
-   VFS-1142 Check return code during node startup.
-   VFS-1142 Add rpm depends and post install scripts.
-   VFS-1134 Rename op-onepanel -\> op-panel, gr-onepanel -\> gr-panel.
-   VFS-1150 Fix installer db multiple installations in row.
-   VFS-1150 Add deregistration during deinstallation to onepanel admin
    script.
-   VFS-1150 Templating onepanel admin script.
-   VFS-1150 Fix onepanel admin addition to package.
-   VFS-1150 Adjust op\_onepanel\_admin script.
-   VFS-1129 Add post install hostname sed and daemon startup.
-   VFS-1129 Add build dependency to pkg-config.
-   VFS-1129 Add deb build dependencies.
-   VFS-1100 Change default riak port
-   VFS-1053 add node\_package to gr\_onepanel
-   VFS-937 Showing provider name in GUI
-   VFS-937 Push channel management.
-   VFS-950 Checking ports during installation.
-   VFS-953 Sending space size in create/support requests.
-   VFS-937 Saving provider ID in CCM state.
-   VFS-937 Connecting/disconnecting to Global Registry during
    registration/deregistration.
-   VFS-915 Add html encoding.
-   VFS-915 Add breadcrumbs.
-   VFS-915 Add info about NAT/PAT in case of connection error.

### 2.0.0

-   Service command may be used after installation
-   Registration in globalregistry hidden

### 1.0.0

-   provide GUI for oneprovider installation and update.

------------------------------------------------------------------------

Generated by sr-release.
