#### Improvements

* VFS-4777 Introduced Ceph cluster deployment. Onepanel can orchestrate
  Ceph deployment among Onedata nodes and use the cluster as Oneprovider's
  storage backend.
* VFS-4777 REST endpoint getTaskStatus now includes total number of steps to be
  executed.
* VFS-5841 The Onepanel REST API now uses the same error classes as other
  Onedata services. Each error has a distinct id and may have well-structured
  detailed information.
* VFS-5819 Changed `mount_in_root` to be a storage parameter and renamed 
  it to `import_existing_data`. Storage with this option selected can support 
  only one space and has data import enabled.
* VFS-5901 Application config can now be customized with arbitrary number
  of config files added to config.d directory in /etc.
* VFS-5838 Significant internal logic refactor. Includes minor API changes:
    * Endpoints creating a long-running task return a JSON body with the task id
      in addition to the Location header and use HTTP code 202 Accepted.
    * POST requests 'support_space' and 'add_onezone_user' now use
      the 201 Created HTTP code, return the id of created resource
      in the response body, and provider the Location header.
    * PATCH request modifying space support no longer returns the space id
      in the response body.
    * All endpoints may return 401 Unauthorized if conditions for
      credential-less access are not met.


#### Bugfixes


#### Removals

* VFS-4777 Creation of storage type `ceph` is no longer possible. Use `cephrados`
  instead. Existing storages of this type will continue to work.
