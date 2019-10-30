#### Improvements

* VFS-4777 Introduced Ceph cluster deployment. Onepanel can orchestrate
  Ceph deployment among Onedata nodes and use the cluster as Oneprovider's
  storage backend.
* VFS-4777 Rest endpoint getTaskStatus now includes total number of steps to be
  executed.


#### Bugfixes


#### Removals

* VFS-4777 Creation of storage type `ceph` is no longer possible. Use `cephrados`
  instead. Existing storages of this type will continue to work.
