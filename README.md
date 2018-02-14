# onepanel

*onepanel* is an administrative tool for a meta file system, called *onedata*,
which unifies access to different storage systems and provides a POSIX
compatible interface.

### Goals

The main goal of *onepanel* is to enable configuration of *onedata* cluster
components in a distributed environment. It provides a console client, which
works in a batch mode, and a web GUI. Both allow for configuration and
management of [cluster manager](https://github.com/onedata/cluster-manager),
[cluster worker](https://github.com/onedata/cluster-worker) and
[couchbase database](http://www.couchbase.com/) components.

# User Guide

## Requirements

*onepanel* requires Erlang version 18 or newer.

## Compilation

*onepanel* works with both [oneprovider](https://github.com/onedata/op-worker)
and [onezone](https://github.com/onedata/oz-worker) components.

In order to compile *onepanel* execute:

```bash
git clone https://github.com/onedata/onepanel.git
cd onepanel
make REL_TYPE=<type> rel
```

where `<type>` can be on of *oneprovider* or *onezone*. The generated application can be found in `rel` directory and it is named, depending on the `REL_TYPE`, either `op_panel` or `oz_panel`.

## Running

In order to run *onepanel* application execute:

```bash
./rel/<app name>/bin/<app name> console
```

## Cluster Configuration

*onepanel* is started when *oneprovider* or *onezone* software package is
installed. It discovers its nodes and sets up administrative cluster using
multicast address.

### Web GUI Configuration

Navigate to *https://\<host\>:9443*, where *host* is equal to fully qualified hostname of machine on which software package has been installed, and follow the instructions.

### Batch Mode Configuration

*onepanel* console client is named *op_panel_admin* or *oz_panel_admin* depending on release type.

In order to configure and start cluster components execute:

```bash
<console client name> --install <path to configuration file>
```

In order to stop and remove cluster components execute:

```bash
<console client name> --uninstall <path to configuration file>
```

In order to get current configuration execute:

```bash
<console client name> --config
```

### Sample Configuration File (onezone)

Below you can find a sample configuration file that sets up single-node onezone
cluster.

```yaml
cluster:
  domainName: "onezone.1.test"
  autoDeploy: true
  nodes:
    n1:
      hostname: "node1"
    n2:
      hostname: "node2"
  managers:
    mainNode: "n1"
    nodes:
      - "n1"
      - "n2"
  workers:
    nodes:
      - "n1"
      - "n2"
  databases:
    serverQuota: 4096
    bucketQuota: 1024
    nodes:
      - "n1"
      - "n2"
onezone:
  name: "Example"
  domainName: "onezone.1.test"
onepanel:
  users:
    "admin1":
      password: "Password1"
      userRole: "admin"
    "user1":
      password: "Password2"
      userRole: "regular"
```

### Sample Configuration File (oneprovider)

Below you can find a sample configuration file that sets up single-node
oneprovider cluster and registers it in onezone configured in previous section.

```yaml
cluster:
  domainName: "oneprovider.1.test"
  autoDeploy: true
  nodes:
    n1:
      hostname: "node1"
    n2:
      hostname: "node2"
  managers:
    mainNode: "n1"
    nodes:
      - "n1"
      - "n2"
  workers:
    nodes:
      - "n1"
      - "n2"
  databases:
    serverQuota: 4096
    bucketQuota: 1024
    nodes:
      - "n1"
      - "n2"
  storages:
    "NFS":
      type: "posix"
      mountPoint: "/mnt/st1"
oneprovider:
  register: true
  name: "Provider"
  redirectionPoint: "https://node1.oneprovider.1.test"
  geoLatitude: 10.0
  geoLongitude: 20.0
onezone:
  domainName: "node1.onezone.1.test"
onepanel:
  users:
    "admin1":
      password: "Password1"
      userRole: "admin"
    "user1":
      password: "Password2"
      userRole: "regular"
```
