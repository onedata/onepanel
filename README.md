# onepanel

*onepanel* is an administrative tool for a meta file system, called *onedata*,
which unifies access to different storage systems and provides a POSIX
compatible interface.

![](https://raw.githubusercontent.com/onedata/onepanel/develop/media/installation.gif)

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
  domain_name: "onezone.dev.local"
  nodes:
    node1:
      hostname: "node1"
  manager:
    default_node: "node1"
    nodes:
      - "node1"
  worker:
    nodes:
      - "node1"
  database:
    nodes:
      - "node1"
  settings:
    open_id_auth_config: "https://raw.githubusercontent.com/krzysztof-trzepla/onedata-getting-started/develop/files/oz_open_id_auth.config"
onezone:
  name: "example"
```

### Sample Configuration File (oneprovider)

Below you can find a sample configuration file that sets up single-node
oneprovider cluster and registers it in onezone configured in previous section.

```yaml
cluster:
  domain_name: "oneprovider.dev.local"
  nodes:
    node1:
      hostname: "node1"
  manager:
    default_node: "node1"
    nodes:
      - "node1"
  worker:
    nodes:
      - "node1"
  database:
    nodes:
      - "node1"
  storage:
    NFS:
      type: "POSIX"
      mount_point: "/tmp"
oneprovider:
  register: true
  name: "example"
  redirection_point: "https://node1.oneprovider.dev.local"
onezone:
  domain_name: "node1.onezone.dev.local"
```
