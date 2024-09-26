#!/bin/sh
# Upgrade couchbase from 4.5.1 to 6.6.0.
# As direct upgrade is not possible we first upgrade to version 5.1.1 and then to 6.6.0

. /lib/lsb/init-functions

if [ "$(id -u)" != "0" ]; then
    log_failure_msg "Must be run as root"
    exit 1
fi

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
VERSIONFILE=/opt/couchbase/var/lib/couchbase/cb-ver
INITDLOG=/opt/couchbase/var/lib/couchbase/logs/initd.log
export HOME=/opt/couchbase/var/lib/couchbase

log_initd_msg() {
    TS=$(date +"%F %T")
    echo $TS "$@" | tee -a ${INITDLOG} 
}

upgrade_5() {
    log_initd_msg "======= Upgrading couchbase... ======="
    mv /opt/couchbase /opt/couchbase-6.6.0
    log_initd_msg "======= Downloading and unpacking  version 5.1.1 ========"
    curl -O https://packages.devel.onedata.org/debs/couchbase-server-community_5.1.1-ubuntu16.04_amd64.deb && \
    dpkg-deb -x couchbase-server-community_5.1.1-ubuntu16.04_amd64.deb cb5
    mv cb5/opt/couchbase /opt/couchbase-5.1.1 && chown -R couchbase.couchbase /opt/couchbase-5.1.1
    rm -rf /opt/couchbase-5.1.1/var/lib/couchbase
    ln -s /volumes/persistence/opt/couchbase/var/lib/couchbase /opt/couchbase-5.1.1/var/lib/couchbase
    rm -rf couchbase-server-community_5.1.1-ubuntu16.04_amd64.deb cb5
    ln -s /opt/couchbase-5.1.1 /opt/couchbase
    log_initd_msg "======= Upgrading to version 5.1.1 ========"
    /opt/couchbase/bin/install/cbupgrade -c /opt/couchbase/var/lib/couchbase/config -a yes >> ${INITDLOG} 2>&1
    errcode=$?
    if [ $errcode -ne 0 ]; then
        log_failure_msg "Failed to upgrade couchbase server to version 5.1.1"
    else
        echo "======= The upgrade to version 5.1.1 finished with success ======"
    fi
    return $errcode
}

upgrade_6() {
    rm /opt/couchbase
    ln -s /opt/couchbase-6.6.0 /opt/couchbase
    log_initd_msg "======= Upgrading to version 6.6.0 ========"
    echo "The couchbase upgrade can take some time."
    echo "Look at /opt/couchbase/var/lib/couchbase/initd.log in the container."
    /opt/couchbase/bin/cbupgrade -c /opt/couchbase/var/lib/couchbase/config -a yes >> ${INITDLOG} 2>&1
    errcode=$?
    if [ $errcode -ne 0 ]; then
        log_failure_msg "Failed to upgrade couchbase server to version 6.6.0"
    else
        echo "660" > $VERSIONFILE
        echo "======= The upgrade to version 6.6.0 finished with success ======"
    fi
    return $errcode
}


upgrade() {
    if ! upgrade_5; then
        return $errcode
    fi
    
    # For some reason the couchbase-server v5.1.1 should be stared and stopped before continuing with the upgrade to v6.6.0
    cp -p /opt/couchbase-6.6.0/etc/couchbase_init.d /opt/couchbase-5.1.1/etc/
    echo "Starting couchbase server v5.1.1"
    service couchbase-server start
    echo "Waiting for couchbase server to start listening on port 8091"
    while ! ss -nlt | grep :8091; do 
            sleep 5
            echo -n .
    done
    echo "Couchbase is up and running."    
    sleep 10
    echo "Wating for couchbase to get idle"
    # When no tasks are running the default output contains a list with one element - status
    # For details see https://docs.couchbase.com/server/current/rest-api/rest-get-cluster-tasks.html 
    while [ `curl -s -u admin:password http://localhost:8091/pools/default/tasks | jq length`x != 1x ];
    do
        sleep 5;
    done
    echo "Couchbase is up and idle."
    echo "Stopping couchbase for the next upgrade."
    service couchbase-server stop
    if ! upgrade_6; then
        return $errcode
    fi
}

should_upgrade() {
    if [ -e $VERSIONFILE ]; then
        v=$(cat $VERSIONFILE)
        if [ $v -ge "660" ]; then
            log_initd_msg "Couchbase already in version 6.6.0 or newer, skipping upgrade."
            return 1
        fi
    fi      
}    

check_fresh_install() {
    if [ "$(ls -A /var/lib/op_panel/mnesia)" ]; then
        return 0
    else
       echo "This is fresh install of oneprovider. No need for couchbase upgrade."
       return 1
    fi
}

if check_fresh_install; then
    if should_upgrade; then
        if ! upgrade; then
            return $errcode
        fi
    fi
fi
