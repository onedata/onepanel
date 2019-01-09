#!/usr/bin/env bash

#####################################################################
# @author Lukasz Opiola
# @copyright (C): 2017 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# usage:
#       This script is not used directly - it should be passed to
#       deps/gui/pull-gui.sh.
#
# This script contains configuration that is required to pull GUI to OZ
# worker (by copying static files from a static docker).
#####################################################################

# Configuration
# Directory relative to this script, to which static GUI files will be copied.
# First put them in deps, later after release generation they will be copied
# from there to release (see Makefile).
TARGET_DIR='_build/default/lib/gui_static'
# Image which will be used by default to get the static files. If it cannot
# be resolved, the script will fall back to secondary.
# @FIXME use 18.07 version
PRIMARY_IMAGE='docker.onedata.org/onepanel-gui:VFS-5153-dns-autodetect-option-18.07'
# Image which will be used if primary image is not resolved.
# @FIXME use 18.07 version
SECONDARY_IMAGE='onedata/onepanel-gui:VFS-5153-dns-autodetect-option-18.07'
