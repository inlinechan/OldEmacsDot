#!/bin/bash
# This script is for integration check-webkit-style with flymake

# flymake-check-webkit-style /home/hyungchan/QtWebKit/ Page_flymake.cpp
CHECK_SCRIPT="Tools/Scripts/check-webkit-style"
ROOT_DIR=$1
TARGET_FILE=$2

# /home/hyungchan/QtWebKit/Tools/Scripts/check-webkit-style Page_flymake.cpp
${ROOT_DIR}${CHECK_SCRIPT} $TARGET_FILE 2>&1 | sed -e "/$TARGET_FILE/!d" | \
    # Source/WebCore/page/Page_flymake.cpp:... -> Page_flymake.cpp:...
    sed -e 's,^\([^:]*\)/,,' | \
    # Page_flymake.cpp -> Page.cpp
    sed -e 's/_flymake//g' >&2
    # >> /tmp/check-webkit-style-log

exit 0
