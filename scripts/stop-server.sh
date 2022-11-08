#! /bin/sh -e
# November 7, 2022
# Stop the thttpd server.

pkill thttpd || echo "thttpd is not running."
