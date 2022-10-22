#! /bin/sh -e
# Run local web server to test site.
# October 19, 2022

# sudo port install thttpd

thttpd -d ./www -p 8080 -l /dev/stdout
