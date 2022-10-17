#! /bin/sh -e

/opt/local/sbin/apachectl -t
sudo port unload apache2
sudo port load apache2
