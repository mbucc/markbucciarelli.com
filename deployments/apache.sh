#! /bin/sh 
# Script run by Let's Encrypt after renewing a certificate.
# November 13, 2022

/sbin/rc-service apache2 restart
