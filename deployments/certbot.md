November 12, 2022

Notes on getting certbot working
=================================


Need to install certbot-apache package.

    The requested apache plugin does not appear to be installed


Need to symlink /usr/sbin/apachectl to /usr/sbin/apache2ctl
Or pass `--apache-ctl /usr/sbin/apachectl` arg to certbot.

    The apache plugin is not working; there may be problems with your existing configuration.
    The error was: NoInstallationError('Cannot find Apache executable apache2ctl')

Need to install SSL module.


    dev:~# certbot --apache
    Saving debug log to /var/log/letsencrypt/letsencrypt.log
    Could not find ssl_module; not disabling session tickets.
    Enter email address (used for urgent renewal and security notices)
     (Enter 'c' to cancel): mkbucc@gmail.com
    
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Please read the Terms of Service at
    https://letsencrypt.org/documents/LE-SA-v1.3-September-21-2022.pdf. You must
    agree in order to register with the ACME server. Do you agree?
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (Y)es/(N)o: y
    
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Would you be willing, once your first certificate is successfully issued, to
    share your email address with the Electronic Frontier Foundation, a founding
    partner of the Let's Encrypt project and the non-profit organization that
    develops Certbot? We'd like to send you email about our work encrypting the web,
    EFF news, campaigns, and ways to support digital freedom.
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    (Y)es/(N)o: y
    Account registered.
    
    Which names would you like to activate HTTPS for?
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    1: test.markbucciarelli.com
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Select the appropriate numbers separated by commas and/or spaces, or leave input
    blank to select all options shown (Enter 'c' to cancel): 1
    Requesting a certificate for test.markbucciarelli.com
    
    Successfully received certificate.
    Certificate is saved at: /etc/letsencrypt/live/test.markbucciarelli.com/fullchain.pem
    Key is saved at:         /etc/letsencrypt/live/test.markbucciarelli.com/privkey.pem
    This certificate expires on 2023-02-10.
    These files will be updated when the certificate renews.
    
    Deploying certificate
    Could not install certificate
    
    NEXT STEPS:
    - The certificate was saved, but could not be installed (installer: apache). After fixing the error shown below, try installing it again by running:
      certbot install --cert-name test.markbucciarelli.com
    - The certificate will need to be renewed before it expires. Certbot can automatically renew the certificate in the background, but you may need to take steps to enable that functionality. See https://certbot.org/renewal-setup for instructions.
    
    Could not find ssl_module; not installing certificate.
    Ask for help or search for solutions at https://community.letsencrypt.org. See the logfile /var/log/letsencrypt/letsencrypt.log or re-run Certbot with -v for more details.
    dev:~# 



After installing apache ssl:


    dev:/etc/apache2# certbot install --cert-name test.markbucciarelli.com
    Saving debug log to /var/log/letsencrypt/letsencrypt.log
    Unable to read ssl_module file; not disabling session tickets.
    Deploying certificate
    Successfully deployed certificate for test.markbucciarelli.com to /etc/apache2/conf.d/test.markbucciarelli.com-le-ssl.conf
    dev:/etc/apache2# 
