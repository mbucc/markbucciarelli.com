November 11, 2022

tags: alpine ssh osx

How to Setup Alpine Linux on Kernel-Based Virtual Machine
=================================================.


Install Alpine
----------------------------

The
<? mn: mn-happy_customer ?>
I've been a happy customer of RAMHost for over eight years.
It's a great deal and the support is excellent.
<?mn?>
kernel-based virtual machine (KVM) I'm using here is
<a href=https://tinykvm.com>a TinyKVM from RAMHost</a>, which you can get for $15/year.
It takes
about five minutes to set one up.

1. Login to your TinyKVM account.
2. Note down the IP address, netmask and gateway.
3. Pick "OS Manual Reload" icon.
4. Select "alpine-extended-3.16.0-x86_64.iso" and click the Reload button.
5. Wait a couple minutes.
6. Click "Back to Main".
7. Click the "VNC Console" icon.
8. Click ">> Launch NoVNC Client (recommended) <<".
9. Maximize the browser.
10. You should see the Alpine Linux login prompt.

Setup Alpine
-------------------------

If you make a mistake, just press Ctrl-C and then restart
setup-alpine.

1. Login as root.
2. Type setup-alpine at prompt and hit Enter.
3. Enter the static IP / Gateway / Net Mask that you noted down above.
4. 8.8.8.8 (Google) for DNS server
5. Enter "sda" for hard disk to use.
<? mn: mn-missed_it ?>
I missed entering "sda" the first four times through 
the installer.   ☹️ 
<?mn?>
6. "sys" for how you would like to use the disk.
7. "y" erase disk and set it up.
8. reboot

Upgrade Alpine to the Latest
-------------------------

> When Alpine Linux is installed in sys mode, e.g. on a hard drive,
> upgrading to the next stable version should be a straightforward
> package manager operation. However, for specific info always refer
> to the appropriate release notes.
>
> <footer><a href=https://wiki.alpinelinux.org/wiki/Upgrading_Alpine#Upgrading_to_latest_release>Alpine Wiki, Upgrading to latest release</a></footer>

1. Open
<? mn: mn-alpine_versions ?>
RAMHost had 3.16.0 Alpine and the latest is 3.16.3,
so <code>/etc/apk/repositories</code> can stay as is.
<?mn?>
<a href=https://www.alpinelinux.org>https://www.alpinelinux.org</a> to see the latest version.
2. <code>apk update</code>
3. <code>apk upgrade</code>

Install Basic Packages
----------------------

    apk add man-pages man-pages-posix mandoc-apropos mandoc-doc ed-doc
    apk add openssh-doc

Setup SSH
---------------

Follow these steps at your discretion.  I'm creating a private SSH key that
allows me to SSH as the root user if I know the passphrase.  I'm storing this
passphrase in my OSX Keychain.

Generate a 60-character random password and copy to the clipboard (on OSX).

    pwgen -s 60|pbcopy

Create an SSH key, pressing command-V to paste in the password.

    cd ~/.ssh
    ssh-keygen -f id_kvm2_rsa

Add the passphrase to OSX Keychain.

    ssh-add --apple-use-keychain ~/.ssh/id_kvm2_rsa

Configure SSH-agent to use the OSX Keychain for this host.

    cat >> ~/.ssh/config
    
    Host testblog
      HostName test.markbucciarelli.com
      User root
      UseKeychain yes
      AddKeysToAgent yes
      IdentityFile ~/.ssh/id_kvm2_rsa
    ^D
Authorize the key on the target host.

    rc-status 
    Runlevel: default
     sshd                        [ started ]
    ...

    ed /etc/ssh/sshd_config
    g/^PermitRootLogin/ s/prohibit-password/yes/
    wq

    rc-service sshd restart

Back on laptop:

    ssh-copy_id -i ~/.ssh/id_kvm2_rsa root@test.markbucciarelli.com


On KVM:

    ed /etc/ssh/sshd_config
    g/^PermitRootLogin/ s/yes/prohibit-password/
    wq
    rc-service sshd restart

