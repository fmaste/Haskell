# Installing a Binary Distribution

## Single user vs. multi user

## Install

```
$ sh <(curl -L https://nixos.org/nix/install) --daemon
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
  0     0    0     0    0     0      0      0 --:--:-- --:--:-- --:--:--     0
100  4053  100  4053    0     0   2233      0  0:00:01  0:00:01 --:--:--  7824
downloading Nix 2.8.1 binary tarball for x86_64-linux from 'https://releases.nixos.org/nix/nix-2.8.1/nix-2.8.1-x86_64-linux.tar.xz' to '/tmp/nix-binary-tarball-unpack.lxqDxpPwii'...
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 27.5M  100 27.5M    0     0  1452k      0  0:00:19  0:00:19 --:--:-- 2331k
Note: a multi-user installation is possible. See https://nixos.org/manual/nix/stable/installation/installing-binary.html#multi-user-installation
Switching to the Multi-user Installer
Welcome to the Multi-User Nix Installation

This installation tool will set up your computer with the Nix package
manager. This will happen in a few stages:

1. Make sure your computer doesn't already have Nix. If it does, I
   will show you instructions on how to clean up your old install.

2. Show you what I am going to install and where. Then I will ask
   if you are ready to continue.

3. Create the system users and groups that the Nix daemon uses to run
   builds.

4. Perform the basic installation of the Nix files daemon.

5. Configure your shell to import special Nix Profile files, so you
   can use Nix.

6. Start the Nix daemon.

Would you like to see a more detailed list of what I will do?
[y/n] y


I will:

 - make sure your computer doesn't already have Nix files
   (if it does, I will tell you how to clean them up.)
 - create local users (see the list above for the users I'll make)
 - create a local group (nixbld)
 - install Nix in to /nix
 - create a configuration file in /etc/nix
 - set up the "default profile" by creating some Nix-related files in
   /root
 - back up /etc/bash.bashrc to /etc/bash.bashrc.backup-before-nix
 - update /etc/bash.bashrc to include some Nix configuration
 - load and start a service (at /etc/systemd/system/nix-daemon.service
   and /etc/systemd/system/nix-daemon.socket) for nix-daemon

Ready to continue?
[y/n] y


---- let's talk about sudo -----------------------------------------------------
This script is going to call sudo a lot. Every time I do, it'll
output exactly what it'll do, and why.

Just like this:

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo echo

to demonstrate how our sudo prompts look


This might look scary, but everything can be undone by running just a
few commands. I used to ask you to confirm each time sudo ran, but it
was too many times. Instead, I'll just ask you this one time:

Can I use sudo?
[y/n] y

Yay! Thanks! Let's get going!

~~> Checking for artifacts of previous installs
Before I try to install, I'll check for signs Nix already is or has
been installed on this system.

---- Nix config report ---------------------------------------------------------
        Temp Dir:       /tmp/tmp.aWWHvVK2Hk
        Nix Root:       /nix
     Build Users:       32
  Build Group ID:       30000
Build Group Name:       nixbld

build users:
    Username:   UID
     nixbld1:   30001
     nixbld2:   30002
     nixbld3:   30003
     nixbld4:   30004
     nixbld5:   30005
     nixbld6:   30006
     nixbld7:   30007
     nixbld8:   30008
     nixbld9:   30009
     nixbld10:  30010
     nixbld11:  30011
     nixbld12:  30012
     nixbld13:  30013
     nixbld14:  30014
     nixbld15:  30015
     nixbld16:  30016
     nixbld17:  30017
     nixbld18:  30018
     nixbld19:  30019
     nixbld20:  30020
     nixbld21:  30021
     nixbld22:  30022
     nixbld23:  30023
     nixbld24:  30024
     nixbld25:  30025
     nixbld26:  30026
     nixbld27:  30027
     nixbld28:  30028
     nixbld29:  30029
     nixbld30:  30030
     nixbld31:  30031
     nixbld32:  30032

Ready to continue?
[y/n] y


~~> Setting up the build group nixbld

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo groupadd -g 30000 --system nixbld

Create the Nix build group, nixbld

            Created:    Yes

~~> Setting up the build user nixbld1

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 1 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30001 --password ! nixbld1

Creating the Nix build user, nixbld1

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 1 nixbld1

in order to give nixbld1 a useful comment

usermod: no changes
              Note:     Nix build user 1
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld2

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 2 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30002 --password ! nixbld2

Creating the Nix build user, nixbld2

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 2 nixbld2

in order to give nixbld2 a useful comment

usermod: no changes
              Note:     Nix build user 2
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld3

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 3 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30003 --password ! nixbld3

Creating the Nix build user, nixbld3

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 3 nixbld3

in order to give nixbld3 a useful comment

usermod: no changes
              Note:     Nix build user 3
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld4

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 4 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30004 --password ! nixbld4

Creating the Nix build user, nixbld4

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 4 nixbld4

in order to give nixbld4 a useful comment

usermod: no changes
              Note:     Nix build user 4
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld5

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 5 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30005 --password ! nixbld5

Creating the Nix build user, nixbld5

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 5 nixbld5

in order to give nixbld5 a useful comment

usermod: no changes
              Note:     Nix build user 5
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld6

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 6 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30006 --password ! nixbld6

Creating the Nix build user, nixbld6

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 6 nixbld6

in order to give nixbld6 a useful comment

usermod: no changes
              Note:     Nix build user 6
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld7

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 7 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30007 --password ! nixbld7

Creating the Nix build user, nixbld7

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 7 nixbld7

in order to give nixbld7 a useful comment

usermod: no changes
              Note:     Nix build user 7
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld8

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 8 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30008 --password ! nixbld8

Creating the Nix build user, nixbld8

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 8 nixbld8

in order to give nixbld8 a useful comment

usermod: no changes
              Note:     Nix build user 8
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld9

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 9 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30009 --password ! nixbld9

Creating the Nix build user, nixbld9

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 9 nixbld9

in order to give nixbld9 a useful comment

usermod: no changes
              Note:     Nix build user 9
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld10

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 10 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30010 --password ! nixbld10

Creating the Nix build user, nixbld10

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 10 nixbld10

in order to give nixbld10 a useful comment

usermod: no changes
              Note:     Nix build user 10
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld11

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 11 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30011 --password ! nixbld11

Creating the Nix build user, nixbld11

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 11 nixbld11

in order to give nixbld11 a useful comment

usermod: no changes
              Note:     Nix build user 11
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld12

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 12 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30012 --password ! nixbld12

Creating the Nix build user, nixbld12

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 12 nixbld12

in order to give nixbld12 a useful comment

usermod: no changes
              Note:     Nix build user 12
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld13

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 13 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30013 --password ! nixbld13

Creating the Nix build user, nixbld13

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 13 nixbld13

in order to give nixbld13 a useful comment

usermod: no changes
              Note:     Nix build user 13
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld14

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 14 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30014 --password ! nixbld14

Creating the Nix build user, nixbld14

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 14 nixbld14

in order to give nixbld14 a useful comment

usermod: no changes
              Note:     Nix build user 14
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld15

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 15 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30015 --password ! nixbld15

Creating the Nix build user, nixbld15

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 15 nixbld15

in order to give nixbld15 a useful comment

usermod: no changes
              Note:     Nix build user 15
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld16

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 16 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30016 --password ! nixbld16

Creating the Nix build user, nixbld16

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 16 nixbld16

in order to give nixbld16 a useful comment

usermod: no changes
              Note:     Nix build user 16
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld17

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 17 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30017 --password ! nixbld17

Creating the Nix build user, nixbld17

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 17 nixbld17

in order to give nixbld17 a useful comment

usermod: no changes
              Note:     Nix build user 17
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld18

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 18 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30018 --password ! nixbld18

Creating the Nix build user, nixbld18

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 18 nixbld18

in order to give nixbld18 a useful comment

usermod: no changes
              Note:     Nix build user 18
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld19

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 19 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30019 --password ! nixbld19

Creating the Nix build user, nixbld19

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 19 nixbld19

in order to give nixbld19 a useful comment

usermod: no changes
              Note:     Nix build user 19
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld20

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 20 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30020 --password ! nixbld20

Creating the Nix build user, nixbld20

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 20 nixbld20

in order to give nixbld20 a useful comment

usermod: no changes
              Note:     Nix build user 20
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld21

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 21 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30021 --password ! nixbld21

Creating the Nix build user, nixbld21

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 21 nixbld21

in order to give nixbld21 a useful comment

usermod: no changes
              Note:     Nix build user 21
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld22

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 22 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30022 --password ! nixbld22

Creating the Nix build user, nixbld22

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 22 nixbld22

in order to give nixbld22 a useful comment

usermod: no changes
              Note:     Nix build user 22
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld23

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 23 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30023 --password ! nixbld23

Creating the Nix build user, nixbld23

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 23 nixbld23

in order to give nixbld23 a useful comment

usermod: no changes
              Note:     Nix build user 23
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld24

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 24 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30024 --password ! nixbld24

Creating the Nix build user, nixbld24

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 24 nixbld24

in order to give nixbld24 a useful comment

usermod: no changes
              Note:     Nix build user 24
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld25

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 25 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30025 --password ! nixbld25

Creating the Nix build user, nixbld25

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 25 nixbld25

in order to give nixbld25 a useful comment

usermod: no changes
              Note:     Nix build user 25
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld26

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 26 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30026 --password ! nixbld26

Creating the Nix build user, nixbld26

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 26 nixbld26

in order to give nixbld26 a useful comment

usermod: no changes
              Note:     Nix build user 26
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld27

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 27 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30027 --password ! nixbld27

Creating the Nix build user, nixbld27

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 27 nixbld27

in order to give nixbld27 a useful comment

usermod: no changes
              Note:     Nix build user 27
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld28

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 28 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30028 --password ! nixbld28

Creating the Nix build user, nixbld28

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 28 nixbld28

in order to give nixbld28 a useful comment

usermod: no changes
              Note:     Nix build user 28
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld29

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 29 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30029 --password ! nixbld29

Creating the Nix build user, nixbld29

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 29 nixbld29

in order to give nixbld29 a useful comment

usermod: no changes
              Note:     Nix build user 29
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld30

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 30 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30030 --password ! nixbld30

Creating the Nix build user, nixbld30

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 30 nixbld30

in order to give nixbld30 a useful comment

usermod: no changes
              Note:     Nix build user 30
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld31

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 31 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30031 --password ! nixbld31

Creating the Nix build user, nixbld31

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 31 nixbld31

in order to give nixbld31 a useful comment

usermod: no changes
              Note:     Nix build user 31
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the build user nixbld32

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo useradd --home-dir /var/empty --comment Nix build user 32 --gid 30000 --groups nixbld --no-user-group --system --shell /sbin/nologin --uid 30032 --password ! nixbld32

Creating the Nix build user, nixbld32

           Created:     Yes
            Hidden:     Yes
    Home Directory:     /var/empty

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo usermod --comment Nix build user 32 nixbld32

in order to give nixbld32 a useful comment

usermod: no changes
              Note:     Nix build user 32
   Logins Disabled:     Yes
  Member of nixbld:     Yes
    PrimaryGroupID:     30000

~~> Setting up the basic directory structure

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo install -dv -m 0755 /nix /nix/var /nix/var/log /nix/var/log/nix /nix/var/log/nix/drvs /nix/var/nix /nix/var/nix/db /nix/var/nix/gcroots /nix/var/nix/profiles /nix/var/nix/temproots /nix/var/nix/userpool /nix/var/nix/daemon-socket /nix/var/nix/gcroots/per-user /nix/var/nix/profiles/per-user

to make the basic directory structure of Nix (part 1)

install: creating directory '/nix'
install: creating directory '/nix/var'
install: creating directory '/nix/var/log'
install: creating directory '/nix/var/log/nix'
install: creating directory '/nix/var/log/nix/drvs'
install: creating directory '/nix/var/nix'
install: creating directory '/nix/var/nix/db'
install: creating directory '/nix/var/nix/gcroots'
install: creating directory '/nix/var/nix/profiles'
install: creating directory '/nix/var/nix/temproots'
install: creating directory '/nix/var/nix/userpool'
install: creating directory '/nix/var/nix/daemon-socket'
install: creating directory '/nix/var/nix/gcroots/per-user'
install: creating directory '/nix/var/nix/profiles/per-user'

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo install -dv -g nixbld -m 1775 /nix/store

to make the basic directory structure of Nix (part 2)

install: creating directory '/nix/store'

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo install -dv -m 0555 /etc/nix

to place the default nix daemon configuration (part 1)

install: creating directory '/etc/nix'

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo install -m 0664 /tmp/tmp.aWWHvVK2Hk/.nix-channels /root/.nix-channels

to set up the default system channel (part 1)


~~> Installing Nix

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo cp -RPp ./store/0l8kkiv0d31hwh2f4x6142drcdibhx7n-keyutils-1.6.3-lib ./store/2npn6mbl8fx14df3avk7sgplr7aaif3c-editline-1.17.1 ./store/2qd7zgbsdsbf4gwlap2nhsb5da00rw86-zstd-1.4.9 ./store/3ss84qd3j02pd10sagbpbk8gz5411hjc-perl-5.32.1 ./store/3zi155ps4sf8y1h2yp6wbghvhpk1h6d6-aws-c-io-0.9.1 ./store/5qbx0wy48f55pybnii26428z24zh07ln-nss-cacert-3.66 ./store/69x2irfhvhscdb61j51qda7l93jzlim3-brotli-1.0.9-lib ./store/6dgkss765krj2x8k7cj1b1gnxvjxnm8f-aws-checksums-0.1.11 ./store/6mjgljq8sm9bsz6k22as5ar3jw78644m-nix-2.8.1 ./store/6sh733l9nz46j5bnbndybndl4s35dg7k-attr-2.4.48 ./store/71ifgy0c7w99kprfxi7iiv85f2bm7c7p-aws-c-common-0.5.5 ./store/7bzaif84k0ia8zkh4mg0c1kgibnb744n-libidn2-2.3.1 ./store/86nwp1qr2q54gaz07hz5h55rqz962qds-nlohmann_json-3.9.1 ./store/8930fqjjypcnqgraxrbgnym64vvi8dmk-boehm-gc-8.0.4 ./store/8jn2afds8gxpz5zq3dd667j2mrcjlch8-sqlite-3.35.5 ./store/b48mzk6yf4i897zzy0waqybdkv7ws227-zlib-1.2.11 ./store/b4bclrfy1jljzvic10qv5iymw68mzrc0-openssl-1.1.1l ./store/c6inislcbc0g5c0i4ck0a0pmkskar1sg-libsodium-1.0.18 ./store/cd91x4x64ijzyxapqp68c8i6q1r66pri-libkrb5-1.18 ./store/dzrvibwj2vjwqmc34wk3x1ffsjpp4av7-bash-4.4-p23 ./store/fkxbprfwzz14509236621bfvj9xwq6dg-nghttp2-1.43.0-lib ./store/j15xgicdd80l2qkirqq2yd7s60ckhhs1-openssl-1.1.1l-bin ./store/jy3gc3sdyd8j2ylw346f5zw2bn3705gd-aws-sdk-cpp-1.8.121 ./store/l4k6scch9g55v1g9j347ninn7k5g096q-libcpuid-0.5.1 ./store/lm88rijmrs98k3svxlypwll0p23zp2zc-libxml2-2.9.12 ./store/lvqrpsasgr3jan8l6yfhvcv628jayk2x-busybox-static-x86_64-unknown-linux-musl-1.32.1 ./store/nizz9fc4wd7qs916x9y343rg80vya4hp-curl-7.76.1 ./store/nprym6lf8lzhp1irb42lb4vp8069l5rj-glibc-2.32-54 ./store/nxkcjcdb9bfh9fvili44b6g2all08vr3-libunistring-0.9.10 ./store/paqfl70z4zxip8lvpsijbspi0y2wzg4i-gcc-10.3.0-lib ./store/pjcrnw6yk4y7793yxz231nslq3ar31lj-aws-c-cal-0.4.5 ./store/pla78sns8gz0l0picyajgmgrcfp4m2k5-libssh2-1.9.0 ./store/r8ba12j6x2771jd1q252xgq2iflcrw3g-xz-5.2.5 ./store/sg1nhv53f5skcb20566kpr8jqfv7cnjl-libarchive-3.5.2-lib ./store/vxnh0p57lh7xvsbvd6mr7rb7dmk6j2la-acl-2.3.0 ./store/whfwl7jbf4n45zxa9rl53d4hdwx67abn-libseccomp-2.5.1-lib ./store/wr80zn4fj1h4a0mqrqdqayk11p8cr3di-openssl-1.1.1l-dev ./store/x8lng14s9w4qpasdcqj3zlfaf6ajnlkq-aws-c-event-stream-0.2.7 ./store/y41s1vcn0irn9ahn9wh62yx2cygs7qjj-coreutils-8.32 ./store/yyc3wki6rx2kfhfa7vg37rk1zgxzwpc0-s2n-tls-1.0.0 ./store/zix9d9fqc4k1jn6gc6510c25irnziav1-bzip2-1.0.6.0.2 /nix/store/

to copy the basic Nix files to the new store at /nix/store


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo chmod -R ugo-w /nix/store/

to make the new store non-writable at /nix/store

      Alright! We have our first nix at /nix/store/6mjgljq8sm9bsz6k22as5ar3jw78644m-nix-2.8.1

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo /nix/store/6mjgljq8sm9bsz6k22as5ar3jw78644m-nix-2.8.1/bin/nix-store --load-db

to load data for the first time in to the Nix Database

      Just finished getting the nix database ready.

~~> Setting up shell profiles: /etc/bashrc /etc/profile.d/nix.sh /etc/zshrc /etc/bash.bashrc /etc/zsh/zshrc

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo touch /etc/bashrc

to create a stub /etc/bashrc which will be updated


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo tee -a /etc/bashrc

extend your /etc/bashrc with nix-daemon settings


# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo touch /etc/profile.d/nix.sh

to create a stub /etc/profile.d/nix.sh which will be updated


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo tee -a /etc/profile.d/nix.sh

extend your /etc/profile.d/nix.sh with nix-daemon settings


# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo touch /etc/zshrc

to create a stub /etc/zshrc which will be updated


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo tee -a /etc/zshrc

extend your /etc/zshrc with nix-daemon settings


# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo cp /etc/bash.bashrc /etc/bash.bashrc.backup-before-nix

to back up your current /etc/bash.bashrc to /etc/bash.bashrc.backup-before-nix


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo tee -a /etc/bash.bashrc

extend your /etc/bash.bashrc with nix-daemon settings


# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix


~~> Setting up the default profile

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo HOME=/root /nix/store/6mjgljq8sm9bsz6k22as5ar3jw78644m-nix-2.8.1/bin/nix-env -i /nix/store/6mjgljq8sm9bsz6k22as5ar3jw78644m-nix-2.8.1

to install a bootstrapping Nix in to the default profile

installing 'nix-2.8.1'
building '/nix/store/nm6hhmwxvyhyzdja261jp2y04kq4bmn1-user-environment.drv'...

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo HOME=/root /nix/store/6mjgljq8sm9bsz6k22as5ar3jw78644m-nix-2.8.1/bin/nix-env -i /nix/store/5qbx0wy48f55pybnii26428z24zh07ln-nss-cacert-3.66

to install a bootstrapping SSL certificate just for Nix in to the default profile

installing 'nss-cacert-3.66'
building '/nix/store/v1d4q2jiv4ypiv8xrzhpp5i34rs8cpyd-user-environment.drv'...

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo HOME=/root NIX_SSL_CERT_FILE=/nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt /nix/store/6mjgljq8sm9bsz6k22as5ar3jw78644m-nix-2.8.1/bin/nix-channel --update nixpkgs

to update the default channel in the default profile

unpacking channels...

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo install -m 0664 /tmp/tmp.aWWHvVK2Hk/nix.conf /etc/nix/nix.conf

to place the default nix daemon configuration (part 2)


~~> Setting up the nix-daemon systemd service

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo ln -sfn /nix/var/nix/profiles/default//lib/tmpfiles.d/nix-daemon.conf /etc/tmpfiles.d/nix-daemon.conf

to create the nix-daemon tmpfiles config


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo systemd-tmpfiles --create --prefix=/nix/var/nix

to run systemd-tmpfiles once to pick that path up


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo systemctl link /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.service

to set up the nix-daemon service

Created symlink /etc/systemd/system/nix-daemon.service → /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.service.

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo systemctl enable /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.socket

to set up the nix-daemon socket service

Created symlink /etc/systemd/system/sockets.target.wants/nix-daemon.socket → /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.socket.
Created symlink /etc/systemd/system/nix-daemon.socket → /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.socket.

---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo systemctl daemon-reload

to load the systemd unit for nix-daemon


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo systemctl start nix-daemon.socket

to start the nix-daemon.socket


---- sudo execution ------------------------------------------------------------
I am executing:

    $ sudo systemctl restart nix-daemon.service

to start the nix-daemon.service

Alright! We're done!
Try it! Open a new terminal, and type:

  $ nix-shell -p nix-info --run "nix-info -m"

Thank you for using this installer. If you have any feedback or need
help, don't hesitate:

You can open an issue at https://github.com/nixos/nix/issues

Or feel free to contact the team:
 - Matrix: #nix:nixos.org
 - IRC: in #nixos on irc.libera.chat
 - twitter: @nixos_org
 - forum: https://discourse.nixos.org

---- Reminders -----------------------------------------------------------------
[ 1 ]
Nix won't work in active shell sessions until you restart them.
S
```

## What was installed?

```
$ cat /etc/nix/nix.conf

build-users-group = nixbld
```

```
$ grep nix /etc/bash.bashrc
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
```

```
$ echo $PATH
/home/fmaste/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin
```

```
$ nix-build --dry-run '<nixpkgs>' -A google-chrome
error: Package ‘google-chrome-102.0.5005.61’ in /nix/store/vrkp5raqkgiaa3xs62i8pm53hc8qrg5s-nixpkgs/nixpkgs/pkgs/applications/networking/browsers/google-chrome/default.nix:162 has an unfree license (‘unfree’), refusing to evaluate.

       a) To temporarily allow unfree packages, you can use an environment variable
          for a single invocation of the nix tools.

            $ export NIXPKGS_ALLOW_UNFREE=1

        Note: For `nix shell`, `nix build`, `nix develop` or any other Nix 2.4+
        (Flake) command, `--impure` must be passed in order to read this
        environment variable.

       b) For `nixos-rebuild` you can set
         { nixpkgs.config.allowUnfree = true; }
       in configuration.nix to override this.

       Alternatively you can configure a predicate to allow specific packages:
         { nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
             "google-chrome"
           ];
         }

       c) For `nix-env`, `nix-build`, `nix-shell` or any other Nix command you can add
         { allowUnfree = true; }
       to ~/.config/nixpkgs/config.nix.
(use '--show-trace' to show detailed location information)
```

```
$ mkdir -p ~/.config/nix/
$ echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```
