# What was installed?

Remember I installed multi-user!

## On /etc

```console
$ cat /etc/nix/nix.conf

build-users-group = nixbld
```

```console
$ grep nix /etc/bash.bashrc
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
```

```console
$ cat /etc/profile.d/nix.sh

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

```

### Users and groups

```console
$ sudo grep nix /etc/shadow
nixbld1:!:19141::::::
nixbld2:!:19141::::::
nixbld3:!:19141::::::
nixbld4:!:19141::::::
nixbld5:!:19141::::::
nixbld6:!:19141::::::
nixbld7:!:19141::::::
nixbld8:!:19141::::::
nixbld9:!:19141::::::
nixbld10:!:19141::::::
nixbld11:!:19141::::::
nixbld12:!:19141::::::
nixbld13:!:19141::::::
nixbld14:!:19141::::::
nixbld15:!:19141::::::
nixbld16:!:19141::::::
nixbld17:!:19141::::::
nixbld18:!:19141::::::
nixbld19:!:19141::::::
nixbld20:!:19141::::::
nixbld21:!:19141::::::
nixbld22:!:19141::::::
nixbld23:!:19141::::::
nixbld24:!:19141::::::
nixbld25:!:19141::::::
nixbld26:!:19141::::::
nixbld27:!:19141::::::
nixbld28:!:19141::::::
nixbld29:!:19141::::::
nixbld30:!:19141::::::
nixbld31:!:19141::::::
nixbld32:!:19141::::::
```

```console
$ grep nix /etc/group
nixbld:x:30000:nixbld1,nixbld2,nixbld3,nixbld4,nixbld5,nixbld6,nixbld7,nixbld8,nixbld9,nixbld10,nixbld11,nixbld12,nixbld13,nixbld14,nixbld15,nixbld16,nixbld17,nixbld18,nixbld19,nixbld20,nixbld21,nixbld22,nixbld23,nixbld24,nixbld25,nixbld26,nixbld27,nixbld28,nixbld29,nixbld30,nixbld31,nixbld32
```

### Systemd

```console
$ ls -la /etc/systemd/system/nix*
lrwxrwxrwx 1 root root 67 May 29 00:05 /etc/systemd/system/nix-daemon.service -> /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.service
lrwxrwxrwx 1 root root 66 May 29 00:05 /etc/systemd/system/nix-daemon.socket -> /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.socket
```

```console
$ cat /etc/systemd/system/nix-daemon.socket
[Unit]
Description=Nix Daemon Socket
Before=multi-user.target
RequiresMountsFor=/nix/store
ConditionPathIsReadWrite=/nix/var/nix/daemon-socket

[Socket]
ListenStream=/nix/var/nix/daemon-socket/socket

[Install]
WantedBy=sockets.target
```

```console
$ cat /etc/systemd/system/nix-daemon.service
[Unit]
Description=Nix Daemon
Documentation=man:nix-daemon https://nixos.org/manual
RequiresMountsFor=/nix/store
RequiresMountsFor=/nix/var
RequiresMountsFor=/nix/var/nix/db
ConditionPathIsReadWrite=/nix/var/nix/daemon-socket

[Service]
ExecStart=@/nix/store/6mjgljq8sm9bsz6k22as5ar3jw78644m-nix-2.8.1/bin/nix-daemon nix-daemon --daemon
KillMode=process

[Install]
WantedBy=multi-user.target
```

## On /root directory

```console
$ sudo ls -la /root/
total 36
...
drwx------ 1 root root   222 May 29 00:05 .
drwxr-xr-x 1 root root   198 May 29 00:05 ..
drwxr-xr-x 1 root root     6 May 29 00:05 .cache
-rw-rw-r-- 1 root root    52 May 29 00:05 .nix-channels
drwxr-xr-x 1 root root    16 May 29 00:05 .nix-defexpr
lrwxrwxrwx 1 root root    29 May 29 00:05 .nix-profile -> /nix/var/nix/profiles/default
...
```

```console
$ sudo ls -la /root/.cache/nix/
total 788
drwxr-xr-x 1 root root    212 May 29 00:05 .
drwxr-xr-x 1 root root      6 May 29 00:05 ..
-rw-r--r-- 1 root root 794624 May 29 08:45 binary-cache-v6.sqlite
-rw-r--r-- 1 root root      0 May 29 08:45 binary-cache-v6.sqlite-journal
-rw-r--r-- 1 root root  12288 May 29 00:05 fetcher-cache-v1.sqlite
-rw-r--r-- 1 root root      0 May 29 00:05 fetcher-cache-v1.sqlite-journal
```

```console
$ sudo cat /root/.nix-channels
https://nixos.org/channels/nixpkgs-unstable nixpkgs
```

```console
$ sudo ls -la /root/.nix-defexpr
total 4
drwxr-xr-x 1 root root  16 May 29 00:05 .
drwx------ 1 root root 222 May 29 00:05 ..
lrwxrwxrwx 1 root root  44 May 29 00:05 channels -> /nix/var/nix/profiles/per-user/root/channels
```

## On my user

### Envs

```console
$ echo $NIX_PROFILES 
/nix/var/nix/profiles/default /home/fmaste/.nix-profile
```

```console
$ ls -la /nix/var/nix/profiles/default
lrwxrwxrwx 1 root root 14 May 29 00:05 /nix/var/nix/profiles/default -> default-2-link
```

```console
$ ls -la /home/fmaste/.nix-profile
lrwxrwxrwx 1 fmaste fmaste 45 May 29 02:44 /home/fmaste/.nix-profile -> /nix/var/nix/profiles/per-user/fmaste/profile
```

### On the user home directory

```console
$ echo $PATH
/home/fmaste/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games
```

```console
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
