# What was installed?

Remember I installed multi-user!

## On /etc

The main configuration (if there exists something like a main config in Nix) is
located on ```/etc/nix/nix.conf``` and only has one parameter with the prefix
used for the newly created unix user names and groups names. How are this users
and groups used? I guess we will find it later.

```console
$ cat /etc/nix/nix.conf

build-users-group = nixbld
```

If file ```nix-daemon.sh``` exists on ```/nix/var/nix/profiles/default/etc/profile.d/``` call the script:

```console
$ grep nix /etc/bash.bashrc
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
```

Does the same thing for ```/etc/profile.d/*``` and ```zshrc```:

```console
$ cat /etc/profile.d/nix.sh

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

```

```console
$ cat /etc/zshrc

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix
```

The scripts mostly sets the ```NIX_PROFILES``` and ```NIX_SSL_CERT_FILE```
environment variables:

```console
$ cat /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
# Only execute this file once per shell.
if [ -n "${__ETC_PROFILE_NIX_SOURCED:-}" ]; then return; fi
__ETC_PROFILE_NIX_SOURCED=1

export NIX_PROFILES="/nix/var/nix/profiles/default $HOME/.nix-profile"

# Set $NIX_SSL_CERT_FILE so that Nixpkgs applications like curl work.
if [ -n "${NIX_SSL_CERT_FILE:-}" ]; then
    : # Allow users to override the NIX_SSL_CERT_FILE
elif [ -e /etc/ssl/certs/ca-certificates.crt ]; then # NixOS, Ubuntu, Debian, Gentoo, Arch
    export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt
elif [ -e /etc/ssl/ca-bundle.pem ]; then # openSUSE Tumbleweed
    export NIX_SSL_CERT_FILE=/etc/ssl/ca-bundle.pem
elif [ -e /etc/ssl/certs/ca-bundle.crt ]; then # Old NixOS
    export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
elif [ -e /etc/pki/tls/certs/ca-bundle.crt ]; then # Fedora, CentOS
    export NIX_SSL_CERT_FILE=/etc/pki/tls/certs/ca-bundle.crt
else
  # Fall back to what is in the nix profiles, favouring whatever is defined last.
  check_nix_profiles() {
    if [ -n "$ZSH_VERSION" ]; then
      # Zsh by default doesn't split words in unquoted parameter expansion.
      # Set local_options for these options to be reverted at the end of the function
      # and shwordsplit to force splitting words in $NIX_PROFILES below.
      setopt local_options shwordsplit
    fi
    for i in $NIX_PROFILES; do
      if [ -e "$i/etc/ssl/certs/ca-bundle.crt" ]; then
        export NIX_SSL_CERT_FILE=$i/etc/ssl/certs/ca-bundle.crt
      fi
    done
  }
  check_nix_profiles
  unset -f check_nix_profiles
fi

export PATH="$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:$PATH"
```

### Users and groups

The newly created users:

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

The newly created groups:

```console
$ grep nix /etc/group
nixbld:x:30000:nixbld1,nixbld2,nixbld3,nixbld4,nixbld5,nixbld6,nixbld7,nixbld8,nixbld9,nixbld10,nixbld11,nixbld12,nixbld13,nixbld14,nixbld15,nixbld16,nixbld17,nixbld18,nixbld19,nixbld20,nixbld21,nixbld22,nixbld23,nixbld24,nixbld25,nixbld26,nixbld27,nixbld28,nixbld29,nixbld30,nixbld31,nixbld32
```

### Systemd

Let's look for the systemd services enabled during the installation:

```console
$ ls -la /etc/systemd/system/nix*
lrwxrwxrwx 1 root root 67 May 29 00:05 /etc/systemd/system/nix-daemon.service -> /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.service
lrwxrwxrwx 1 root root 66 May 29 00:05 /etc/systemd/system/nix-daemon.socket -> /nix/var/nix/profiles/default/lib/systemd/system/nix-daemon.socket
```

One starts the socket that I guess Nix commands use to communicate with the Nix
service when running as non-root:

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

The other one starts the ```nix-daemon```, remember I installed with
```--daemon```. Nice to see that the nix-daemon appears to be part of a recently
build/installed nix package, where the installer may be using a binary "old"
version as part of a bootstrapping process, to compile the newest version:

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

Looks like the most interesting configuration files are located on the /root
directory:

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

As I read everything Nix needs is on the top level ```/nix``` folder, but for
obvious performance reasons it's also using a sqlite database as cache:

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

Here you can find the actual channels, that can be managed with the
```nix-channel``` command. Channels are like repositories, so you don't have to
download and update package's expressions one by one. The default channel is
```nixpkgs``` and you are going to see it a lot as a prefix (or maybe suffix?)
when running nix commands:

```console
$ sudo cat /root/.nix-channels
https://nixos.org/channels/nixpkgs-unstable nixpkgs
```

Who know what this is? I don't:

```console
$ sudo ls -la /root/.nix-defexpr
total 4
drwxr-xr-x 1 root root  16 May 29 00:05 .
drwx------ 1 root root 222 May 29 00:05 ..
lrwxrwxrwx 1 root root  44 May 29 00:05 channels -> /nix/var/nix/profiles/per-user/root/channels
```

```console
$ ls -la /nix/var/nix/profiles/per-user/root/channels/
total 8
dr-xr-xr-x 1 root root        38 Jan  1  1970 .
drwxrwxr-t 1 root nixbld 1186080 May 29 10:09 ..
lrwxrwxrwx 1 root root        60 Jan  1  1970 manifest.nix -> /nix/store/yx3y97fmx221jqr3nlxl6is388zy8l0b-env-manifest.nix
lrwxrwxrwx 1 root root        59 Jan  1  1970 nixpkgs -> /nix/store/vrkp5raqkgiaa3xs62i8pm53hc8qrg5s-nixpkgs/nixpkgs
```

Looks like a ```git clone``` of the nix packages/expressions repository:

```console
$ ls -la /nix/var/nix/profiles/per-user/root/channels/nixpkgs/
total 60
dr-xr-xr-x 1 root root  382 Jan  1  1970 .
dr-xr-xr-x 1 root root   14 Jan  1  1970 ..
-r--r--r-- 1 root root 6977 Jan  1  1970 CONTRIBUTING.md
-r--r--r-- 1 root root 1097 Jan  1  1970 COPYING
-r--r--r-- 1 root root  971 Jan  1  1970 default.nix
dr-xr-xr-x 1 root root  430 Jan  1  1970 doc
-r--r--r-- 1 root root 2122 Jan  1  1970 .editorconfig
-r--r--r-- 1 root root 1359 Jan  1  1970 flake.nix
-r--r--r-- 1 root root  598 Jan  1  1970 .gitattributes
-r--r--r-- 1 root root 1108 Jan  1  1970 .git-blame-ignore-revs
dr-xr-xr-x 1 root root  212 Jan  1  1970 .github
-r--r--r-- 1 root root  425 Jan  1  1970 .gitignore
-r--r--r-- 1 root root   40 Jan  1  1970 .git-revision
dr-xr-xr-x 1 root root  640 Jan  1  1970 lib
dr-xr-xr-x 1 root root   78 Jan  1  1970 maintainers
dr-xr-xr-x 1 root root  202 Jan  1  1970 nixos
dr-xr-xr-x 1 root root  254 Jan  1  1970 pkgs
-r--r--r-- 1 root root 6189 Jan  1  1970 README.md
-r--r--r-- 1 root root   19 Jan  1  1970 svn-revision
-r--r--r-- 1 root root    5 Jan  1  1970 .version
-r--r--r-- 1 root root   21 Jan  1  1970 .version-suffix
```

```console
$ cat /nix/var/nix/profiles/per-user/root/channels/manifest.nix
[ { meta = { }; name = "nixpkgs"; out = { outPath = "/nix/store/vrkp5raqkgiaa3xs62i8pm53hc8qrg5s-nixpkgs"; }; outPath = "/nix/store/vrkp5raqkgiaa3xs62i8pm53hc8qrg5s-nixpkgs"; outputs = [ "out" ]; system = "builtin"; type = "derivation"; } ]
```

## On my user

### Envs

After restating my console/terminal I can see the updated ```$PATH```:

```console
$ echo $PATH
/home/fmaste/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games
```

And the actual/active profiles on ```$NIX_PROFILES```:

```console
$ echo $NIX_PROFILES
/nix/var/nix/profiles/default /home/fmaste/.nix-profile
```

When a new profile is created this link is updated:

```console
$ ls -la /nix/var/nix/profiles/default
lrwxrwxrwx 1 root root 14 May 29 00:05 /nix/var/nix/profiles/default -> default-2-link
```

### On the user home directory

The user's ```.nix-profile``` is a link to the top level per-user profiles:

```console
$ ls -la /home/fmaste/.nix-profile
lrwxrwxrwx 1 fmaste fmaste 45 May 29 02:44 /home/fmaste/.nix-profile -> /nix/var/nix/profiles/per-user/fmaste/profile
```

This file didn't exist, I created it to enable ```flakes```. More on that later
(if I manage to fully understand it and explain it):

```console
$ mkdir -p ~/.config/nix/
$ echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

# TODO

Unfree? WTF?:

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
