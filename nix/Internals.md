# What was installed?

Remember I installed Nix in multi-user mode!

## On the /etc directory

### Settings

The main configuration (if there exists something like a main config in Nix) is
located on ```/etc/nix/nix.conf``` and by default only has one parameter with
the prefix used for the newly created unix user names and groups names. How are
this users and groups used? I guess we will find it later:

```console
$ cat /etc/nix/nix.conf

build-users-group = nixbld
```

By default Nix reads settings from the following places:
- The system-wide configuration file ```/nix/nix.conf```, or
  ```$NIX_CONF_DIR/nix.conf``` if ```NIX_CONF_DIR``` is set. ***Values loaded in
  this file are not forwarded to the Nix daemon. The client assumes that the
  daemon has already loaded them*** (What does this means? I should reload the
  daemon or not?).
- If ```NIX_USER_CONF_FILES``` is set, then each path separated by ```:``` will
  be loaded in reverse order. Otherwise it will look for ```nix/nix.conf```
  files in ```XDG_CONFIG_DIRS``` and ```XDG_CONFIG_HOME```. If unset,
  ```XDG_CONFIG_DIRS``` defaults to ```/etc/xdg```, and ```XDG_CONFIG_HOME```
  defaults to ```$HOME/.config``` as per
  [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html).
- If ```NIX_CONFIG``` is set, its contents is treated as the contents of a
  configuration file.

You can override settings on the command line using the ```--option``` flag,
e.g. ```--option keep-outputs false```. Every configuration setting also has a
corresponding command line flag, e.g. ```--max-jobs  16```; for Boolean
settings, there are two flags to enable or disable the setting (e.g.
```--keep-failed``` and ```--no-keep-failed```)

### Users and groups

The newly created 32 users:

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

The newly created 32 groups:

```console
$ grep nix /etc/group
nixbld:x:30000:nixbld1,nixbld2,nixbld3,nixbld4,nixbld5,nixbld6,nixbld7,nixbld8,nixbld9,nixbld10,nixbld11,nixbld12,nixbld13,nixbld14,nixbld15,nixbld16,nixbld17,nixbld18,nixbld19,nixbld20,nixbld21,nixbld22,nixbld23,nixbld24,nixbld25,nixbld26,nixbld27,nixbld28,nixbld29,nixbld30,nixbld31,nixbld32
```

### The Daemon managed with systemd

The Nix daemon is a required component in multi-user Nix installations. It
performs build actions and other operations on the Nix store on behalf of
non-root users. Usually you don’t run the daemon directly; instead it’s managed
by a service management framework such as systemd (see below).

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
```--daemon``` (multi-user). Nice to see that the nix-daemon appears to be part
of a recently build/installed nix package (the correct terminology is package or
expression?), where the installer may be using a binary "old" version as part of
a bootstrapping process, to compile the newest version:

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

### Definition of environment variables

Why this file is called ```nix-daemon.sh``` is a mystery to me. It's not how the
daemon is started.

If file ```nix-daemon.sh``` exists on ```/nix/var/nix/profiles/default/etc/profile.d/``` the script is called for both interactive and non-interactive
shells:

```console
$ grep nix /etc/bash.bashrc
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
```

Does the same thing on ```/etc/profile.d/*``` and ```zshrc```:

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

The script mainly sets the ```NIX_PROFILES``` and ```NIX_SSL_CERT_FILE```
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

## On the /root directory

Looks like the most interesting internal files are located on the /root
directory. Mostly the same files that must appear on every user's home that use
Nix:

```console
$ sudo ls -la /root/
...
drwxr-xr-x 1 root root     6 May 29 00:05 .cache
-rw-rw-r-- 1 root root    52 May 29 00:05 .nix-channels
drwxr-xr-x 1 root root    16 May 29 00:05 .nix-defexpr
lrwxrwxrwx 1 root root    29 May 29 00:05 .nix-profile -> /nix/var/nix/profiles/default
...
```

### Cache

As I read, everything Nix needs is on the top level ```/nix``` folder, but for
obvious performance reasons it's also using a sqlite database as cache. ***An
independent cache will also appear in non-root user's homes that interact with
nix***, I guess mostly because different users can have different
channels/repositories and applications/expressions installed/built
(@##!! terminology):

```console
$ sudo ls -la /root/.cache/nix/
-rw-r--r-- 1 root root 794624 May 29 08:45 binary-cache-v6.sqlite
-rw-r--r-- 1 root root      0 May 29 08:45 binary-cache-v6.sqlite-journal
-rw-r--r-- 1 root root  12288 May 29 00:05 fetcher-cache-v1.sqlite
-rw-r--r-- 1 root root      0 May 29 00:05 fetcher-cache-v1.sqlite-journal
```

### Channels

The two channel related files and folders (respectively) are:
- ```.nix-channels```: The Nix channel sources managed by ```nix-channel```.
- ```.nix-defexpr```: For ```nix-env``` to manage and use multiple channels.

#### The .nix-channels file

Here you can find the actual channels, that can be managed with the
```nix-channel``` command. Channels are like repositories, is a mechanism that
allows you to automatically stay up-to-date with a set of pre-built Nix
expressions. A Nix channel is just a URL that points to a place containing a set
of Nix expressions.

##### Channel format

A channel is an URL that should point to a directory containing the following:
- A ```nixexprs.tar.xz``` file:
  - A tarball containing Nix expressions and files referenced by them (such as
  build scripts and patches). At the top level, the tarball should contain a
  single directory. That directory must contain a file default.nix that serves
  as the channel’s “entry point”

See [The official channels](https://nixos.wiki/wiki/Nix_channels#The_official_channels) for a short description of the available
channels. ***But I still couldn't add a channel different than "unstable"***.

##### Default root channel

***I did not find an specific mentions in the documentation, but this installed
channel even thou it appears to be for the root user only, it is used by
non-root users until the users add channels and overrides it!***

The default/pre-installed channel is usually named ```nixpkgs```, this is
because channels are added with an alias. You are going to see this alias a lot
used as a prefix (or it was a suffix?) when running nix commands to signal which
channel we are trying to use:

```console
$ sudo cat /root/.nix-channels
https://nixos.org/channels/nixpkgs-unstable nixpkgs
```

As we will see later, channel versions are managed in ```.nix-defexpr/``` 
internally using ```nix-env```, let's only look at the symlinks created:

```console
$ ls -la /nix/var/nix/profiles/per-user/root/channels/
lrwxrwxrwx 1 root root        60 Jan  1  1970 manifest.nix -> /nix/store/yx3y97fmx221jqr3nlxl6is388zy8l0b-env-manifest.nix
lrwxrwxrwx 1 root root        59 Jan  1  1970 nixpkgs -> /nix/store/vrkp5raqkgiaa3xs62i8pm53hc8qrg5s-nixpkgs/nixpkgs
```

The manifest is a list containing only one element named ```nixpkgs```:

```console
$ cat /nix/var/nix/profiles/per-user/root/channels/manifest.nix
[ { meta = { }; name = "nixpkgs"; out = { outPath = "/nix/store/vrkp5raqkgiaa3xs62i8pm53hc8qrg5s-nixpkgs"; }; outPath = "/nix/store/vrkp5raqkgiaa3xs62i8pm53hc8qrg5s-nixpkgs"; outputs = [ "out" ]; system = "builtin"; type = "derivation"; } ]
```

Looks like a ```git clone``` of the nix packages/expressions repository. We will
add a new channel next to see how it works:

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

##### Adding a channel

The user starts with no channels, the ```.nix-channels``` file does not even
exists:

```console
$ ls ~/.nix-*
/home/fmaste/.nix-defexpr:
channels  channels_root

/home/fmaste/.nix-profile:
```

Let's add a channel:

```console
$ nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
```

```console
$ ls ~/.nix-*
/home/fmaste/.nix-channels

/home/fmaste/.nix-defexpr:
channels  channels_root

/home/fmaste/.nix-profile:
```

```console
$ cat ~/.nix-channels
https://nixos.org/channels/nixpkgs-unstable nixpkgs
```

```console
$ nix-channel --list
nixpkgs https://nixos.org/channels/nixpkgs-unstable
```

Channel was added but nothing was downloaded:

```console
$ ls -la /nix/var/nix/profiles/per-user/fmaste/channels/
lrwxrwxrwx 1 root root        60 Jan  1  1970 manifest.nix -> /nix/store/w213fg5zb7df8h1wsldqpqi7yk9d80rk-env-manifest.nix
```

Let's update the new channel:

```console
$ nix-channel --update
unpacking channels...
```

```console
$ ls -la /nix/var/nix/profiles/per-user/fmaste/channels/
lrwxrwxrwx 1 root root        60 Jan  1  1970 manifest.nix -> /nix/store/zzfpx72f3w2h377drbzr3fjpkg2214l5-env-manifest.nix
lrwxrwxrwx 1 root root        59 Jan  1  1970 nixpkgs -> /nix/store/x1w5yls3p126bk9bj4j8lb6mf9qzm9qn-nixpkgs/nixpkgs
```

Now you can see that some content fetched from
```https://nixos.org/channels/nixpkgs-unstable``` was dumped:

```console
$ ls -la /nix/var/nix/profiles/per-user/fmaste/channels/nixpkgs/
-r--r--r-- 1 root root 7199 Jan  1  1970 CONTRIBUTING.md
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
dr-xr-xr-x 1 root root  672 Jan  1  1970 lib
dr-xr-xr-x 1 root root   78 Jan  1  1970 maintainers
dr-xr-xr-x 1 root root  202 Jan  1  1970 nixos
dr-xr-xr-x 1 root root  254 Jan  1  1970 pkgs
-r--r--r-- 1 root root 6189 Jan  1  1970 README.md
-r--r--r-- 1 root root   19 Jan  1  1970 svn-revision
-r--r--r-- 1 root root    5 Jan  1  1970 .version
-r--r--r-- 1 root root   21 Jan  1  1970 .version-suffix
```

#### The .nix-defexpr/ folder

```root/.nix-defexpr/``` or ```~/.nix-defexpr/``` contains
```.nix-defexpr/channels``` as a symlink to
```/nix/var/nix/profiles/per-user/{username}/channels```. It ensures that
```nix-env``` can find your channels. In a multi-user installation, you also
have  ```.nix-defexpr/channels_root``` on the non-root home, which links to the
channels of the root user.

***The non-root user, can see/use the channels of the root user? It looks like
yes!***

```console
$ sudo ls -la /root/.nix-defexpr
lrwxrwxrwx 1 root root  44 May 29 00:05 channels -> /nix/var/nix/profiles/per-user/root/channels
```

```console
$ ls -la ~/.nix-defexpr/
lrwxrwxrwx 1 fmaste fmaste   46 May 29 00:40 channels -> /nix/var/nix/profiles/per-user/fmaste/channels
lrwxrwxrwx 1 fmaste fmaste   44 May 29 00:40 channels_root -> /nix/var/nix/profiles/per-user/root/channels
```

```nix-channel``` uses a nix-env profile to keep track of previous versions of
the subscribed channels. Every time you run ```nix-channel --update```, a new
channel generation (that is, a symlink to the channel Nix expressions in the Nix
store) is created. This enables ```nix-channel --rollback``` to revert to
previous versions.

What does this means? That ```nix-channel``` is using the same "magic sauce" it
uses to build applications and maintain different versions to keep track of
the different channel versions.

### Profiles

[Profile](https://nixos.org/manual/nix/stable/glossary.html#gloss-profile):
> A symlink to the current user environment of a user, e.g.,
> ```/nix/var/nix/profiles/default```.

[User environment](https://nixos.org/manual/nix/stable/glossary.html#gloss-user-env)
> An automatically generated store object that consists of a set of symlinks to
> “active” applications, i.e., other store paths. These are generated
> automatically by ```nix-env```.

## For my user only

### Environment variables

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
