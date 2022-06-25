# [Nix](https://nixos.org/)

## TL;DR;

Nix is a purely functional software package manager.

## But it's much more than that

[The ecosystem](https://nixos.wiki/wiki/Nix_Ecosystem): Nixpkgs, NixOS, NixOps,
Hydra.
![Nix stack](https://nixos.wiki/images/4/44/Nixos-stack2.png)

## Why?

Unlike most package managers Nix installs packages independently of the global
state of the system, this means that an specific combination of packages can be
installed identically in different hosts creating reproducible environments.

If you have never experienced ["DLL hell"](https://en.wikipedia.org/wiki/DLL_Hell)
or needed to share a common environment between setups or colleagues then this
"guide" is not for you, go watch one random Netflix series only to realize
after 160 days watching TV non-stop that it was not good like the other 20 you
have already watched.

## How?

Nix packages are descriptions on how to build and install software components
that are written following an specification in a very powerful special purpose
language.

Nix takes care of following the necessary steps in a way that the output is
reproducible (a kind of ```make install``` sandbox). Every package is installed
in the Nix store using an unique path of the form ```/nix/store/$HASH-$NAME```
so all dependencies can use an absolute path.

For example, here is the location of an installed version of ```sqlite```:
```console
$ ls -la /nix/store/p3d1pgw1kwd328zn1vgsqac01b6q11ya-sqlite-3.38.5-bin/bin/
-r-xr-xr-x 1 root root 1649464 Jan  1  1970 sqlite3
```

And Nix takes care of relating dependencies together or managing user profiles:

```console
$ env | grep nix
PATH=...:/home/fmaste/.nix-profile/bin:...
```

```console
$ which sqlite3
/home/fmaste/.nix-profile/bin/sqlite3
```

```console
$ ls -la /home/fmaste/.nix-profile/bin/
lrwxrwxrwx 1 root root 73 Jan  1  1970 sqlite3 -> /nix/store/p3d1pgw1kwd328zn1vgsqac01b6q11ya-sqlite-3.38.5-bin/bin/sqlite3
```

## Main Features

### Reproducible

> ***Nix builds packages in isolation from each other***. This ensures that they
> ***are reproducible and don't have undeclared dependencies***, so if a package
> works on one machine, it will also work on another.

### Declarative

> Nix makes it trivial to ***share development and build environments*** for
> your projects, regardless of what programming languages and tools you're
> using.

### Reliable

> Nix ensures that ***installing or upgrading one package cannot break other
> packages***. It allows you to ***roll back to previous versions***, and
> ensures that ***no package is in an inconsistent state*** during an upgrade.

### Packages and NixOS

> [Nixpkgs](https://github.com/nixos/nixpkgs) is a ***collection of over 80,000
> software packages*** that can be installed with the Nix package manager. It
> also implements ***NixOS, a purely-functional Linux distribution***.

## 10,000 ft view

A software package is a bunch of different things to build and install in a way
that allows to later be able to use it.

A Nix software package contains ***a description of the build and install
process in a function from inputs to outputs written in a language created for
this specific purpose***, the Nix expression language.

The ***inputs are other expressions also written in Nix***, mostly Nix libraries
passed as arguments that are involved in building the package like a library
to fetch code using ```git``` or dependencies needed at runtime.

\* A Nix file contains a Nix language expression that can have no inputs, just a
value, but you know how we are told that everything is a function.

Per Nix's interface the ***output is a well-known list of key/value pairs that
contains other Nix packages, the build script and command lines, environment
variables for the build script, destination directory, etc***. Nix tries very
hard to ensure that Nix expressions are deterministic: evaluating a Nix
expression with the same inputs twice should yield the same result.

\* If you are curious reader, the big ***if*** in this last sentence is solved
with what Nix calls Flakes.

After running, or evaluating, this "description program(s)" Nix ***obtains a
precise description of everything it needs to build the package's software***.
With the output of this description Nix creates something akin to a sandbox
description file with specific dependencies and the build process to run inside
it. Almost like [Flatpak](https://flatpak.org/) or
[snaps](https://snapcraft.io/). The big difference is that Nix's expression
language is very expressive and powerful and is also used for other Nix stuff
besides building software, more on this later.

The secret sauce is that this sandbox is linked with an specific version of each
dependency and are ***always the same dependencies each time the software is
run***. From an administrator's point of view: if you want an old PHP version
for one application, but want to upgrade the rest of the system, that's not
painful any more.

There are no upgrade/downgrade scripts for your packages. It doesn't make sense
with this approach, because there's nothing to be upgraded. With Nix you switch
to using other software with its own of dependencies, but there's no formal
notion of upgrade or downgrade when doing so.

\* If there is a data format change, then migrating to the new data format
remains your own responsibility. Of course!

### Again but using Nix's terminology

<!-- The Nix language is used to describe such derivations. -->

To deploy software using Nix you must write ***Nix expressions*** that
describe how to build ***packages***. Nix expressions are written using the
***Nix expression language***.

This high-level description of software packages are ***instantiated*** into
what Nix calls a ***derivation***. In this step Nix evaluates code to resolve
the ***closures*** of a derivation, the list of all its dependencies including
absolutely everything necessary to use that derivation.

The results of derivations are stored in a ```.drv``` file in the
***Nix store*** (typically ```/nix/store```) and describes how to properly built
the package in a reproducible way.

<!--
Nix with all the knowledge it obtained about a package creates a sandbox to
build software that only has the dependencies 
-->

### Now explain it with examples using nix commands

The [Nix language](https://nixos.org/manual/nix/stable/expressions/expression-language.html)
is used to write expressions that when evaluated produce derivations:

```console
$ cat hello-world.nix
{ pkgs ? import <nixpkgs> {} }:
pkgs.stdenv.mkDerivation {
        name = "hello";
        buildCommand = "echo 'Hello world!' > $out";
}
```

[```nix-instantiate```](https://nixos.org/manual/nix/stable/command-ref/nix-instantiate.html)
parses and evaluates the ```.nix``` files and returns .drv files corresponding
to the parsed derivation set:

```console
$ nix-instantiate hello-world.nix
warning: you did not specify '--add-root'; the result might be removed by the garbage collector
/nix/store/q6h8xmwk2wmdn7fcsj4llcsi6xnzhn7d-hello.drv
```

[```nix show-derivation```](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-show-derivation.html)
shows the contents of a store derivation:

```console
$ nix show-derivation /nix/store/q6h8xmwk2wmdn7fcsj4llcsi6xnzhn7d-hello.drv
{
  "/nix/store/q6h8xmwk2wmdn7fcsj4llcsi6xnzhn7d-hello.drv": {
    "outputs": {
      "out": {
        "path": "/nix/store/2h3znnvdncxyc6pwslr2dsi6c4hg601b-hello"
      }
    },
    "inputSrcs": [
      "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
    ],
    "inputDrvs": {
      "/nix/store/42pr7zqjf0y29v19q1wxn6hs5gdl5car-bash-5.1-p16.drv": [
        "out"
      ],
      "/nix/store/ddmyhp06jqy8bxj715zwsmbcnzvx8iax-stdenv-linux.drv": [
        "out"
      ]
    },
    "system": "x86_64-linux",
    "builder": "/nix/store/0d3wgx8x6dxdb2cpnq105z23hah07z7l-bash-5.1-p16/bin/bash",
    "args": [
      "-e",
      "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
    ],
    "env": {
      "buildCommand": "echo 'Hello world!' > $out",
      "buildInputs": "",
      "builder": "/nix/store/0d3wgx8x6dxdb2cpnq105z23hah07z7l-bash-5.1-p16/bin/bash",
      "configureFlags": "",
      "depsBuildBuild": "",
      "depsBuildBuildPropagated": "",
      "depsBuildTarget": "",
      "depsBuildTargetPropagated": "",
      "depsHostHost": "",
      "depsHostHostPropagated": "",
      "depsTargetTarget": "",
      "depsTargetTargetPropagated": "",
      "doCheck": "",
      "doInstallCheck": "",
      "name": "hello",
      "nativeBuildInputs": "",
      "out": "/nix/store/2h3znnvdncxyc6pwslr2dsi6c4hg601b-hello",
      "outputs": "out",
      "patches": "",
      "propagatedBuildInputs": "",
      "propagatedNativeBuildInputs": "",
      "stdenv": "/nix/store/28hqpbwpzvpff7ldbhxdhzcpdc34lgsa-stdenv-linux",
      "strictDeps": "",
      "system": "x86_64-linux"
    }
  }
}
```

[```nix-store```](https://nixos.org/manual/nix/stable/command-ref/nix-store.html)
realizes the ```.drv``` file, which actually builds it. Manipulates or queries
the Nix store:

```console
$ nix-store --realise /nix/store/q6h8xmwk2wmdn7fcsj4llcsi6xnzhn7d-hello.drv
this derivation will be built:
  /nix/store/q6h8xmwk2wmdn7fcsj4llcsi6xnzhn7d-hello.drv
building '/nix/store/q6h8xmwk2wmdn7fcsj4llcsi6xnzhn7d-hello.drv'...
warning: you did not specify '--add-root'; the result might be removed by the garbage collector
/nix/store/2h3znnvdncxyc6pwslr2dsi6c4hg601b-hello
```

Excellent:

```console
$ cat /nix/store/2h3znnvdncxyc6pwslr2dsi6c4hg601b-hello
Hello world!
```

This was a little more detailed process, you can use the
[nix-build](https://nixos.org/manual/nix/stable/command-ref/nix-build.html)
command to build derivations.

### ELIGR (Explain me Like I'm a Golden Retriever)

You install Nix, do ...

## Background

> Existing systems for software deployment are neither safe nor sufficiently
> flexible. Primary safety issues are the inability to enforce reliable
> specification of component dependencies, and the lack of support for multiple
> versions or variants of a component. This renders deployment operations such
> as upgrading or deleting components dangerous and unpredictable. A deployment
> system must also be flexible (i.e., policy-free) enough to support both
> centralised and local package management, and to allow a variety of mechanisms
> for transferring components. In this paper we present Nix, a deployment system
> that addresses these issues through a simple technique of using cryptographic
> hashes to compute unique paths for component instances.
>
> [Nix: A Safe and Policy-Free System for Software Deployment](https://raw.githubusercontent.com/edolstra/edolstra.github.io/master/pubs/nspfssd-lisa2004-final.pdf)

> Existing package and system configuration management tools suffer from an
> imperative model, where system administration actions such as upgrading
> packages or changes to system configuration files are stateful: they
> destructively update the state of the system. This leads to many problems,
> such as the inability to roll back changes easily, to deploy multiple versions
> of a package side-by-side, to reproduce a configuration deterministically on
> another machine, or to reliably upgrade a system. In this article we show that
> we can overcome these problems by moving to a purely functional system
> configuration model. This means that all static parts of a system (such as
> software packages, configuration files and system startup scripts) are built
> by pure functions and are immutable, stored in a way analogously to a heap in
> a purely functional language. We have implemented this model in NixOS, a
> non-trivial Linux distribution that uses the Nix package manager to build the
> entire system configuration from a modular, purely functional specification
>
> [NixOS: A Purely Functional Linux Distribution](https://raw.githubusercontent.com/edolstra/edolstra.github.io/master/pubs/nixos-icfp2008-final.pdf)

## Other features

### The best of functional programming

The Nix Expression Language is a dynamic, functional and lazy domain-specific language used to instruct Nix how to build packages.
https://nixos.org/manual/nix/stable/expressions/expression-language.html

### Multiple versions

Multiple versions

### No dependency resolution

> There isn't anything like apt which solves a SAT problem in order to satisfy
> dependencies with lower and upper bounds on versions. There's no need for this
> because all the dependencies are static: if a derivation X depends on a
> derivation Y, then it always depends on it. A version of X which depended on Z
> would be a different derivation.

### Complete dependencies

Complete dependencies

### Multi-user support

Multi-user support

### Atomic upgrades and rollbacks

Atomic upgrades and rollbacks

### Nix Packages collection

Nix Packages collection

#### Channels

Channels

### Managing build environments

Managing build environments

### NixOS

NixOS

# Minimum Concepts

The core of a Nix system is the Nix store, usually installed under
```/nix/store```, and some tools to manipulate the store. In Nix there is the
notion of a derivation rather than a package. The difference can be subtle at
the beginning, so I will often use the words interchangeably.

A package/expression

[Profile](https://nixos.org/manual/nix/stable/glossary.html#gloss-profile):
> A symlink to the current user environment of a user, e.g.,
> ```/nix/var/nix/profiles/default```.

[User environment](https://nixos.org/manual/nix/stable/glossary.html#gloss-user-env)
> An automatically generated store object that consists of a set of symlinks to
> “active” applications, i.e., other store paths. These are generated
> automatically by ```nix-env```.

[Nix expression](https://nixos.org/manual/nix/stable/glossary.html#gloss-nix-expression)
> A high-level description of software packages and compositions thereof.
> Deploying software using Nix entails writing Nix expressions for your
> packages. Nix expressions are translated to derivations that are stored in the
> Nix store. These derivations can then be built.

[Derivation](https://nixos.org/manual/nix/stable/glossary.html#gloss-derivation)
> A description of a build action. The result of a derivation is a store object.
> Derivations are typically specified in Nix expressions using the derivation
> primitive. These are translated into low-level store derivations (implicitly
> by ```nix-env``` and ```nix-build```, or explicitly by ```nix-instantiate```).

[Store](https://nixos.org/manual/nix/stable/glossary.html#gloss-store)
> The location in the file system where store objects live. Typically
> ```/nix/store```.

[Store path](https://nixos.org/manual/nix/stable/glossary.html#gloss-store-path)
> The location in the file system of a store object, i.e., an immediate child of
> the Nix store directory.

[Store object](https://nixos.org/manual/nix/stable/glossary.html#gloss-store-object)
> A file that is an immediate child of the Nix store directory. These can be
> regular files, but also entire directory trees. Store objects can be sources
> (objects copied from outside of the store), derivation outputs (objects
> produced by running a build action), or derivations (files describing a build
> action).

A shell

A channel

# Further Reading

- [Home](https://nixos.org/)
- [Wikipedia](https://en.wikipedia.org/wiki/Nix_(package_manager))
- Learn
  - [Install Nix](https://nixos.org/download.html#download-nix)
  - [First steps with Nix](https://nixos.org/guides/ad-hoc-developer-environments.html)
  - [How Nix works](https://nixos.org/guides/how-nix-works.html)
- [Manuals](https://nixos.org/learn.html):
  - [Nix Manual](https://nixos.org/manual/nix/stable/)
    - Nix is a package manager which comes in a form of many command line tools.
      Packages that Nix can build are defined with the Nix Expression Language.
  - [Nixpkgs Manual](https://nixos.org/manual/nixpkgs/stable)
    - The Nix Packages collection (Nixpkgs) is a set of thousands of packages
      for the Nix package manager and NixOS Linux distribution.
  - [NixOS Manual](https://nixos.org/manual/nixos/stable)
    - NixOS is a Linux distribution based on Nix package manager.
- Tutorials
  - [Nix Pills](https://nixos.org/guides/nix-pills)
    - A low-level tutorial on building software packages with Nix, showing in
      detail how nixpkgs is constructed.
- Others
  - [Unofficial Wiki](https://nixos.wiki/)
    - A user-maintained wiki for Nix and NixOS.
  - [nix.dev](https://nix.dev/)
    - An unofficial and opinionated guide for developers getting things done
      using the Nix ecosystem.
- Blog posts
  - https://serokell.io/blog/what-is-nix
  - https://serokell.io/blog/practical-nix-flakes
  - https://nix.dev/recommended-reading
  - https://nixos.org/guides/nix-pills/
  - https://ianthehenry.com/posts/how-to-learn-nix/
  - Flakes
    - https://nixos.wiki/wiki/Flakes
    - Part 1: https://www.tweag.io/blog/2020-05-25-flakes/
    - Part 2: https://www.tweag.io/blog/2020-06-25-eval-cache/
    - Part 3: https://www.tweag.io/blog/2020-07-31-nixos-flakes/
  - [Nix package scopes](https://andreas.rammhold.de/posts/nix-package-scopes/)
    - > overlays are used to add internal packages into pkgs. IMHO this is an
      > anti-pattern as long as you are not trying to modify packages that are
      > part of nixpkgs
- YouTube
  - [Nix Fundamentals](https://www.youtube.com/watch?v=m4sv2M9jRLg)
- Std
  - https://divnix.github.io/std/
