# Nix

## TL;DR;

Nix is a purely functional package manager.

## But it's much more than that

[The ecosystem](https://nixos.wiki/wiki/Nix_Ecosystem): Nixpkgs, NixOS, NixOps,
Hydra.
![Nix stack](https://nixos.wiki/images/4/44/Nixos-stack2.png)

## Main Features

### Reproducible

> Nix builds packages in isolation from each other. This ensures that they are
> reproducible and don't have undeclared dependencies, so if a package works on
> one machine, it will also work on another.

### Declarative

> Nix makes it trivial to share development and build environments for your
> projects, regardless of what programming languages and tools you’re using.

### Reliable

> Nix ensures that installing or upgrading one package cannot break other
> packages. It allows you to roll back to previous versions, and ensures that no
> package is in an inconsistent state during an upgrade.

### Packages

> [Nixpkgs](https://github.com/nixos/nixpkgs) is a collection of over 80,000
> software packages that can be installed with the Nix package manager. It also
> implements NixOS, a purely-functional Linux distribution.

## 10,000 ft view

A software package (a bunch of different things to build and run) is described
in a special language that defines a function from inputs to outputs. Nix runs
or evaluates it and obtains a precise description of everything it needs to
build this software (or to do some other Nix stuff besides building software,
more on this later). With this description Nix creates something akin to a
sandbox description file with specific dependencies and the build process to run
inside it.

The magic is that this sandbox is linked with specific version of its
dependencies that are always the same dependencies each time the build is run.
From an administrator's point of view: if you want an old PHP version for one
application, but want to upgrade the rest of the system, that's not painful any
more.

There are no upgrade/downgrade scripts for your data. It doesn't make sense with
this approach, because there's no real derivation to be upgraded. With Nix you
switch to using other software with its own stack of dependencies, but there's
no formal notion of upgrade or downgrade when doing so.
* If there is a data format change, then migrating to the new data format
remains your own responsibility.

### In Nix's terminology

To deploy software using Nix you must write ***Nix expressions*** that
describe how to build ***packages***. Nix expressions are written using the
***Nix expression language***.

This high-level description of software packages are evaluated into what Nix
calls a ***derivation***. In this step Nix evaluates code to resolve the
***closures*** of a derivation, the list of all its dependencies including
absolutely everything necessary to use that derivation.

The results of derivations are stored in the ***Nix store*** (typically
```/nix/store```) and describes how to properly built the package in a
reproducible way.

<!--
Nix with all the knowledge it obtained about a package creates a sandbox to
build software that only has the dependencies 
-->

## Others

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
