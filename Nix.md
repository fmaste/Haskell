# Nix

## TL;DR;

Nix is a purely functional package manager

## It's much more than that

Nixpkgs, NixOS, NixOps, Hydra.
https://nixos.wiki/wiki/Nix_Ecosystem
https://nixos.wiki/images/4/44/Nixos-stack2.png

### The best of functional programming

The Nix Expression Language is a dynamic, functional and lazy domain-specific language used to instruct Nix how to build packages.
https://nixos.org/manual/nix/stable/expressions/expression-language.html

### Multiple versions

Multiple versions

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

Wikipedia
https://en.wikipedia.org/wiki/Nix_(package_manager)
Manual
https://nixos.org/manual/nix/stable/
https://nixos.org/manual/nix/stable/introduction.html
Wiki
https://nixos.wiki/wiki/Main_Page
https://nix.dev/
Blog posts
https://serokell.io/blog/what-is-nix
https://serokell.io/blog/practical-nix-flakes
Other
https://nix.dev/recommended-reading
https://nixos.org/guides/nix-pills/
https://ianthehenry.com/posts/how-to-learn-nix/
Flakes
https://nixos.wiki/wiki/Flakes
- Part 1: https://www.tweag.io/blog/2020-05-25-flakes/
- Part 2: https://www.tweag.io/blog/2020-06-25-eval-cache/
- Part 3: https://www.tweag.io/blog/2020-07-31-nixos-flakes/
