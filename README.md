# 🗺️ cartographer-core

This is a rewrite of the core library part of
the [cartographer project](http://cartographer.id/).

It lets you:

- [x] construct hypergraphs
- [x] compose and tensor them
- [x] match patterns
- [ ] apply rewrite rules

The last of these is currently implemented save for the safety check
("convexity") as described [here](https://arxiv.org/pdf/1602.06771.pdf).

# Dependencies, Build, and REPL

cartographer uses the [nix package manager](https://nixos.org/nix/) to manage
dependencies.

You can build with

    nix-build release.nix

If you want to mess around with the library in GHCI, make sure you have cabal
installed:

    nix-env --install cabal-install

Then you can get a repl with

    nix-shell
    cabal repl
