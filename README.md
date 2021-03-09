# strema

![Build status](https://gitlab.com/gilmi/strema/badges/master/pipeline.svg?job=pipeline)

A compiler targeting javascript for a custom type inferred programming language with first class functions, ADTs and pattern matching.

> Status: WIP toy proglang

![Strema logo](logos/lambda2.png)

> logos adapted from “Rad Pack – 80’s Theme” Wallpapers by Nate Wren (CC BY-NC 4.0)

This project is streamed on [twitch.tv/suppipi](https://twitch.tv/suppipi).
For more information visit the [webpage](https://gilmi.gitlab.io/strema).

## Build and run

### With [Stack](https://haskellstack.org)

```sh
> stack run -- compile --input examples/factorial.strm --output /tmp/output.js
> node /tmp/output.js # use node to run the program
120
```

### With [Cabal+GHC](https://www.haskell.org/ghcup):

```sh
> cabal v2-update
> cabal v2-build all
> cabal v2-run stremac -- compile --input examples/factorial.strm --output /tmp/output.js
> node /tmp/output.js
120
```

Note that the type checker will be integrated to the `compile` command once it is finished.
