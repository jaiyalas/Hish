# Hish

[![Build Status](https://api.travis-ci.org/jaiyalas/Hish.png?branch=stable)](http://travis-ci.org/jaiyalas/Hish)
[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](https://en.wikipedia.org/wiki/MIT_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-orange.png)](http://haskell.org)

Prompt program for Fish shell written in Haskell

## Install

```
> cabal configure
> cabal build
> cabal install
```



## Features

+ show **git status**
  + "#" for *clean*
  + "\*" for *dirty*
+ show **git branch**
+ show **working directory**
  + be shortened if too long

## Todo

+ [ ] show *time*
+ [ ] implement *color theme*
+ [ ] show *SSH* info
+ [ ] use *config file*
+ [ ] load *environment variables*
