# Hish

[![Build Status](https://api.travis-ci.org/jaiyalas/Hish.png?branch=stable)](http://travis-ci.org/jaiyalas/Hish)
[![MIT](http://b.repl.ca/v1/license-BSD3-blue.png)](https://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-orange.png)](http://haskell.org)

Prompt program for Fish shell written in Haskell

## Features

### Done

+ show **git status**
  + "#" for *clean*
  + "\*" for *dirty*
+ show **git branch**
+ show **working directory**
  + be shortened if too long

### Todo

+ [ ] distinguish the cases of *clean* and *up-to-date*
+ [ ] show *time*
+ [ ] implement *color theme*
+ [ ] show *SSH* info
+ [ ] use *config file*
+ [ ] load *environment variables*

## Installation

```
> cabal install Hish
```
