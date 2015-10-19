# Hish

[![Build Status](https://api.travis-ci.org/jaiyalas/Hish.png?branch=stable)](http://travis-ci.org/jaiyalas/Hish)
[![MIT](http://b.repl.ca/v1/license-BSD3-blue.png)](https://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-orange.png)](http://haskell.org)

Generating a beautiful and useful prompt.

## Features

### Done

+ [X] show **user name**
+ [X] show **host name**
+ [X] show **working directory**
  + will be shortened if too long
+ [X] show **branch name (git only)**
+ [X] show **index/tree status (git only)**
  + *\** - dirty
  + *?* - clean *but still having untracked files*
  + *#* - clean
+ [X] show **tracking status (git only)**
  + *+n* - branch is *ahead* by n
  + *-n* - branch is *behind* by n
+ [X] Basic ANSI color

### Todo

+ [ ] support *darcs*
+ [ ] show *time*
+ [ ] implement *color theme*
+ [ ] show *SSH* info
+ [ ] use *config file*
+ [ ] load *environment variables*

## Installation

```
> cabal install Hish
```
