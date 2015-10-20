# Hish

[![Build Status](https://api.travis-ci.org/jaiyalas/Hish.png?branch=stable)](http://travis-ci.org/jaiyalas/Hish)
[![MIT](http://b.repl.ca/v1/license-BSD3-blue.png)](https://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-orange.png)](http://haskell.org)

Generating a beautiful and useful prompt. (**Warning**: *darcs* is supported but *turned-off* by default.)

## Features

### Done

+ [X] Basic ANSI color
+ [X] show **local time**
+ [X] show **user name**
+ [X] show **host name**
+ [X] show **working directory**
  + will be shortened if too long
+ [X] support version control system
   + git (fully supported)
   + <del>darcs (darcs has no branch; tracking status is not supported)</del>
+ [X] show **branch name (git only)**
+ [X] show **index/tree status (git and <del>darcs</del>)**
  + *\ * - clean
  + *?* - clean  *but still having untracked files*
  + *#** - dirty non-empty index
  + *\** - dirty with empty index
+ [X] show **tracking status (git only)**
  + *+n* - branch is *ahead* by n
  + *-n* - branch is *behind* by n

### Todo

+ [ ] use *config file*
+ [ ] re-design the darcs supporting system
+ [ ] implement *color theme*
+ [ ] show *SSH* info
+ [ ] load *environment variables*

## Installation

```
> cabal install Hish
```
