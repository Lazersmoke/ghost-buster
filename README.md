Ghost Buster
============

[![Hackage](https://img.shields.io/hackage/v/ghost-buster.svg)](https://hackage.haskell.org/package/ghost-buster) [![Build Status](https://travis-ci.org/Lazersmoke/ghost-buster.svg?branch=master)](https://travis-ci.org/Lazersmoke/ghost-buster)

This package provides an easier way to work with existential data types. 
Normally, GHC will reject types that try to instantiate unification variables to polymorphic types (AKA impredicative types).
This package uses a data contructor to hide that away, so you can nest as deeply as you like.

See [tests](https://github.com/Lazersmoke/ghost-buster/blob/master/tests/Spec.hs) for example code (more coming soon).
