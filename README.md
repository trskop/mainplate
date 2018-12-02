# Mainplate

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]


## Description

**Warning: Work in progress**

Patterns to be used for application `main` functions, options parsing, and
configuration.

This library is NOT optimised for rapid development of simple tools.  Its goal
is to enforce proper and modular structure of applications, especially those
with command-line interface.  It forces you to tackle the complexity up front,
and to structure the `main` function in a way that, hopefully, creates
logically consistent command-line interface.  This includes options parsing,
handling of environment variables, and configuration files.
