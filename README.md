# generic-aeson

[![Build Status](https://travis-ci.org/silkapp/generic-aeson.svg?branch=master)](https://travis-ci.org/silkapp/generic-aeson)

The structure of the generated JSON is meant to be close to
idiomatic JSON. This means:

* Enumerations are converted to JSON strings.

* Record fields become JSON keys.

* Data types with one unlabeled field convert to just that field.

* Data types with multiple unlabeled fields become arrays.

* Multiple constructors are represented by keys.

* 'Maybe' values are either an absent key, or the value.

See `tests/Main.hs` for more examples.
