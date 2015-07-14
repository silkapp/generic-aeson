# Changelog

#### 0.2.0.7

* Allow `generic-deriving 1.8.*`
* Allow `vector 0.11.*`

#### 0.2.0.6

* Allow `aeson 0.9.*`

#### 0.2.0.5

* Allow `attoparsec 0.13.*`

#### 0.2.0.4

* Fix compilation on GHC 7.4

#### 0.2.0.3

* Allow `tagged 0.8.*`

#### 0.2.0.2

* Allow `generic-deriving 1.7.*`

#### 0.2.0.1

* Allow `text 1.2.*`

## 0.2.0.0

* Add `gtoJsonWithSettings` and `gparseJsonWithSettings` to customize
  the generated JSON, currently only to strip specified prefixes from
  record fields.
* Format Change: The behavior of Maybes was inconsistent and buggy,
  now we always map `Just` directly to the value, and `Nothing` to
  null if on the top level or in an unnamed field and remove the
  property if it's in a named field.
* Changed the type of `selNameT` to return a `Maybe Text` which will
  be `Nothing` instead of `""` (unnamed fields)

#### 0.1.1.1

* Fix regression in implementation of `multipleConstructors` introduced in 0.1.1

### 0.1.1

* Add `Generics.Generic.Aeson.Util` module with helper function for interoperating packages

#### 0.1.0.3

* Allow `attoparsec 0.12.*`

#### 0.1.0.2

* Allow `mtl 2.2.*`
