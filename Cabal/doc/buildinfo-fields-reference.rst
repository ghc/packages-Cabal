.. _buildinfo-field-reference:

==================================================
 BuildInfo field reference
==================================================

Notation
---------------

TBW

Field reference
---------------

Field formats are described as they are in the latest file format version


buildable
  * format: ``True|False``
  * more documentation about :pkg-field:`buildable`

default-language
  * format: ``Haskell98|Haskell2010``
  * more documentation about :pkg-field:`default-language`


Library stanza fields
---------------------


exposed
  * format: ``True|False``
  * more documentation about :pkg-field:`exposed`


Test-suite stanza fields
------------------------


main-is
  * format: ``{haskell-string}|[^ ,]+``
  * more documentation about :pkg-field:`main-is`

test-module
  * format: ``{module-name}``
  * more documentation about :pkg-field:`test-module`

type
  * format: ``exitcode-stdio-1.0|detailed-0.9``
  * more documentation about :pkg-field:`type`


Benchmark stanza fields
-----------------------


benchmark-module
  * format: ``{module-name}``
  * more documentation about :pkg-field:`benchmark-module`

main-is
  * format: ``{haskell-string}|[^ ,]+``
  * more documentation about :pkg-field:`main-is`

type
  * format: ``exitcode-stdio-1.0``
  * more documentation about :pkg-field:`type`


Foreign-library stanza fields
-----------------------------


lib-version-info
  * format: ``[:digit:]+(:[:digit:]+(:[:digit:]+)?)?``
  * more documentation about :pkg-field:`lib-version-info`

lib-version-linux
  * format: ``[:digit:]+(.[:digit:]+)*``
  * more documentation about :pkg-field:`lib-version-linux`

type
  * format: ``native-shared|native-static``
  * default: ``unknown``
  * more documentation about :pkg-field:`type`


Flag stanza fields
------------------


default
  * format: ``True|False``
  * more documentation about :pkg-field:`default`

description
  * format: free text field
  * more documentation about :pkg-field:`description`

manual
  * format: ``True|False``
  * more documentation about :pkg-field:`manual`


Source-Repository stanza fields
-------------------------------


branch
  * format: ``{haskell-string}|[^ ,]+``
  * more documentation about :pkg-field:`branch`

location
  * format: free text field
  * more documentation about :pkg-field:`location`

module
  * format: ``{haskell-string}|[^ ,]+``
  * more documentation about :pkg-field:`module`

subdir
  * format: ``{haskell-string}|[^ ,]+``
  * more documentation about :pkg-field:`subdir`

tag
  * format: ``{haskell-string}|[^ ,]+``
  * more documentation about :pkg-field:`tag`

type
  * format: ``[[:alnum:]-_]+``
  * more documentation about :pkg-field:`type`


Custom-setup stanza fields
--------------------------



Installed package info
----------------------


abi
  * format: ``[:alnum:]*``
  * default: ````
  * more documentation about :pkg-field:`abi`

author
  * format: free text field
  * more documentation about :pkg-field:`author`

category
  * format: free text field
  * more documentation about :pkg-field:`category`

copyright
  * format: free text field
  * more documentation about :pkg-field:`copyright`

data-dir
  * format: ``{haskell-string}|[^ ,]+``
  * default: ``""``
  * more documentation about :pkg-field:`data-dir`

description
  * format: free text field
  * more documentation about :pkg-field:`description`

exposed
  * format: ``True|False``
  * more documentation about :pkg-field:`exposed`

homepage
  * format: free text field
  * more documentation about :pkg-field:`homepage`

id
  * format: ``[[:alnum:]+-._]+``
  * default: ````
  * more documentation about :pkg-field:`id`

indefinite
  * format: ``True|False``
  * more documentation about :pkg-field:`indefinite`

instantiated-with
  * format: ``{open-module-substitution}``
  * default: ````
  * more documentation about :pkg-field:`instantiated-with`

key
  * format: ``{compat-package-key}``
  * default: ````
  * more documentation about :pkg-field:`key`

lib-name
  * format: ``{unqualified-component-name}``
  * more documentation about :pkg-field:`lib-name`

license
  * format: ``{ipi-lenient-license}``
  * default: ``NONE``
  * more documentation about :pkg-field:`license`

maintainer
  * format: free text field
  * more documentation about :pkg-field:`maintainer`

name
  * format: ``{munged-package-name}``
  * default: ````
  * more documentation about :pkg-field:`name`

package-name
  * format: ``{unqualified-component-name}``
  * more documentation about :pkg-field:`package-name`

package-url
  * format: free text field
  * more documentation about :pkg-field:`package-url`

pkgroot
  * format: ``{haskell-string}|[^ ,]+``
  * more documentation about :pkg-field:`pkgroot`

stability
  * format: free text field
  * more documentation about :pkg-field:`stability`

synopsis
  * format: free text field
  * more documentation about :pkg-field:`synopsis`

trusted
  * format: ``True|False``
  * more documentation about :pkg-field:`trusted`

version
  * format: ``[:digit:]+(.[:digit:]+)*``
  * default: ````
  * more documentation about :pkg-field:`version`

visibility
  * format: ``public|private``
  * default: ``private``
  * more documentation about :pkg-field:`visibility`


