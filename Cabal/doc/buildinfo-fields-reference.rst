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
  * format: ``[:alnum:-_.+]+``
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
  * format: ``\d+(.\d+)*``
  * default: ````
  * more documentation about :pkg-field:`version`

visibility
  * format: ``public|private``
  * default: ``private``
  * more documentation about :pkg-field:`visibility`


