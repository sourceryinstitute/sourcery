Sourcery Library
================

A grab bag of useful tricks in Fortran 2023.  

```fortran
                      -:/+-
                    ./oydddy/.
                   ./sdmNNmmdh/.
                   /odNNmNmyshdh/.
                  -+ymNNmmms...ohy.
                 .+odmNNmdmm+.  ...
                 -+sdmNNNmdmmo.
                 /+ydmNNNmddmms-
                -+ohmmNNNNmddmmy:
                /+ohmmNNNNmdhdmmy/.
               .++shdmNNNNNmdhdmmh/.
               :/+shdmmNNNNmdhhmmmy/.
              .+/+shdmmNNNNNmdhdmmm##.............
              -//+shdmmNNNNNmdhyyy##--sssssssssssso+-.
           ..-##/+shdmmmNNNNmhs/##//+sdhhhyyhddhso+++:
       ..:oyh## /+shdmmmNNNds##syyyyhhhhhhhdho-.
     .:oyhhdmh##/+shddmddy+##+hdysoosyys/-..
   ./ssssyhdmd:-##//////##ndifmdhyooo+-
  :ooo+oosyhdmdyo+######ydmNNNNNmhs/.
 :+++///++osyhhhdmmmmmNNNNNNNNmdo-
 +++//////+oossshddmmmmmNNNmdy/.
 .///////+++oosyhhddmmmmddy/.
   .:////+++ossyyhhhhhs+:.
      ---..........--
```

This library gathers software that developers at [Archaeologic Inc.] and
[Sourcery Institute] find useful across many of our projects, including in
courses that we teach.  Most code starts here because it is too limited in 
capability to release as a standalone package but too distinct in purpose to 
fold into other existing packages.  Over time, when code that starts here grows
in capability, a new repository is born and the corresponding code is removed
from the Sourcery repository.  Examples include the [Assert] and [Emulators]
libraries.  Following the practice of [semantic versioning], code removal
causes an increment in the major version number.

Contents
--------

### Procedures

* Array functions
* String functions
* User-defined collective subroutines
* Input/output format strings and format string generators

### Classes
* A command-line abstraction that searches for program arguments.
* A minimalistic unit testing framework based around four types (see [./test/main.f90](./test/main.f90) for a use case): 
  - `test_t` is a parent type to extend when defining unit tests,
  - `test_description_t` encapsulates scalar test functions and test descriptoins,
  - `vector_test_description`: encapsulates array-valued test functions and test descriptions.
  - `test_result_t` encapsulates test deescriptions and boolean (pass/fail) outcomes, and
* Parallel data partitioning and gathering,
* (Co-)[Object pattern] abstract parent,
* Runtime units tracking,
* A test oracle using the [Template Method pattern], and

Documentation
-------------
See the [Sourcery GitHub Pages site] for HTML documentation generated with [`ford`]:

Prerequisites
-------------
* [FORD] 6.1.0 or later is required for producing HTML documentation (see
"[Building the documentation]" below for instructions).  
* The Fortran Package Manager ([fpm]) is required to build Sourcery from source.
* GCC (`gfortran`) 13.1.0
* OpenCoarrays 2.10.1 for parallel execution

See [fpm manifest](./fpm.toml) for the dependencies and developer dependencies,
that [fpm] automatically downloads and builds via the `fpm` command provided in
the "[Downloding, Building, and Testing]" section below.

Downloding
----------
```zsh
git clone git@github.com:sourceryinstitute/sourcery
```

Building and Testing
--------------------
### Test-Suite Usage
Executing `fpm test -- --help` prints the following message:
```
Usage: fpm test -- [--help] | [--contains <substring>]

where square brackets ([]) denote optional arguments, a pipe (|) separates alternative arguments,
angular brackets (<>) denote a user-provided value, and passing a substring limits execution to
the tests with test subjects or test descriptions containing the user-specified substring.
```

### GNU Fortran (`gfortran`)  compiler
#### Single-image (serial) testing
With recent versions of [GNU Fortran] (gfortran) and [OpenCoarrays] installed, 
execute the following command in a `zsh` or `bash`-like shell:
```zsh
fpm test
```

#### Multi-image (parallel) testing
With recent versions of `gfortran` and [OpenCoarrays] installed, 
execute the following command in a `zsh` or `bash`-like shell:
```zsh
fpm test --compiler caf --runner "cafrun -n 4"
```
Substitute the desired number of images for the `4` above.

### Intel (`ifx`) compiler
```zsh
fpm test --compiler ifx --flag "-coarray"
```

### Numerical Algorithms Group (`nagfor`) compiler
```zsh
fpm test --compiler nagfor --flag -fpp
```

### Building and testing with the Cray Compiler Environment (CCE)
Because `fpm` uses the compiler name to determine the compiler identity and because
CCE provides one compiler wrapper, `ftn`, for invoking all compilers, you will
need to invoke `ftn` in a shell script named to identify CCE compiler. For example,
place a script named `crayftn.sh` in your PATH with the following contents and with
executable privileges set appropriately:
```
#!/bin/bash

ftn $@
```
Then build and test Sourcery with the command
```
fpm test --compiler crayftn.sh
```

### Building and testing with other compilers
To use Sourcery with other compilers, please submit an issue or pull request. 

Building the documentation
--------------------------
After installing `ford`, execute the following command in a `zsh` or `bash`-like
shell:
```zsh
ford doc/ford-documentation.md
```
after which opening subdirectory `doc/html/index.html` in a browser displays the
documentation.

[GNU Fortran]: https://gcc.gnu.org
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[fpm]: https://github.com/fortran-lang/fpm
[vegetables]: https://gitlab.com/everythingfunctional/vegetables
[FORD]: https://github.com/Fortran-FOSS-Programmers/ford
[Archaeologic Inc.]: https://archaeologic.codes
[Sourcery Institute]: http://www.sourceryinstitute.org
[Assert]: https://github.com/sourceryinstitute/assert
[Emulators]: https://github.com/sourceryinstitute/emulators
[Object pattern]: https://www.cambridge.org/rouson
[semantic versioning]: https://semver.org
[Template Method pattern]: https://en.wikipedia.org/wiki/Template_method_pattern
[Downloding, Building, and Testing]: #downloding-building-and-testing
[Building the documentation]: #building-the-documentation
[Sourcery GitHub Pages site]: http://sourceryinstitute.github.io/sourcery/
[`ford`]: https://github.com/Fortran-FOSS-Programmers/ford
