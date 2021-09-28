Sourcery Library
================

A grab bag of useful tricks in Fortran 2018.  

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
* Parallel data partitioning and gathering,
* (Co-)[Object pattern] abstract parent,
* Runtime units tracking,
* A test oracle using the [Template Method pattern], and
* A command-line abstraction that searches for program arguments.

Documentation
-------------
See the [Sourcery GitHub Pages site] for HTML documentation generated with [`ford`]:

Prerequisites
-------------
[FORD] 6.1.0 or later is required for producing HTML documentation (see
"[Building the documentation]" below for instructions).  The Fortran Package
Manager ([fpm]) is required to build Sourcery from source.  See the
[fpm manifest](./fpm.toml) for the dependencies and developer dependencies,
all of which [fpm] automatically downloads and builds via the `fpm` command
provided in the "[Downloding, Building, and Testing]" section below.


Downloding, Building, and Testing
---------------------------------
With recent versions of [GNU Fortran] (gfortran) and [OpenCoarrays] installed, 
execute the following command in a `zsh` or `bash`-like shell:

```zsh
git clone git@github.com:sourceryinstitute/sourcery
fpm test --compiler caf --runner "cafrun -n 4"
  
```
substituting the number of desired images for the "4" above.

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

