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
courses that we teach.  Most code starts here because it feels too small to
release as a standalone package but too distinct in purpose to fold into other
existing packages.  Over time, when code that starts here grows in capability, a
new repository is born and the corresponding code is removed from the Sourcery
repository.  Following the practice of [semantic versioning], code removal 
results in a major version number increment.

versioning to Examples
include the [Assert] and [Emulators] libraries.

Utility functions
-----------------

* Array functions
* String functions
* User-defined collective subroutines: `co_all`

Classes
-------
* Parallel data partitioning and gathering
* (Co-)[Object pattern] abstract parent
* Runtime units tracking

Prerequisites
-------------
See the [fpm manifest](./fpm.toml) for the dependencies and developer
dependencies, the latter of which are needed only for contributing to Sourcery
by adding new tests.  Additionally, [FORD] 6.1.0 or later is required for
producing HTML documentation.

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
