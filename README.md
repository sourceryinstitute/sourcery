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

Utility functions
-----------------

* Array functions
* Assertions
* Emulated intrinsic functions: `findloc`
* Emulated collective subroutines: `co_sum`, `co_broadcast`
* User-defined collective subroutines: `co_all`
* String functions

Classes
-------
* Parallel data partitioning and gathering
* (Co-)Object pattern abstract parent

Prerequisites
-------------
The following are the prerequisite package versions/commits currently
employed in developing and testing (_earlier versions/commits might work also_):

* Compiler: [GNU Fortran] (gfortran) 10.2.0
* Parallel runtime library: [OpenCoarrays] 2.9.2
* Fortran package manager: [fpm] 7d320295c6fe56cc767b352bd26f0466aa29ac5c
* _Optional_: [FORD] 6.0.0 for producing HTML documentation.

This library also uses the [vegetables] unit testing framework, which
the [fpm] build system will install automatically.

Downloding, Building, and Testing
---------------------------------

```zsh
git clone git@github.com:sourceryinstitute/sourcery
fpm test \
  --compiler caf \
  --runner "cafrun -n 4" \
  --flag "-Wall" \
  --flag "-std=f2018" \
  --flag "-DCOMPILER_LACKS_COLLECTIVE_SUBROUTINES" \
  --flag "-DCOMPILER_LACKS_FINDLOC"
```
where the `COMPILER_LACKS_*` flags exercise the Sourcery Library's
emulated instrinsic procedures, which are intended for use with
compiler versions that lack support for the named features.  Delete
those flags with compilers that support these features.

Building the documentation
--------------------------
After installing python and `ford` (via `pip install ford`), execute
```zsh
ford doc/ford-documentation.md
```
after which opening subdirectory `doc/html/index.html` in a browser.

[GNU Fortran]: https://gcc.gnu.org
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[fpm]: https://github.com/fortran-lang/fpm
[vegetables]: https://gitlab.com/everythingfunctional/vegetables
[FORD]: https://github.com/Fortran-FOSS-Programmers/ford
