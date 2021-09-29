submodule(oracle_m) oracle_s
  !! define procedures corresponding to the interface bodies in oracle_m
  implicit none

contains

  module procedure within_tolerance
    class(oracle_t), allocatable :: error

    error = self - reference
    in_tolerance = (error%norm() <= tolerance)

  end procedure

end submodule oracle_s
