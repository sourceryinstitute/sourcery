!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
module emulated_intrinsics_interface
  !! author: Damian Rouson
  !!
  !! This module contains two categories of procedures:
  !! 1. Emulations of some Fortran 2008 and 2018 instrinsic procedures for use with
  !!    compilers that lack support for the corresponding procedures.
  !! 2. User-defined collective procedures not defined in the Fortran standard.
  use iso_fortran_env, only : real32, real64

  implicit none

  interface
    module subroutine co_all(boolean)
      implicit none
      logical, intent(inout) :: boolean
    end subroutine
  end interface

  !! Fortran 2003 emulation of Fortran 2008 intrinsic procedures (e.g, findloc)

#ifdef COMPILER_LACKS_FINDLOC
  interface findloc
    !! result is the last occurrence of a value in an array or zero if not found
    module procedure findloc_integer_dim1, findloc_logical_dim1, findloc_character_dim1
  end interface
#endif

  !! Fortran 2008 coarray emulations of Fortran 2018 intrinsic collective subroutines

#ifdef COMPILER_LACKS_COLLECTIVE_SUBROUTINES
  interface co_sum
    !! parallel computation of the sum of the first argument
    module procedure co_sum_integer
    module procedure co_sum_real32_1D_array
    module procedure co_sum_real32_2D_array
    module procedure co_sum_real64_1D_array
    module procedure co_sum_real64_2D_array
  end interface

  interface co_broadcast
    !! parallel one-to-all communication of the value of first argument
    module procedure co_broadcast_logical
    module procedure co_broadcast_integer
    module procedure co_broadcast_real32_1D_array
    module procedure co_broadcast_real32_2D_array
    module procedure co_broadcast_real64_1D_array
    module procedure co_broadcast_real64_2D_array
  end interface

  interface co_reduce
    !! parallel reduction of values across images of the first argument
    module procedure co_reduce_logical
  end interface
#endif

  interface

#ifdef COMPILER_LACKS_COLLECTIVE_SUBROUTINES
    module subroutine co_sum_integer(a,result_image,stat,errmsg)
      implicit none
      integer, intent(inout) :: a
      integer, intent(in), optional :: result_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_sum_real32_1D_array(a,result_image,stat,errmsg)
      implicit none
      real(real32), intent(inout) :: a(:)
      integer, intent(in), optional :: result_image
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_sum_real32_2D_array(a,result_image,stat,errmsg)
      implicit none
      real(real32), intent(inout) :: a(:,:)
      integer, intent(in), optional :: result_image
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_sum_real64_1D_array(a,result_image,stat,errmsg)
      implicit none
      real(real64), intent(inout) :: a(:)
      integer, intent(in), optional :: result_image
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_sum_real64_2D_array(a,result_image,stat,errmsg)
      implicit none
      real(real64), intent(inout) :: a(:,:)
      integer, intent(in), optional :: result_image
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_broadcast_logical(a,source_image,stat,errmsg)
      implicit none
      logical, intent(inout) :: a
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_broadcast_integer(a,source_image,stat,errmsg)
      implicit none
      integer, intent(inout) :: a
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_broadcast_real32_1D_array(a,source_image,stat,errmsg)
      implicit none
      real(real32), intent(inout) :: a(:)
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_broadcast_real32_2D_array(a,source_image,stat,errmsg)
      implicit none
      real(real32), intent(inout) :: a(:,:)
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_broadcast_real64_1D_array(a,source_image,stat,errmsg)
      implicit none
      real(real64), intent(inout) :: a(:)
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_broadcast_real64_2D_array(a,source_image,stat,errmsg)
      implicit none
      real(real64), intent(inout) :: a(:,:)
      integer, intent(in) :: source_image
      integer, intent(out), optional ::  stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine

    module subroutine co_reduce_logical(a,operation,result_image,stat,errmsg)
      implicit none
      abstract interface
        pure function operation_i(x, y)
          logical, intent(in) :: x, y
          logical :: operation_i
        end function
      end interface
      logical, intent(inout) :: a
      procedure(operation_i) :: operation
      integer, intent(in), optional :: result_image
      integer, intent(out), optional :: stat
      character(len=*), intent(inout), optional :: errmsg
    end subroutine
#endif

#ifdef COMPILER_LACKS_FINDLOC
    pure module function findloc_integer_dim1(array, value, dim, back) result(location)
      implicit none
      integer, intent(in) :: array(:), value, dim
      logical, intent(in), optional :: back
      integer location
    end function

    pure module function findloc_logical_dim1(array, value, dim, back) result(location)
      implicit none
      logical, intent(in) :: array(:), value, back
      integer, intent(in) :: dim
      integer location
    end function

    pure module function findloc_character_dim1(array, value, dim, back) result(location)
      implicit none
      character(len=*), intent(in) :: array(:), value
      integer, intent(in) :: dim
      logical, intent(in) :: back
      integer location
    end function
#endif

  end interface

end module emulated_intrinsics_interface
