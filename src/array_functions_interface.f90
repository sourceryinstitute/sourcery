!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
module array_functions_interface
  !! author: Damian Rouson
  !! date: 04/25/2019
  !!
  !! Functionally pure array utilities
  implicit none

  private
  public :: operator(.catColumns.)
  public :: operator(.catRows.)
  public :: operator(.columnVectors.)
    !! Because the Fortran standard requires that operator dummy arguments have the intent(in) attribute
    !! exposing only the operator and not the function names communicates more information in the
    !! public interface and in code using this interface.

    interface operator(.columnVectors.)
        module procedure column_vectors
    end interface

    interface operator(.catColumns.)
        module procedure concatenate_columns
    end interface

    interface operator(.catRows.)
        module procedure concatenate_rows
    end interface

    interface

        pure module function column_vectors(vector_field) result(array_of_3D_column_vectors)
            !! Result is array of 3D column vectors of dimension (space_dim,nx*ny*nz) reshaped from vector-field argument
            !! of dimension (nx,ny,nz,space_dim)
            implicit none
            real, dimension(:,:,:,:), intent(in) :: vector_field
            real, dimension(:,:), allocatable ::  array_of_3D_column_vectors
        end function

        pure module function concatenate_columns(a, b) result(concatenated)
            !! Result contains the concatenation of the columns of argument a with the columns of argument b
            implicit none
            real, dimension(:,:), intent(in) :: a, b
            real, dimension(:,:), allocatable :: concatenated
        end function

        pure module function concatenate_rows(a, b) result(concatenated)
            !! Result contains the concatenation of the rows of argument a with the rows of argument b
            implicit none
            real, dimension(:,:), intent(in) :: a, b
            real, dimension(:,:), allocatable :: concatenated
        end function

    end interface

end module
