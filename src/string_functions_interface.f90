!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
module string_functions_interface
  !! author: Damian Rouson
  !! date: August 23, 2019
  !! summary: utilities for manipulating or producing character variables
  implicit none

  private
  public :: file_extension, csv_format, base_name, string

  character(len=*), parameter :: csv_format = '(*(G0,:,","))'

  interface string
    module procedure integer_to_string
  end interface

  interface

    pure module function file_extension(file_name) result(extension)
      !! result contains all characters in file_name after the first dot (.)
      character(len=*), intent(in) :: file_name
      character(len=:), allocatable :: extension
    end function

    pure module function base_name(file_name) result(base)
      !! result contains all characters in file_name before the first dot (.)
      character(len=*), intent(in) :: file_name
      character(len=:), allocatable :: base
    end function

    pure module function integer_to_string(integer_value) result(characters)
      integer, intent(in) :: integer_value
      character(len=:), allocatable :: characters
    end function

  end interface

end module
