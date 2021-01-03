module command_line_interface
  !! return command line argument information
  implicit none

  private
  public :: command_line_t

  type command_line_t
  contains
    procedure, nopass :: argument_present
  end type

  interface

    module function argument_present(acceptable_argument) result(found)
      !! result is .true. only if a command-line argument matches an element of this function's argument
      character(len=*), intent(in) :: acceptable_argument(:)
        !! sample list: [character(len=len(<longest_argument>)):: "--benchmark", "-b", "/benchmark", "/b"]
        !! where dashes support Linux/macOS, slashes support Windows, and <longest_argument> must be replaced
        !! by the longest list element ("--benchmark" above)
      logical found
    end function

  end interface

end module
