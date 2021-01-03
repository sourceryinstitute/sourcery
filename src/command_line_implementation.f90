submodule(command_line_interface) command_line_implementation
  implicit none

contains

  module procedure argument_present
      !! list of acceptable arguments
      !! sample list: [character(len=len(longest_argument)):: "--benchmark", "-b", "/benchmark", "/b"]
      !! where dashes support Linux/macOS and slashes support Windows
    integer :: i, argnum, arglen
      !! loop counter, argument position, argument length
    character(len=32) arg
      !! argument position

      !! acceptable argument lengths (used to preclude extraneous trailing characters)

    associate(acceptable_length => [(len(trim(acceptable_argument(i))), i = 1, size(acceptable_argument))])

      found = .false.

      do argnum = 1,command_argument_count()

        call get_command_argument(argnum, arg, arglen)

        if (any( &
          [(arg==acceptable_argument(i) .and. arglen==acceptable_length(i), i = 1, size(acceptable_argument))] &
        )) then
          found = .true.
        end if

      end do

    end associate

  end procedure

end submodule
