submodule(command_line_m) command_line_s
  use assert_m, only : assert
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

  module procedure flag_value

    integer argnum, arglen
    character(len=64) arg

    flag_search: &
    do argnum = 1,command_argument_count()
      call get_command_argument(argnum, arg, arglen)
      if (arg==flag) then
        call assert(arglen<=len(arg), "flag_value: arglen<=len(arg)")
        allocate(character(len=arglen) :: flag_val)
        call get_command_argument(argnum+1, flag_val)
        exit flag_search
      end if
    end do flag_search

  end procedure

end submodule
