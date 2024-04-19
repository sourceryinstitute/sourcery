program handle_missing_flag
  !! Verify that a missing command-line flag value returns a zero-length string.
  !! Running this program as follows with the command
  !!
  !! fpm run --example handle-missing-flag -- --empty-flag
  !!
  !! should result in normal termination.
  use assert_m, only : assert
  use sourcery_m, only : command_line_t
  implicit none

  type(command_line_t) command_line
  character(len=:), allocatable :: flag_value
  character(len=*), parameter :: expected_name=""

  flag_value = command_line%flag_value("--empty-flag")

  call assert(flag_value==expected_name,"handle_missing_flag: expected empty flag value", flag_value)
end program
