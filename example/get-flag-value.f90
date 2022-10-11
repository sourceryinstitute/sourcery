program get_flag_value
  !! Demonstrate how to find the value of a command-line flag 
  use command_line_m, only : command_line_t
  implicit none

  type(command_line_t) command_line
  character(len=:), allocatable :: input_file_name

  input_file_name = command_line%flag_value("--input-file")

  ! Running this program as follows with the command
  !
  ! fpm run --example get-flag-value -- --input-file foo
  !
  ! result in normal termination.

  print *,"input file: ",input_file_name
  
  if (input_file_name/="foo") error stop "example/get-flag-value: expected flag value 'foo' not receieved"
end program
