module formats
  !! Useful strings for formatting `print` and `write` statements
  implicit none

  character(len=*), parameter :: comma_separated_values = ""(*(G0,:,','))""
end module
