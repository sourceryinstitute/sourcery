module sourcery_formats_m
  !! Useful strings for formatting `print` and `write` statements
  implicit none

  character(len=*), parameter :: csv = "(*(G0,:,','))" !! comma-separated values
  character(len=*), parameter :: cscv = "(*('(',G0,',',G0,')',:,',')))" !! comma-separated complex values

  interface

    pure module function separated_values(separator, mold) result(format_string)
      character(len=*), intent(in) :: separator 
      class(*), intent(in) :: mold(..)
      character(len=:), allocatable :: format_string
    end function

  end interface

end module
