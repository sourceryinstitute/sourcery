program main
  use user_defined_collectives_test_m, only : collectives_test_t  
  use data_partition_test_m, only : data_partition_test_t
  use object_m_test_m, only : object_test_t  
  use formats_test_m, only : formats_test_t  
  use test_result_test_m, only : test_result_test_t  
  use command_line_test_m, only : command_line_test_t
  use string_test_m, only : string_test_t
  implicit none

  type(collectives_test_t) collectives_test
  type(data_partition_test_t) data_partition_test
  type(formats_test_t) formats_test
  type(object_test_t) object_test
  type(test_result_test_t) test_result_test
  type(command_line_test_t) command_line_test
  type(string_test_t) string_test

  integer :: passes=0, tests=0


  call data_partition_test%report(passes, tests)
  call collectives_test%report(passes, tests)
  call object_test%report(passes, tests)
  call formats_test%report(passes, tests)
  call test_result_test%report(passes, tests)
  call string_test%report(passes, tests)

  if (.not. GitHub_CI())  call command_line_test%report(passes, tests)

  if (this_image()==1) print *, new_line('a'), "_________ In total, ",passes," of ",tests, " tests pass. _________"

  if (passes /= tests) error stop

contains

  logical function GitHub_CI()
    integer name_length
    character(len=:), allocatable :: CI

    call get_environment_variable("CI", length=name_length)

    if (name_length==0) then
      GitHub_CI = .false.
    else
      allocate(character(len=name_length):: CI)
      call get_environment_variable("CI", value=CI)
      GitHub_CI = merge(.true., .false., CI=="true")
    end if
  end function

end program
