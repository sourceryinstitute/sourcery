program main
  use user_defined_collectives_test, only : collectives_test_t  
  use data_partition_test, only : data_partition_test_t
  use object_m_test, only : object_test_t  
  use formats_test, only : formats_test_t  
  implicit none

  type(collectives_test_t) collectives_test
  type(data_partition_test_t) data_partition_test
  type(formats_test_t) formats_test
  type(object_test_t) object_test

  integer :: passes=0, tests=0

  call data_partition_test%report(passes, tests)
  call collectives_test%report(passes, tests)
  call object_test%report(passes, tests)
  call formats_test%report(passes, tests)

  print *
  print *,"_________ In total, ",passes," of ",tests, " tests pass. _________"

end program
