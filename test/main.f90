program main
  use user_defined_collectives_test, only : collectives_test_t  
  use data_partition_test, only : data_partition_test_t
  use object_m_test, only : object_test_t  
  use formats_test, only : formats_test_t  
  implicit none

  type(data_partition_test_t) data_partition_test
  type(collectives_test_t) collectives_test
  type(object_test_t) object_test
  type(formats_test_t) formats_test

  call data_partition_test%report()
  call collectives_test%report()
  call object_test%report()
  call formats_test%report()
end program
