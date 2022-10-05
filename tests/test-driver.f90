program main
  use user_defined_collectives_test, only : collectives_test_t  
  use object_m_test, only : object_test_t  
  use formats_test, only : formats_test_t  
  implicit none

  type(collectives_test_t) collectives_test
  type(object_test_t) object_test
  type(formats_test_t) formats_test

  call collectives_test%report()
  call object_test%report()
  call formats_test%report()
end program
