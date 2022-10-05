program main
  use user_defined_collectives_test, only : collectives_test_t  
  implicit none

  type(collectives_test_t) collectives_test

  call collectives_test%report()
end program
