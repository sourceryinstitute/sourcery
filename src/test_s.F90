submodule(test_m) test_s
#ifdef XLF
  use test_result_m, only : test_result_t
#endif
  implicit none

contains

  module procedure report
    integer i
#ifdef XLF
    type(test_result_t), allocatable :: test_results(:)
    test_results = test%results()
#else
    associate(test_results => test%results())
#endif

      print *
      print *, test%subject()

      do i=1,size(test_results)
        print *,"   ",test_results(i)%characterize()
      end do
#ifndef XLF
    end associate
#endif
  end procedure

end submodule test_s
