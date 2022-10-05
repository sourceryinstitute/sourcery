submodule(test_m) test_s
  implicit none

contains

  module procedure report
    integer i

    associate(test_results => test%results())
      print *
      print *, test%subject()
      associate(num_tests => size(test_results))
        tests = tests + num_tests
        do i=1,num_tests
          print *,"   ",test_results(i)%characterize()
        end do
        associate(num_passes => count(test_results%passed()))
          print '(a,2(i0,a))'," ",num_passes," of ", num_tests," tests pass."
          passes = passes + num_passes
        end associate
      end associate
    end associate

  end procedure

end submodule test_s
