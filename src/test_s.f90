submodule(sourcery_test_m) test_s
  use sourcery_m, only : co_all
  implicit none

contains

  module procedure report

    associate(me => this_image())
      if (me==1) print *, new_line('a'), test%subject()
      associate(test_results => test%results())
        associate(num_tests => size(test_results))
          tests = tests + num_tests
          if (me==1) then
            block
              integer i
              do i=1,num_tests
                if (me==1) print *,"   ",test_results(i)%characterize()
              end do
            end block
          end if
          block 
            logical, allocatable :: passing_tests(:)
            passing_tests = test_results%passed()
            call co_all(passing_tests)
            associate(num_passes => count(passing_tests))
              if (me==1) print '(a,2(i0,a))'," ",num_passes," of ", num_tests," tests pass."
              passes = passes + num_passes
            end associate
          end block
        end associate
      end associate
    end associate

  end procedure

end submodule test_s
