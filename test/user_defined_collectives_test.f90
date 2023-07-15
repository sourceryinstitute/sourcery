module user_defined_collectives_test
    use sourcery_m, only : co_all, test_t, test_result_t
    implicit none

    private
    public :: collectives_test_t

    type, extends(test_t) :: collectives_test_t
    contains
      procedure, nopass :: subject
      procedure, nopass :: results
    end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The co_all subroutine" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ &
      test_result_t("setting all arguments to .true. when previously .true. on all images", check_co_all_with_all_true()), &
      test_result_t("setting all arguments to .false. when previously .false. on image 1", check_co_all_with_one_false()) &
    ]
  end function

  function check_co_all_with_all_true() result(test_passed)
    logical test_passed, all_true

    all_true=.true.
    call co_all(all_true)
    test_passed = all_true
  end function

  function check_co_all_with_one_false() result(test_passed)
    logical test_passed, all_true

    all_true = merge(.false., .true., this_image()==1)
    call co_all(all_true)
    test_passed = .not. all_true
  end function

end module user_defined_collectives_test
