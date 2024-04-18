module user_defined_collectives_test_m
    use sourcery_m, only : co_all, test_t, test_result_t, test_description_t, test_description_substring, test_function_i, string_t
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
    type(test_description_t), allocatable :: test_descriptions(:)

#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t &
        (string_t("setting all arguments to .true. when previously .true. on all images"), check_co_all_with_all_true), &
      test_description_t
        (string_t("setting all arguments to .false. when previously .false. on image 1"), check_co_all_with_one_false) &
    ]   
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: check_co_all_true_ptr, check_co_all_one_false_ptr 
    check_co_all_true_ptr => check_co_all_with_all_true
    check_co_all_one_false_ptr => check_co_all_with_one_false
    test_descriptions = [ &
      test_description_t &
        (string_t("setting all arguments to .true. when previously .true. on all images"), check_co_all_true_ptr), &
      test_description_t &
        (string_t("setting all arguments to .false. when previously .false. on image 1"), check_co_all_one_false_ptr) &
    ]
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
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

end module user_defined_collectives_test_m
