module test_result_test_m
  !! Verify test_result_t object behavior
  use sourcery_m, only : string_t, test_result_t, test_description_t, test_t, test_description_substring
#ifdef __GFORTRAN__
  use sourcery_test_description_m, only : test_function_i
#endif
  implicit none

  private
  public :: test_result_test_t

  type, extends(test_t) :: test_result_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The test_result_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)

#ifndef __GFORTRAN__
    test_descriptions = [ &
      test_description_t(string_t("constructing an array of test_result_t objects elementally"), check_array_result_construction) &
      test_description_t(string_t("reporting failure if the test fails on one image"), check_single_image_failure) &
    ]
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: check_array_ptr, check_single_ptr
    check_array_ptr => check_array_result_construction
    check_single_ptr => check_single_image_failure
    test_descriptions = [ &
      test_description_t(string_t("constructing an array of test_result_t objects elementally"), check_array_ptr), &
      test_description_t(string_t("reporting failure if the test fails on one image"), check_single_ptr) &
    ]
#endif
    test_descriptions = pack(test_descriptions, &
      index(subject(), test_description_substring) /= 0 .or. &
      test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  function check_array_result_construction() result(passed)
    type(test_result_t), allocatable :: test_results(:)
    logical passed

    test_results = test_result_t(["foo","bar"], [.true.,.false.])
    passed = size(test_results)==2
  end function

  function check_single_image_failure() result(passed)
    type(test_result_t), allocatable :: test_result
    logical passed

    if (this_image()==1) then
      test_result = test_result_t("image 1 fails", .false.)
    else
      test_result = test_result_t("all images other than 1 pass", .true.)
    end if

    passed = .not. test_result%passed()
  end function

end module test_result_test_m