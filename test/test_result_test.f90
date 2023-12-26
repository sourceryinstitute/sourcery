module test_result_test_m
  !! Verify object pattern asbtract parent
  use sourcery_test_m, only : test_t, test_result_t
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

    test_results = [ &
      test_result_t("constructing an array of test_result_t objects elementally", check_array_result_construction()), &
      test_result_t("reporting failure if the test fails on one image", check_single_image_failure()) &
    ]
  end function

  pure function check_array_result_construction() result(passed)
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
