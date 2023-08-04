module string_test_m
  use sourcery_m, only : test_t, test_result_t, string_t
  implicit none

  private
  public :: string_test_t

  type, extends(test_t) :: string_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The string_t type"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ &
      test_result_t("is_allocated() result .true. if & only if the string_t component(s) is/are allocated", check_allocation()) &
    ]
  end function

  pure function check_allocation() result(passed)
    type(string_t) :: scalar_not_allocated, scalar_allocated, array_allocated(2), array_not_allocated(2)
    logical passed

    scalar_allocated = string_t("")
    array_allocated = [string_t("yada yada"), string_t("blah blah blah")]
    passed = (.not. any([scalar_not_allocated%is_allocated(), array_not_allocated%is_allocated()])) .and. &
             (all([scalar_allocated%is_allocated(), array_allocated%is_allocated()]))
  end function

end module string_test_m
