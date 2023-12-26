module sourcery_test_result_m
  !! Define an abstraction for describing test intentions and results
  implicit none

  private
  public :: test_result_t

  type test_result_t
    !! Encapsulate test descriptions and outcomes and reporting
    private
    character(len=:), allocatable :: description_
    logical passed_ 
  contains
    procedure :: characterize
    procedure :: passed
  end type

  interface test_result_t

    elemental module function construct(description, passed) result(test_result)
      !! The result is a test_result_t object with the components defined by the dummy arguments
      implicit none
      character(len=*), intent(in) :: description
      logical, intent(in) :: passed
      type(test_result_t) test_result 
    end function

  end interface

  interface

    pure module function characterize(self) result(characterization)
      !! The result is a character description of the test and its outcome
      implicit none
      class(test_result_t), intent(in) :: self
      character(len=:), allocatable :: characterization
    end function

    impure elemental module function passed(self) result(test_passed)
      !! The result is true if and only if the test passed on all images
      implicit none
      class(test_result_t), intent(in) :: self
      logical test_passed
    end function

  end interface

end module sourcery_test_result_m
