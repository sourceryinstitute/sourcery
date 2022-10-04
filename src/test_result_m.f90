module test_result_m
  !! Define an abstraction for describing test intentions and results
  implicit none

  private
  public :: test_result_t

  type test_result_t
    !! Encapsulate test descriptions and outcomes and reporting
    private
    character(len=:), allocatable :: description_
    logical outcome_
  contains
    procedure :: characterize
  end type

  interface test_result_t

    pure module function construct(description, outcome) result(test_result)
      !! The result is a test_result_t object with the components defined by the dummy arguments
      implicit none
      character(len=*), intent(in) :: description
      logical, intent(in) :: outcome
      type(test_result_t) test_result 
    end function

  end interface

  interface

    pure module function characterize(self) result(characterization)
      !! The result is a character test description and its outcome
      implicit none
      class(test_result_t), intent(in) :: self
      character(len=:), allocatable :: characterization
    end function

  end interface

end module test_result_m
