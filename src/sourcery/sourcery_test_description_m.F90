module sourcery_test_description_m
  !! Define an abstraction for describing test intentions and test functions
  use sourcery_string_m, only : string_t
  use sourcery_test_result_m, only : test_result_t
  implicit none

  private
  public :: test_description_t
#ifdef __GFORTRAN__
  public :: test_function_i
#endif

  abstract interface
    function test_function_i() result(passes)
      implicit none
      logical passes
    end function
  end interface

  type test_description_t
    !! Encapsulate test descriptions and test-functions
    private
    type(string_t) description_
    procedure(test_function_i), pointer, nopass :: test_function_ => null()
  contains
    procedure run
  end type

  interface test_description_t

    module function construct(description, test_function) result(test_description)
      !! The result is a test_description_t object with the components defined by the dummy arguments
      implicit none
      type(string_t), intent(in) :: description
      procedure(test_function_i), intent(in), pointer :: test_function
      type(test_description_t) test_description
    end function

  end interface

  interface

    impure elemental module function run(self) result(test_result)
      !! The result encapsulates the test description and test outcome
      implicit none
      class(test_description_t), intent(in) :: self
      type(test_result_t) test_result
    end function

  end interface

end module sourcery_test_description_m