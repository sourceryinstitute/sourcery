module sourcery_vector_test_description_m
  !! Define an abstraction for describing test intentions and array-valued test functions
  use sourcery_string_m, only : string_t
  use sourcery_test_result_m, only : test_result_t
  use assert_m, only : assert
  implicit none

  private
  public :: vector_test_description_t
  public :: vector_function_strategy_t

  abstract interface
    function vector_function_i() result(passes)
      implicit none
      logical, allocatable :: passes(:)
    end function
  end interface

  type, abstract :: vector_function_strategy_t
  contains
    procedure(vector_function_i), deferred, nopass ::  vector_function
  end type

  type vector_test_description_t
    !! Encapsulate test descriptions and vector-valued test functions
    private
    type(string_t), allocatable :: description_vector_(:)
    class(vector_function_strategy_t), allocatable :: vector_function_strategy_
  contains
    procedure run
    procedure contains_text
  end type

  interface vector_test_description_t

    module function construct(description_vector, vector_function_strategy) result(vector_test_description)
     !! The result is a vector_test_description_t object with the components defined by the dummy arguments
      implicit none
      type(string_t), intent(in) :: description_vector(:)
      class(vector_function_strategy_t), intent(in) :: vector_function_strategy
      type(vector_test_description_t) vector_test_description
    end function

  end interface

  interface

    impure module function run(self) result(test_results)
      !! The result encapsulates the test description and test outcome
      implicit none
      class(vector_test_description_t), intent(in) :: self
      type(test_result_t), allocatable :: test_results(:)
    end function

    module function contains_text(self, substring) result(match_vector)
      !! The result is .true. if the test description includes the value of substring 
      implicit none
      class(vector_test_description_t), intent(in) :: self
      character(len=*), intent(in) :: substring
      logical, allocatable :: match_vector(:)
    end function

  end interface

contains

  module procedure contains_text
    integer i
    associate(num_descriptions => size(self%description_vector_))
      allocate(match_vector(num_descriptions))
      do i = 1, num_descriptions
        match_vector(i) = index(self%description_vector_(i)%string(), substring ) /= 0
      end do
    end associate
  end procedure

  module procedure construct
    vector_test_description%description_vector_ = description_vector
    vector_test_description%vector_function_strategy_ = vector_function_strategy
  end procedure

  module procedure run
    associate(vector_result => self%vector_function_strategy_%vector_function())
      call assert(size(self%description_vector_)==size(vector_result), "sourcery_vector_test_description_s: size match")
      test_results = test_result_t(self%description_vector_, vector_result)
    end associate
  end procedure

end module sourcery_vector_test_description_m
