module sourcery_string_m
  use assert_m, only : characterizable_t
  implicit none
  
  private
  public :: string_t
  public :: array_of_strings

  type, extends(characterizable_t) :: string_t
    private
    character(len=:), allocatable :: string_
  contains
    procedure :: as_character
    generic :: string => as_character
    procedure :: is_allocated
    procedure :: get_json_key
    procedure :: get_json_string_value
    procedure :: get_json_logical_value
    procedure :: get_json_integer_value
    procedure :: get_json_integer_array_value
    generic :: get_json_value => &
                 get_json_string_value, get_json_logical_value, get_json_integer_array_value, get_json_integer_value
    procedure :: equivalent
    generic :: operator(==) => equivalent
  end type

  interface string_t

    elemental module function construct(string) result(new_string)
      implicit none
      character(len=*), intent(in) :: string
      type(string_t) new_string
    end function

  end interface

  interface

    pure module function as_character(self) result(raw_string)
      implicit none
      class(string_t), intent(in) :: self
      character(len=:), allocatable :: raw_string
    end function

    pure module function array_of_strings(delimited_strings, delimiter) result(strings_array)
      implicit none
      character(len=*), intent(in) :: delimited_strings, delimiter
      type(string_t), allocatable :: strings_array(:)
    end function

    elemental module function is_allocated(self) result(string_allocated)
      implicit none
      class(string_t), intent(in) :: self
      logical string_allocated
    end function

    elemental module function get_json_key(self) result(unquoted_key)
     implicit none
      class(string_t), intent(in) :: self
      type(string_t) unquoted_key
    end function

    elemental module function get_json_string_value(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key, mold
      type(string_t) :: value_
    end function

    elemental module function get_json_integer_value(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) ::  mold
      integer value_
    end function

    elemental module function get_json_logical_value(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      logical, intent(in) :: mold
      logical value_
    end function

    pure module function get_json_integer_array_value(self, key, mold) result(value_)
      implicit none
      class(string_t), intent(in) :: self, key
      integer, intent(in) :: mold(:)
      integer, allocatable :: value_(:)
    end function

    elemental module function equivalent(lhs, rhs) result(lhs_eqv_rhs)
      implicit none
      class(string_t), intent(in) :: lhs, rhs
      logical lhs_eqv_rhs
    end function

  end interface
  
end module sourcery_string_m
