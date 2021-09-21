module oracle_interface
  !! verify actual output against expected
  use object_interface, only : object_t
  implicit none

  private
  public :: oracle_t

  type, abstract, extends(object_t) :: oracle_t
    !! define procedures for testing output values against expected values
  contains
    procedure(subtract_interface), deferred :: subtract
    procedure(norm_interface), deferred :: norm
    generic :: operator(-) => subtract
    procedure :: within_tolerance
  end type

  abstract interface

    function subtract_interface(self, rhs) result(difference)
      !! result has components corresponding to subtracting rhs's components fron self object's components
      import oracle_t
      implicit none
      class(oracle_t), intent(in) :: self, rhs
      class(oracle_t), allocatable :: difference
    end function

    pure function norm_interface(self) result(norm_of_self)
      !! result is a norm of the array formed by concatenating the real components of self object
      import oracle_t
      implicit none
      class(oracle_t), intent(in) :: self
      real norm_of_self
    end function

  end interface

  interface

    module function within_tolerance(self, reference, tolerance) result(in_tolerance)
      !! template method with true result iff the difference in state vectors (self - reference) has a norm within tolerance
      !! (impure because of internal call to 'subtract' binding)
      !! The existence of self procedure eliminates the need to rewrite similar code for every oracle child type.
      implicit none
      class(oracle_t), intent(in) :: self, reference
      real, intent(in) :: tolerance
      logical in_tolerance
    end function

  end interface

end module
