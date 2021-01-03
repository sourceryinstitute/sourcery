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

    function subtract_interface(this, rhs) result(difference)
      !! result has components corresponding to subtracting rhs's components fron this object's components
      import oracle_t
      implicit none
      class(oracle_t), intent(in) :: this, rhs
      class(oracle_t), allocatable :: difference
    end function

    pure function norm_interface(this) result(norm_of_this)
      !! result is a norm of the array formed by concatenating the real components of this object
      import oracle_t
      implicit none
      class(oracle_t), intent(in) :: this
      real norm_of_this
    end function

  end interface

  interface

    module function within_tolerance(this, reference, tolerance) result(in_tolerance)
      !! template method with true result iff the difference in state vectors (this - reference) has a norm within tolerance
      !! (impure because of internal call to 'subtract' binding)
      !! The existence of this procedure eliminates the need to rewrite similar code for every oracle child type.
      implicit none
      class(oracle_t), intent(in) :: this, reference
      real, intent(in) :: tolerance
      logical in_tolerance
    end function

  end interface

end module
