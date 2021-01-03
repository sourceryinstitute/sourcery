module oracle_interface
  !! verify actual output against expected
  use object_interface, only : object
  implicit none

  private
  public :: oracle

  type, abstract, extends(object) :: oracle
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
      import oracle
      implicit none
      class(oracle), intent(in) :: this, rhs
      class(oracle), allocatable :: difference
    end function

    pure function norm_interface(this) result(norm_of_this)
      !! result is a norm of the array formed by concatenating the real components of this object
      import oracle
      implicit none
      class(oracle), intent(in) :: this
      real norm_of_this
    end function

  end interface

  interface

    module function within_tolerance(this, reference, tolerance) result(in_tolerance)
      !! template method with true result iff the difference in state vectors (this - reference) has a norm within tolerance
      !! (impure because of internal call to 'subtract' binding)
      !! The existence of this procedure eliminates the need to rewrite similar code for every oracle child type.
      implicit none
      class(oracle), intent(in) :: this, reference
      real, intent(in) :: tolerance
      logical in_tolerance
    end function

  end interface

end module
