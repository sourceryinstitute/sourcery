! ### Copyright notice
!
!     ```
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!     ```
module object_interface
  implicit none

  private
  public :: object_t

  type, abstract :: object_t
    !! author: Damian Rouson, GSE LLC
    !! category: Morfeus-FD
    !! summary: Abstract type to ensure all objects extending it implement the required methods
    !!
    !! Define an abstract parent type to ensure basic functionality expected to be provided by all non-abstract types.
    !! Each non-abstract type provides the functionality by extending self type and implementing its deferred binding(s).  This
    !! type resembles java's Object class in the sense that it is intended to be the ultimate ancestor of every other type.
    private
    logical :: defined=.false.
      !! Default initialization indicates not yet user-defined
  contains
    procedure :: mark_as_defined
    procedure :: user_defined
    procedure(write_interface), deferred :: write_formatted
    generic :: write(formatted) => write_formatted
  end type

  interface

    pure module subroutine mark_as_defined(self)
      !! Mark the object as user-defined
      implicit none
      class(object_t), intent(inout) :: self
    end subroutine

    pure module function user_defined(self) result(is_defined)
      !! Return a boolean result indicating whether self object has been initialized since its declaration
      implicit none
      class(object_t), intent(in) :: self
      logical :: is_defined
    end function

  end interface

  abstract interface
    subroutine write_interface(self, unit, iotype, v_list, iostat, iomsg)
      import object_t
      implicit none
      class(object_t), intent(in) :: self
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine
  end interface

end module object_interface
