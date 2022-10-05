module object_m_test
   !! author: Damian Rouson
   !!
   !! summary: verify object pattern asbtract parent
   use vegetables, only: &
     result_t, input_t, integer_input_t, test_item_t, &   ! types
     describe, it, assert_equals, assert_that, assert_not ! functions
   use object_m, only : object_t
   implicit none

   private
   public :: test_object

   type, extends(object_t) :: subject
   contains
     procedure write_formatted
   end type

contains

  function test_object() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "object class", &
     [it( &
       ".not. user_defined() if only default-initialized", &
       check_default_initialization), &
      it( &
       "user_defined() after call mark_as_defined", &
       check_mark_as_defined)])
  end function

  function check_default_initialization() result(result_)
    !! Verify that user_defined() is .false. for a default-initialied object
   class(object_t), allocatable :: object
    type(result_t) result_

    allocate(subject :: object)

    result_ = assert_not(object%user_defined())
  end function

  function check_mark_as_defined() result(result_)
    !! Verify that mark_as_defined results in user_defined() being .true.
    class(object_t), allocatable :: object
    type(result_t) result_

    allocate(subject :: object)

    call object%mark_as_defined
    result_ = assert_that(object%user_defined())
  end function

  subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
    class(subject), intent(in) :: self
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    select case(iotype)
      case('LISTDIRECTED')
        write(unit,*) self%user_defined()
        iostat = 0
        iomsg = ""
      case default
        iostat = -1
        iomsg = "object_m_test: subject%write_formatted iotype received unsupported iotype " // iotype
    end select

    associate( unused => v_list)
    end associate
  end subroutine

end module object_m_test
