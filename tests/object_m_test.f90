module object_m_test
  !! Verify object pattern asbtract parent
  use test_m, only : test_t, test_result_t
  use object_m, only : object_t
  implicit none

  private
  public :: object_test_t

  type, extends(test_t) :: object_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  type, extends(object_t) :: subject_t
  contains
    procedure write_formatted
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The object_m type" 
  end function

  pure function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ &
      test_result_t("object being .not. user_defined() if it is only default-initialized", check_default_initialization()), &
      test_result_t("object being user_defined() after call to mark_as_defined", check_mark_as_defined()) &
    ]
  end function

  pure function check_default_initialization() result(passed)
    !! Verify that user_defined() is .false. for a default-initialied object
    class(object_t), allocatable :: object
    logical passed

    allocate(subject_t :: object)
    passed = .not. object%user_defined()
  end function

  pure function check_mark_as_defined() result(passed)
    !! Verify that mark_as_defined results in user_defined() being .true.
    class(object_t), allocatable :: object
    logical passed

    allocate(subject_t :: object)
    call object%mark_as_defined
    passed = object%user_defined()
  end function

  subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
    class(subject_t), intent(in) :: self
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
        iomsg = "object_m_test: subject_t%write_formatted iotype received unsupported iotype " // iotype
    end select

    associate( unused => v_list)
    end associate
  end subroutine

end module object_m_test
