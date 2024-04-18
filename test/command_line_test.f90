module command_line_test_m
  !! Verify object pattern asbtract parent
  use sourcery_m, only : &
    test_t, test_result_t, command_line_t, test_description_substring, test_function_i, string_t, test_description_t
  implicit none

  private
  public :: command_line_test_t

  type, extends(test_t) :: command_line_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The command_line_t type" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)
    type(test_description_t), allocatable :: test_descriptions(:)
#ifndef __GFORTRAN__
    test_descriptions = [ & 
      test_description_t(string_t("returning the value passed after a command-line flag"), check_flag_value) &
    ]   
#else
    ! Work around missing Fortran 2008 feature: associating a procedure actual argument with a procedure pointer dummy argument:
    procedure(test_function_i), pointer :: check_flag_ptr
    check_flag_ptr => check_flag_value 
    test_descriptions = [ & 
      test_description_t(string_t("returning the value passed after a command-line flag"), check_flag_ptr) &
    ]   
#endif
    test_descriptions = pack(test_descriptions, test_descriptions%contains_text(string_t(test_description_substring)))
    test_results = test_descriptions%run()
  end function

  function check_flag_value() result(test_passes)
    logical test_passes

    integer exit_status, command_status
    character(len=132) command_message

    call execute_command_line( &
      command = "fpm run --example get-flag-value -- --input-file some_file_name", &
      wait = .true., exitstat = exit_status, cmdstat = command_status, cmdmsg = command_message &
    )   
    test_passes = exit_status == 0 

  end function

end module command_line_test_m
