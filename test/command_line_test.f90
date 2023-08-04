module command_line_test_m
  !! Verify object pattern asbtract parent
  use sourcery_m, only : test_t, test_result_t, command_line_t
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

    test_results = [ &
      test_result_t("returning the value passed after a command-line flag", check_flag_value()) &
    ]
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
