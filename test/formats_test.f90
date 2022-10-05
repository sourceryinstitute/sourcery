module formats_test
  !! Verify that format strings provide the desired formatting
  use formats_m, only : separated_values
  use test_m, only : test_t, test_result_t
  implicit none

  private
  public :: formats_test_t

  type, extends(test_t) :: formats_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The csv format" 
  end function

  pure function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ &
      test_result_t("yielding a comma-separated list of real numbers", check_csv_reals()), &
      test_result_t("yielding a space-separated list of complex numbers", check_space_separated_complex()), &
      test_result_t("yielding a comma- and space-separated list of character values", check_csv_character()), &
      test_result_t("yielding a new-line-separated list of integer numbers", check_new_line_separated_integers()) &
    ]
  end function

  pure function check_csv_reals() result(test_passes)
    logical test_passes
    character(len=*), parameter :: expected_output =  "0.00000000,1.00000000,2.00000000"
    character(len=len(expected_output)) captured_output 

    write(captured_output, fmt = separated_values(separator=",", mold=[integer::])) [0.,1.,2.]
    test_passes = expected_output == captured_output
  end function

  pure function check_space_separated_complex() result(test_passes)
    logical test_passes 
    character(len=*), parameter :: expected_output = "(0.00000000,1.00000000) (1.00000000,0.00000000)"
    character(len=len(expected_output)) captured_output 

    write(captured_output, fmt = separated_values(separator=" ", mold=[complex::])) [(0.,1.),(1.,0.)]
    test_passes = expected_output == captured_output
  end function

  pure function check_new_line_separated_integers() result(test_passes)
    logical test_passes
    character(len=*), parameter :: expected_output = ( "0" // new_line("") // "1" //new_line("") // "2")
    character(len=len(expected_output)) captured_output 

    write(captured_output, fmt = separated_values(separator=new_line(""), mold=[integer::])) [0,1,2]
    test_passes = captured_output == "0" // new_line("") // "1" //new_line("") // "2"
  end function

  pure function check_csv_character() result(test_passes)
    logical test_passes
    integer, parameter :: num_spaces=3
    character(len=*), parameter :: expected_output = "Yodel, Ay, Hee, Hoo!"
    character(len=len(expected_output)+num_spaces) captured_output 

    write(captured_output, fmt = separated_values(separator=", ", mold=[integer::])) "Yodel", "Ay", "Hee", "Hoo!"
    test_passes= expected_output == captured_output
  end function

end module formats_test
