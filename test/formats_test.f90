module formats_test_m
  !! Verify that format strings provide the desired formatting
  use sourcery_m, only : separated_values, test_t, test_result_t
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
    specimen = "A format string" 
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ &
      test_result_t("yielding a comma-separated list of real numbers", check_csv_reals()), &
      test_result_t("yielding a space-separated list of complex numbers", check_space_separated_complex()), &
      test_result_t("yielding a comma- and space-separated list of character values", check_csv_character()), &
      test_result_t("yielding a new-line-separated list of integer numbers", check_new_line_separated_integers()) &
    ]
  end function

  function check_csv_reals() result(test_passes)
    logical test_passes
    character(len=100) captured_output 
    real zero, one, two

    write(captured_output, fmt = separated_values(separator=",", mold=[integer::])) [0.,1.,2.]

    associate(first_comma => index(captured_output, ','))
      associate(second_comma => first_comma + index(captured_output(first_comma+1:), ','))
        read(captured_output(:first_comma-1), *) zero
        read(captured_output(first_comma+1:second_comma-1), *) one
        read(captured_output(second_comma+1:), *) two
        test_passes = (zero==0.) .and. (one==1.) .and. (two==2.)
      end associate
    end associate
  end function

  function check_space_separated_complex() result(test_passes)
    logical test_passes 
    character(len=100) captured_output 
    character(len=:), allocatable :: i_string, one_string
    complex, parameter :: i = (0.,1.), one = (1.,0.)
    complex i_read, one_read

    write(captured_output, fmt = separated_values(separator=" ", mold=[complex::])) i,one

    i_string = captured_output(:index(captured_output,")"))
    one_string = captured_output(len(i_string)+1:)

    read(i_string,*) i_read
    read(one_string,*) one_read
    
    test_passes = i_read == i .and. one_read == one
  end function

  function check_csv_character() result(test_passes)
    logical test_passes
    character(len=200) captured_output 
    character(len=*), parameter :: expected_output = "Yodel, Ay, Hee, Hoo!"

    write(captured_output, fmt = separated_values(separator=", ", mold=[integer::])) "Yodel", "Ay", "Hee", "Hoo!"
    test_passes = expected_output == captured_output
  end function

  function check_new_line_separated_integers() result(test_passes)
    logical test_passes
    character(len=100) captured_output 

    write(captured_output, fmt = separated_values(separator=new_line(""), mold=[integer::])) [0,1,2]
    test_passes = captured_output == "0" // new_line("") // "1" //new_line("") // "2"
  end function

end module formats_test_m
