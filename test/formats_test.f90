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
    specimen = "The csv format" 
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
    character(len=*), parameter :: longest_expected_output =  "0.00000000,1.00000000,2.00000000"
    character(len=len(longest_expected_output)) captured_output 
    character(len=:), allocatable :: substring 

    write(captured_output, fmt = separated_values(separator=",", mold=[integer::])) [0.,1.,2.]

    substring = adjustl(trim(captured_output))
    associate(leading_real_zero => index(substring, "0.") == 1)
      substring = adjustl(substring(index(substring, ",")+1:)) ! get remaining string after fierst comma
      associate(followed_by_real_one => index(substring, "1.") == 1 )
        substring = adjustl(substring(index(substring, ",")+1:)) ! get remaining string after fierst comma
        associate(followed_by_real_two => index(substring, "2.") == 1 )
          test_passes = leading_real_zero .and. followed_by_real_one .and.followed_by_real_two
        end associate
      end associate
    end associate
  end function

  function check_space_separated_complex() result(test_passes)
    logical test_passes 
    character(len=*), parameter :: longest_expected_output = "(0.00000000,1.00000000) (1.00000000,0.00000000)"
    character(len=len(longest_expected_output)) captured_output 
    complex, parameter :: i = (0.,1.), one = (1.,0.)
    character(len=:), allocatable :: i_imag_part, one_imag_part, i_string, one_string

    write(captured_output, fmt = separated_values(separator=" ", mold=[complex::])) i,one

    i_string = adjustl(trim(captured_output(:index(captured_output,")"))))
    one_string = adjustl(trim(captured_output(len(i_string)+1:)))

    associate( &
      i_real_part_zero => index(i_string,"(0.") == 1, &
      one_real_part_one => index(one_string,"(1.") == 1 &
    )
      i_imag_part = adjustl(i_string(index(i_string,",")+1:index(i_string,")")-1))
      one_imag_part = adjustl(one_string(index(one_string,",")+1:index(one_string,")")-1))
      associate( &
        i_imag_part_one => index(i_imag_part,"1.") == 1, &
        one_imag_part_zero => index(one_imag_part,"0.") == 1 &
      )
        test_passes = i_real_part_zero .and. i_imag_part_one .and. one_real_part_one .and. one_imag_part_zero
      end associate
    end associate
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

end module formats_test_m
