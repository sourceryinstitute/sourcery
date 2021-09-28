module formats_test

   !! author: Damian Rouson
   !!
   !! summary: verify that format strings provide the desired formatting
   use vegetables, only: &
     result_t, test_item_t, &   ! types
     describe, it, assert_equals ! functions
   use formats_m, only : separated_values
   implicit none

   private
   public :: test_object

contains

  function test_object() result(tests)
    type(test_item_t) tests

    tests = describe( &
     "csv format", &
     [it( &
       "yields a comma-separated list of real numbers", &
       check_csv_reals), &
      it( &
       "yields a space-separated list of complex numbers", &
       check_space_separated_complex), &
      it( &
       "yields a comma- and space-separated list of character values", &
       check_cssv_character), &
      it( &
       "yields a new-line-separated list of integer numbers", &
       check_new_line_separated_integers)])
  end function

  function check_csv_reals() result(result_)
    type(result_t) result_
    character(len=*), parameter :: expected_output =  "0.00000000,1.00000000,2.00000000"
    character(len=len(expected_output)) captured_output 

    write(captured_output, fmt = separated_values(separator=",", mold=[integer::])) [0.,1.,2.]

    result_ = assert_equals(expected_output, captured_output)
  end function

  function check_space_separated_complex() result(result_)
    type(result_t) result_

    character(len=*), parameter :: expected_output = "(0.00000000,1.00000000) (1.00000000,0.00000000)"
    character(len=len(expected_output)) captured_output 

    write(captured_output, fmt = separated_values(separator=" ", mold=[complex::])) [(0.,1.),(1.,0.)]

    result_ = assert_equals(expected_output, captured_output)
  end function

  function check_new_line_separated_integers() result(result_)
    type(result_t) result_

    character(len=*), parameter :: expected_output = ( "0" // new_line("") // "1" //new_line("") // "2")
    character(len=len(expected_output)) captured_output 

    write(captured_output, fmt = separated_values(separator=new_line(""), mold=[integer::])) [0,1,2]

    result_ = assert_equals(captured_output, "0" // new_line("") // "1" //new_line("") // "2")
  end function

  function check_cssv_character() result(result_)
    type(result_t) result_

    integer, parameter :: num_spaces=3
    character(len=*), parameter :: expected_output = "Yodel, Ay, Hee, Hoo!"
    character(len=len(expected_output)+num_spaces) captured_output 

    write(captured_output, fmt = separated_values(separator=", ", mold=[integer::])) "Yodel", "Ay", "Hee", "Hoo!"

    result_ = assert_equals(expected_output, captured_output)
  end function

end module formats_test
