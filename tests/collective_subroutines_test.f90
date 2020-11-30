module collective_subroutines_test
    use Vegetables, only: Result_t, Test_Item_t, describe, it, succeed, assert_equals

    implicit none
    private

    public :: test_collective_subroutines
contains
    function test_collective_subroutines() result(tests)
        type(Test_Item_t) :: tests

        tests = describe(&
                "co_sum", &
                [it( &
                        "gives the correct answer with result_image present", &
                        check_co_sum_with_result_image), &
                 it( &
                        "gives the correct answer without result_image present", &
                        check_co_sum_without_result_image)])
    end function

    function check_co_sum_with_result_image() result(result_)
        use emulated_intrinsics_interface, only : co_sum
        type(Result_t) :: result_

        integer i, j
        integer, parameter :: result_image=2

        associate(me => this_image())
          i = me
          call co_sum(i, result_image)
          if (me==result_image) then
            result_ = assert_equals(sum([(j, j=1, num_images())]), i, "collective sum on result_image")
          else
            result_ = assert_equals(me, i, "co_sum argument unchanged on non-result_image")
          end if
        end associate
    end function

    function check_co_sum_without_result_image() result(result_)
        use emulated_intrinsics_interface, only : co_sum
        type(Result_t) :: result_

        integer i, j
        integer, parameter :: result_image=2

        associate(me => this_image())
          i = me
          call co_sum(i)
          result_ = assert_equals(sum([(j, j=1, num_images())]), i, "co_sum without result_image present")
        end associate
    end function

end module
