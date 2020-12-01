module collective_subroutines_test
    use Vegetables, only: Result_t, Test_Item_t, describe, it, assert_equals, assert_that, assert_not
    use emulated_intrinsics_interface, only : &
#ifdef COMPILER_LACKS_COLLECTIVE_SUBROUTINES
    co_all, co_sum
#else
    co_all
#endif

    implicit none
    private

    public :: test_co_all
    public :: test_co_sum

contains

    function test_co_all() result(tests)
        type(Test_Item_t) :: tests

        tests = describe( &
                "co_all", &
                [it( &
                        "sets all arguments to .true. when previously .true. on all images", &
                        check_co_all_with_all_true), &
                 it( &
                        "sets all arguments to .false. when previously .false. on image 1", &
                        check_co_all_with_one_false)])
    end function

    function check_co_all_with_all_true() result(result_)
        type(Result_t) :: result_
        logical all_true

        all_true=.true.

        call co_all(all_true)
        result_ = assert_that(all_true, "co_all argument remains .true. after call with all arguments .true.")
    end function

    function check_co_all_with_one_false() result(result_)
        type(Result_t) :: result_
        logical all_true

        all_true = merge(.false., .true., this_image()==1)
        call co_all(all_true)
        result_ = assert_not(all_true, "co_all argument is .false. after call with one argument .false.")
    end function

    function test_co_sum() result(tests)
        type(Test_Item_t) :: tests

        tests = describe( &
                "co_sum", &
                [it( &
                        "gives sums with result_image present", &
                        check_co_sum_with_result_image), &
                 it( &
                        "gives sums without result_image present", &
                        check_co_sum_without_result_image)])
    end function

    function check_co_sum_with_result_image() result(result_)
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
