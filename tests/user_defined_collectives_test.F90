module user_defined_collectives_test
    use Vegetables, only: Result_t, Test_Item_t, describe, it, assert_equals, assert_that, assert_not
    use user_defined_collectives_m, only : co_all
    
#ifdef USE_CAFFEINE
   use caffeine_m, only : this_image => caf_this_image
#endif
    
    implicit none

    private
    public :: test_co_all

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

end module user_defined_collectives_test
