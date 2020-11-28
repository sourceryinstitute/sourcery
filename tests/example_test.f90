module example_test
    use Vegetables_m, only: Result_t, TestItem_t, describe, it, succeed

    implicit none
    private

    public :: test_example
contains
    function test_example() result(tests)
        type(TestItem_t) :: tests

        tests = describe(&
                "something", &
                [it( &
                        "does a thing", &
                        check_example)])
    end function

    function check_example() result(result_)
        type(Result_t) :: result_

        result_ = succeed("For now")
    end function
end module
