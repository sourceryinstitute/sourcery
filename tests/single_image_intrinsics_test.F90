!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
module single_image_intrinsics_test
    use Vegetables, only: Result_t, Test_Item_t, describe, it, assert_equals
#ifdef COMPILER_LACKS_FINDLOC
    use emulated_intrinsics_interface, only : findloc
#endif

    implicit none
    private
    public :: test_findloc

contains

    function test_findloc() result(tests)
        type(Test_Item_t) :: tests

        tests = describe( &
                "findloc", &
                [it( &
                        "handles zero-sized argument", &
                        check_zero_sized_argument), &
                 it( &
                        "handles absent back argument", &
                        check_absent_back), &
                 it( &
                        "handles .false. back argument", &
                        check_false_back), &
                 it( &
                        "handles .true. back argument", &
                        check_true_back), &
                 it( &
                        "handles absent back argument", &
                        check_logical_argument), &
                 it( &
                        "handles character array argument", &
                        check_character_array), &
                 it( &
                        "handles empty character array argument", &
                        check_empty_character_array), &
                 it( &
                        "handles failed search", &
                        check_nonexistent_character_value)])
     end function

    function check_zero_sized_argument() result(result_)
        type(Result_t) :: result_
        integer, parameter :: zero_sized_array(*) = [ integer :: ]
        result_ = assert_equals(0, findloc(zero_sized_array, value=99, dim=1, back=.true.), "findloc handles zero-sized array")
    end function

    function check_absent_back() result(result_)
        type(Result_t) :: result_
        result_ = assert_equals(3, findloc([1,2,3,4], value=3, dim=1), "findloc handles absent 'back' argument")
    end function

    function check_false_back() result(result_)
        type(Result_t) :: result_
        result_ = assert_equals(2, findloc([1,2,3,4], value=2, dim=1, back=.false.), "findloc handles .false. 'back' argument")
    end function

    function check_true_back() result(result_)
        type(Result_t) :: result_
        result_ = assert_equals(1, findloc([1,2,3,4], value=1, dim=1, back=.true.), "findloc handles .true. 'back' argument")
    end function

  function check_logical_argument() result(result_)
    type(Result_t) :: result_
    logical, parameter :: first_true(*) = [.true., .false., .false.]
    result_ = assert_equals(1, findloc(first_true, value=.true., dim=1, back=.true.), "findloc handles logical argument")
  end function

  function check_character_array() result(result_)
    type(Result_t) :: result_
    character(len=*), parameter, dimension(*) :: rgb = ["roy", "gee", "biv"]
    result_ = assert_equals(2, findloc(rgb, value="gee", dim=1),  "findloc finds string location")
  end function

  function check_empty_character_array() result(result_)
    type(Result_t) :: result_
    character(len=*), parameter, dimension(*) :: empty = [character(len=len("hello"))::]
    result_ = &
      assert_equals(0, findloc(empty, value="hello", dim=1, back=.false.), "findloc handles empty character array from front")
  end function

  function check_nonexistent_character_value() result(result_)
    type(Result_t) :: result_
    character(len=*), parameter, dimension(*) :: unsuccessful = ["foo", "bar", "too"]
    result_ = assert_equals(0, &
      findloc(unsuccessful, value="foobar", dim=1, back=.false.), "findloc handles character array without search target")
  end function

end module
