module string_test_m
  use sourcery_m, only : test_t, test_result_t, string_t
  implicit none


  private
  public :: string_test_t

  type, extends(test_t) :: string_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

contains

  pure function subject() result(specimen)
    character(len=:), allocatable :: specimen
    specimen = "The string_t type"
  end function

  function results() result(test_results)
    type(test_result_t), allocatable :: test_results(:)

    test_results = [ &
      test_result_t("is_allocated() result .true. if & only if the string_t component(s) is/are allocated", check_allocation()), &
      test_result_t("extracting a key string from colon-separated key/value pair", extracts_key()), &
      test_result_t("extracting a string value from colon-separated key/value pair", extracts_string_scalar_value()), &
      test_result_t("extracting a logical value from colon-separated key/value pair", extracts_logical_scalar_value()), &
      test_result_t("extracting an integer array value from colon-separated key/value pair", extracts_integer_array_value()) &
    ]
  end function

  pure function check_allocation() result(passed)
    type(string_t) :: scalar_not_allocated, scalar_allocated, array_allocated(2), array_not_allocated(2)
    logical passed

    scalar_allocated = string_t("")
    array_allocated = [string_t("yada yada"), string_t("blah blah blah")]
    passed = (.not. any([scalar_not_allocated%is_allocated(), array_not_allocated%is_allocated()])) .and. &
             (all([scalar_allocated%is_allocated(), array_allocated%is_allocated()]))
  end function

  function extracts_key() result(passed)
    logical passed
    
    associate(line => string_t('"foo" : "bar"'))
      passed = line%get_json_key() == string_t("foo")
    end associate
  end function

  function extracts_string_scalar_value() result(passed)
    logical passed
    
    associate(line => string_t('"foo" : "bar"'))
      passed = line%get_json_value(key=string_t("foo"), mold=string_t("")) == string_t("bar")
    end associate
  end function

  function extracts_logical_scalar_value() result(passed)
    logical passed
    
    associate( &
      key_true_pair => string_t('"yada yada" : true'), &
      key_false_pair => string_t('"blah blah" : false'), &
      trailing_comma => string_t('"trailing comma" : true,') &
    )
      associate( &
         true => key_true_pair%get_json_value(key=string_t("yada yada"), mold=.true.), &
         false => key_false_pair%get_json_value(key=string_t("blah blah"), mold=.true.), &
         true_too => trailing_comma%get_json_value(key=string_t("trailing comma"), mold=.true.) &
      )
        passed = true .and. true_too .and. .not. false
      end associate
    end associate
  end function

  function extracts_integer_array_value() result(passed)
    logical passed

    associate(key_integer_array_pair => string_t('"some key" : [1, 2, 3],'))
      associate(integer_array => key_integer_array_pair%get_json_value(key=string_t("some key"), mold=[integer::]))
        passed = all(integer_array == [1, 2, 3])
      end associate
    end associate
  end function

end module string_test_m
