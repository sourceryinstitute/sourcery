module string_test_m
  use sourcery_m, only : test_t, test_result_t, string_t, operator(.cat.)
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
      test_result_t('supporting operator(==) for string_t and character operands', supports_equivalence_operator()), &
      test_result_t('supporting operator(/=) for string_t and character operands', supports_non_equivalence_operator()), &
      test_result_t('supporting operator(//) for string_t and character operands', supports_concatenation_operator()), &
      test_result_t('assigning a string_t object to a character variable', assigns_string_t_to_character()), &
      test_result_t('assigning a character variable to a string_t object', assigns_character_to_string_t()), &
      test_result_t('constructing from a default integer', constructs_from_default_integer()), &
      test_result_t('constructing from a real value', constructs_from_real()), &
      test_result_t('supporting unary operator(.cat.) for array arguments', concatenates_elements()), &
      test_result_t("extracting a key string from a colon-separated key/value pair", extracts_key()), &
      test_result_t("extracting a real value from a colon-separated key/value pair", extracts_real_value()), &
      test_result_t("extracting a string value from a colon-separated key/value pair", extracts_string_value()), &
      test_result_t("extracting a logical value from a colon-separated key/value pair", extracts_logical_value()), &
      test_result_t("extracting an integer array value from a colon-separated key/value pair", extracts_integer_array_value()), &
      test_result_t("extracting an integer value from a colon-separated key/value pair", extracts_integer_value()), &
      test_result_t('extracting a file base name', extracts_file_base_name()), &
      test_result_t('extracting a file name extension', extracts_file_name_extension()) &
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

#ifndef _CRAYFTN
    associate(line => string_t('"foo" : "bar"'))
      passed = line%get_json_key() == string_t("foo")
    end associate
#else
    block
      type(string_t) line
      line = string_t('"foo" : "bar"')
      passed = line%get_json_key() == string_t("foo")
    end block
#endif
  end function

  function extracts_real_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(line => string_t('"pi" : 3.14159'))
      passed = line%get_json_value(key=string_t("pi"), mold=1.) == 3.14159
    end associate
#else
    block
      type(string_t) line
      line = string_t('"pi" : 3.14159')
      passed = line%get_json_value(key=string_t("pi"), mold=1.) == 3.14159
    end block
#endif
  end function

  function extracts_string_value() result(passed)
    logical passed
    
#ifndef _CRAYFTN
    associate(line => string_t('"foo" : "bar"'))
      passed = line%get_json_value(key=string_t("foo"), mold=string_t("")) == string_t("bar")
    end associate
#else
    block
      type(string_t) line
      line = string_t('"foo" : "bar"')
      passed = line%get_json_value(key=string_t("foo"), mold=string_t("")) == string_t("bar")
    end block
#endif
  end function

  function extracts_integer_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(line => string_t('"an integer" : 99'))
      passed = line%get_json_value(key=string_t("an integer"), mold=0) == 99
    end associate
#else
    block
      type(string_t) line
      line = string_t('"an integer" : 99')
      passed = line%get_json_value(key=string_t("an integer"), mold=0) == 99
    end block
#endif
  end function

  function extracts_logical_value() result(passed)
    logical passed
    
#ifndef _CRAYFTN
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
#else
    block
      type(string_t) key_true_pair, key_false_pair, trailing_comma
      logical  true, false, true_too

      key_true_pair = string_t('"yada yada" : true')
      key_false_pair = string_t('"blah blah" : false')
      trailing_comma = string_t('"trailing comma" : true,')

      true = key_true_pair%get_json_value(key=string_t("yada yada"), mold=.true.)
      false = key_false_pair%get_json_value(key=string_t("blah blah"), mold=.true.)
      true_too = trailing_comma%get_json_value(key=string_t("trailing comma"), mold=.true.)

      passed = true .and. true_too .and. .not. false
    end block
#endif
  end function

  function extracts_integer_array_value() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(key_integer_array_pair => string_t('"some key" : [1, 2, 3],'))
      associate(integer_array => key_integer_array_pair%get_json_value(key=string_t("some key"), mold=[integer::]))
        passed = all(integer_array == [1, 2, 3])
      end associate
    end associate
#else
    block
      type(string_t) key_integer_array_pair
      integer, allocatable :: integer_array(:)
      key_integer_array_pair = string_t('"some key" : [1, 2, 3],')
      integer_array = key_integer_array_pair%get_json_value(key=string_t("some key"), mold=[integer::])
      passed = all(integer_array == [1, 2, 3])
    end block
#endif
  end function

  function supports_equivalence_operator() result(passed)
    logical passed
    passed = &
      string_t("abcdefg") == string_t("abcdefg") .and. &
      string_t("xyz pdq") ==          "xyz pdq"  .and. &
               "123.456"  == string_t("123.456")
  end function

  function supports_non_equivalence_operator() result(passed)
    logical passed
    passed = &
      string_t("abcdefg") /= string_t("xyz pdq") .and. &
      string_t("xyz pdq") /=          "abcdefg"  .and. &
               "123.456"  /= string_t("456.123")
  end function

  function assigns_string_t_to_character() result(passed)
    logical passed
    character(len=:), allocatable :: lhs

    associate(rhs => string_t("ya don't say"))
      lhs = rhs
      passed = lhs == rhs
    end associate
  end function

  function assigns_character_to_string_t() result(passed)
    logical passed
    character(len=*), parameter :: rhs = "well, alrighty then"
    type(string_t) lhs

    lhs = rhs
    passed = lhs == rhs
  end function

  function supports_concatenation_operator() result(passed)
    logical passed
    character(len=*), parameter :: prefix = "foo", postfix="bar"

#ifndef _CRAYFTN
    associate(infix => string_t(" yada yada "))
      passed = prefix // infix // postfix == prefix // infix%string() // postfix 
    end associate
#else
    block
      type(string_t) infix
      infix = string_t(" yada yada ")
      passed = prefix // infix // postfix == prefix // infix%string() // postfix 
    end block
#endif
  end function

  function constructs_from_default_integer() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(string => string_t(1234567890))
      passed = adjustl(trim(string%string())) == "1234567890"
    end associate
#else
    block 
      type(string_t) string
      string = string_t(1234567890)
      passed = adjustl(trim(string%string())) == "1234567890"
    end block
#endif
  end function

  function constructs_from_real() result(passed)
    logical passed
    real, parameter :: real_value = -1./1024. ! use a negative power of 2 an exactly representable rational number
    real read_value

#ifndef _CRAYFTN
    associate(string => string_t(real_value)
      read(string%string(), *) read_value
      passed = read_value == real_value
    end associate
#else
    block
      type(string_t) string
      string = string_t(real_value)
      read(string%string(), *) read_value
      passed = read_value == real_value
    end block
#endif

  end function

  function extracts_file_base_name() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(string => string_t(" foo .bar.too "))
      passed = string%base_name() == "foo .bar"
    end associate
#else
    block
      type(string_t) string
      string = string_t(" foo .bar.too ")
      passed = string%base_name() == "foo .bar"
    end block
#endif
  end function

  function extracts_file_name_extension() result(passed)
    logical passed

#ifndef _CRAYFTN
    associate(string => string_t(" foo .bar.too "))
      passed = string%file_extension() == "too"
    end associate
#else
    block
      type(string_t) string
      string = string_t(" foo .bar.too ")
      passed = string%file_extension() == "too"
    end block
#endif
  end function

  function concatenates_elements() result(passed)
    logical passed
    passed = (.cat. [string_t("foo"), string_t("bar")]) == "foobar"
  end function

end module string_test_m
