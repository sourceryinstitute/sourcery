submodule(sourcery_string_m) sourcery_string_s
  use assert_m, only : assert
  use sourcery_m, only : csv
  implicit none
  
contains

  module procedure construct
    new_string%string_ = string
  end procedure

  module procedure as_character
    raw_string = self%string_
  end procedure

  module procedure is_allocated
    string_allocated = allocated(self%string_)
  end procedure

  module procedure array_of_strings
    character(len=:), allocatable :: remainder, next_string
    integer next_delimiter, string_end

    remainder = trim(adjustl(delimited_strings))
    allocate(strings_array(0))

    do  
      next_delimiter = index(remainder, delimiter)
      string_end = merge(len(remainder), next_delimiter-1, next_delimiter==0)
      next_string = trim(adjustl(remainder(:string_end)))
      if (len(next_string)==0) exit
      strings_array = [strings_array, string_t(next_string)]
      if (next_delimiter==0) then
        remainder = ""
      else
        remainder = trim(adjustl(remainder(next_delimiter+1:)))
      end if
    end do

  end procedure

  module procedure get_json_key
    character(len=:), allocatable :: raw_line
  
    raw_line = self%string()
    associate(opening_key_quotes => index(raw_line, '"'), separator => index(raw_line, ':'))
      associate(closing_key_quotes => opening_key_quotes + index(raw_line(opening_key_quotes+1:), '"'))
        unquoted_key = string_t(trim(raw_line(opening_key_quotes+1:closing_key_quotes-1)))
      end associate
    end associate

  end procedure

  module procedure get_json_real
    character(len=:), allocatable :: raw_line, string_value

    call assert(key==self%get_json_key(), "string_s(get_json_real): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else 
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        read(string_value, fmt=*) value_
      end associate
    end associate

  end procedure

  module procedure get_json_string

    character(len=:), allocatable :: raw_line

    call assert(key==self%get_json_key(), "key==self%get_string_json()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(opening_value_quotes => index(text_after_colon, '"'))
        associate(closing_value_quotes => opening_value_quotes + index(text_after_colon(opening_value_quotes+1:), '"'))
          if (any([opening_value_quotes, closing_value_quotes] == 0)) then
            value_ = string_t(trim(adjustl((text_after_colon))))
          else
            value_ = string_t(text_after_colon(opening_value_quotes+1:closing_value_quotes-1))
          end if
        end associate
      end associate
    end associate

  end procedure

  module procedure get_json_logical
    character(len=:), allocatable :: raw_line, string_value

    call assert(key==self%get_json_key(), "string_s(get_json_logical): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else 
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        call assert(string_value=="true" .or. string_value=="false", &
          'string_s(get_json_logical): string_value=="true" .or. string_value="false"', string_value)
        value_ = string_value == "true"
      end associate
    end associate

  end procedure

  module procedure get_json_integer
    character(len=:), allocatable :: raw_line, string_value

    call assert(key==self%get_json_key(), "string_s(get_json_logical): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(text_after_colon => raw_line(index(raw_line, ':')+1:))
      associate(trailing_comma => index(text_after_colon, ','))
        if (trailing_comma == 0) then
          string_value = trim(adjustl((text_after_colon)))
        else 
          string_value = trim(adjustl((text_after_colon(:trailing_comma-1))))
        end if
        read(string_value, fmt=*) value_
      end associate
    end associate

  end procedure

  module procedure get_json_integer_array
    character(len=:), allocatable :: raw_line
    real, allocatable :: real_array(:)
    integer i

    call assert(key==self%get_json_key(), "string_s(get_json_integer_array): key==self%get_json_key()", key)

    raw_line = self%string()
    associate(colon => index(raw_line, ":"))
      associate(opening_bracket => colon + index(raw_line(colon+1:), "["))
        associate(closing_bracket => opening_bracket + index(raw_line(opening_bracket+1:), "]"))
          associate(commas => count("," == [(raw_line(i:i), i=opening_bracket+1,closing_bracket-1)]))
            associate(num_inputs => commas + 1)
              allocate(real_array(num_inputs))
              read(raw_line(opening_bracket+1:closing_bracket-1), fmt=*) real_array
              value_ = int(real_array)
            end associate
          end associate
        end associate
      end associate
    end associate

  end procedure

  module procedure string_t_eq_string_t
    lhs_eq_rhs = lhs%string() == rhs%string()
  end procedure
   
  module procedure string_t_eq_character
    lhs_eq_rhs = lhs%string() == rhs
  end procedure

  module procedure character_eq_string_t
    lhs_eq_rhs = lhs == rhs%string()
  end procedure
   
end submodule sourcery_string_s
