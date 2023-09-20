submodule(sourcery_formats_m) formats_s
  !! Construct separated-value formats 
  implicit none

contains

  module procedure separated_values
    character(len=*), parameter :: prefix = "(*(G0,:,'"
    character(len=*), parameter :: suffix =           "'))"
    character(len=*), parameter :: complex_prefix = "(*('(',G0,',',G0,')',:,'" 

    select rank(mold)
      rank(1)
        select type(mold)
          type is(complex)
            format_string = complex_prefix // separator // suffix
          type is(real)
            format_string = prefix // separator // suffix
          type is(integer)
            format_string = prefix // separator // suffix
          type is(character(len=*))
            format_string = prefix // separator // suffix
          class default
             error stop "format_s separated_values: unsupported type"
        end select
      rank default
        error stop "formats_s separated_values: unsupported rank"
    end select
  end procedure

end submodule formats_s
