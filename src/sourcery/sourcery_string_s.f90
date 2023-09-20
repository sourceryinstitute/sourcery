<<<<<<< HEAD:src/sourcery/string_s.f90
submodule(sourcery_string_m) string_s
=======
submodule(sourcery_string_m) sourcery_string_s
>>>>>>> 03abe88 (chore: prefix sourcery_ to submodule names):src/sourcery/sourcery_string_s.f90
  implicit none
  
contains

  module procedure construct
    new_string%string_ = string
  end procedure

  module procedure string
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

end submodule sourcery_string_s
