submodule(test_result_m) test_result_s
  implicit none

contains

    module procedure construct
      test_result%description_ = description
      test_result%passed_ = passed
    end procedure

    module procedure characterize
      characterization = merge("passes on ", "FAILS on  ", self%passed_) // self%description_ // "."
    end procedure

    module procedure passed
      test_passed = self%passed_
    end procedure

end submodule test_result_s
