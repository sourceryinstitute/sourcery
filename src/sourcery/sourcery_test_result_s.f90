submodule(sourcery_test_result_m) sourcery_test_result_s
  implicit none

contains

    module procedure construct
      test_result%description_ = description
      test_result%passed_ = passed
    end procedure

    module procedure characterize
      characterization = trim(merge("passes on ", "FAILS on  ", self%passed_)) // " " // trim(self%description_) // "."
    end procedure

    module procedure passed
      test_passed = self%passed_
    end procedure

end submodule sourcery_test_result_s
