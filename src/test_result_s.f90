submodule(test_result_m) test_result_s
  implicit none

contains

    module procedure construct
      test_result%description_ = description
      test_result%outcome_ = outcome
    end procedure

    module procedure characterize
      characterization = merge("Pass: ", "Fail: ", self%outcome_) // self%description_
    end procedure

end submodule test_result_s
