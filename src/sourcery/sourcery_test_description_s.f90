submodule(sourcery_test_description_m) sourcery_test_description_s
  implicit none

contains

    module procedure construct
      test_description%description_ = description
      test_description%test_function_ => test_function
    end procedure

    module procedure run
      test_result = test_result_t(self%description_, self%test_function_())
    end procedure

end submodule sourcery_test_description_s