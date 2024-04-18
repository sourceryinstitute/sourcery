module sourcery_m
  !! export all public entities from every other sourcery module
  use sourcery_command_line_m, only : command_line_t
  use sourcery_data_partition_m, only : data_partition_t
  use sourcery_bin_m, only : bin_t
  use sourcery_formats_m, only : csv, cscv, separated_values
  use sourcery_file_m, only : file_t
  use sourcery_string_m, only : string_t, operator(.cat.)
  use sourcery_test_result_m, only : test_result_t
  use sourcery_test_m, only : test_t, test_description_substring
  use sourcery_user_defined_collectives_m, only : co_all

  !! legacy modules (likely to be removed in a future release):
  use sourcery_object_m, only : object_t
  use sourcery_co_object_m, only : co_object
  use sourcery_oracle_m, only : oracle_t
  use sourcery_units_m

  implicit none

end module sourcery_m
