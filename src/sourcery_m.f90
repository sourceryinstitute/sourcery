module sourcery_m
  !! export all public entities from every other sourcery module
  use command_line_m, only : command_line_t
  use data_partition_m, only : data_partition_t
  use formats_m, only : formats_t
  use file_m, only : file_t
  use string_m, only : string_t
  use test_result_m, only : test_result_t
  use test_m, only : test_t
  use user_defined_collectives_m, only : co_all

  !! legacy modules (likely to be removed in a future release):
  use object_m, only : object_t
  use co_object_m, only : co_object_t
  use oracle_m, only : oracle_t
  use units_m

  implicit none

end module sourcery_m
