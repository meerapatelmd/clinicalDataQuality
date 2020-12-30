#03 Getting the final output together
#Filtering for other flagged data
OUTPUT_FLAGS_03 <-
list(
INPUT_DATA_FINAL %>%
        filter_all(any_vars(. == "")) %>%
        mutate(FLAG_REASON = "blank value") %>%
        mutate(FLAG_TYPE = "soft"),
INPUT_DATA_FINAL %>%
        filter_all(any_vars(is.na(.))) %>%
        mutate(FLAG_REASON = "NA value") %>%
        mutate(FLAG_TYPE = "soft")
)
OUTPUT_FLAGS_03 <- bind_rows(OUTPUT_FLAGS_03)

#04 Aggregating all flagged data
OUTPUT_FLAGS_OBJS <- objects(pattern = "^OUTPUT_FLAGS_[0-9]{1,2}")
OUTPUT_FLAGS_LIST <- sapply(1:length(OUTPUT_FLAGS_OBJS), function(i) get(OUTPUT_FLAGS_OBJS[i]))
OUTPUT_FLAGS <- bind_rows(OUTPUT_FLAGS_LIST)

FINAL_OUTPUT <-
        list(VALIDATED_DATA = INPUT_DATA_FINAL,
             OUTPUT_FLAGS = OUTPUT_FLAGS,
             OVERLAP_INPUT_FLAGS = merge(OUTPUT_FLAGS, INPUT_DATA_FINAL))

my_write_xlsx(FINAL_OUTPUT, "final_standalone_flag_output")
