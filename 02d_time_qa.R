# 02d: Time

source('~/R/qa_functions.R')

if (file.exists("02d_time_qa.RData")) {
        load("02d_time_qa.RData")
} else {
        # Assigning `SCRIPT_MAP_02d_data` object as the current operating SCRIPT_MAP,
        SCRIPT_MAP <- SCRIPT_MAP_02d_time %>% call_mr_clean()
        INPUT_DATA_FINAL_02D <- INPUT_DATA_FINAL
        
        for (i in 1:nrow(SCRIPT_MAP)) {
                time_vector <-
                        col_to_vector(INPUT_DATA_FINAL_02D, SCRIPT_MAP$COL_NAME[i])
                names(time_vector) <- INPUT_DATA_FINAL$PKEY
                
                if (!(exists("OUTPUT_FLAGS_02D"))) {
                        OUTPUT_FLAGS_02D <- list()
                }
                
                OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02D) + 1
                
                if (any(get_nchar_number(time_vector) > 6)) {
                        nam <- names(time_vector[get_nchar_number(time_vector) > 6])
                        INPUT_DATA_FINAL_02D <- INPUT_DATA_FINAL_02D[!(INPUT_DATA_FINAL_02D$PKEY %in% nam),]
                        OUTPUT_FLAGS_02D[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02D[(INPUT_DATA_FINAL_02D$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("more than 6 numbers in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "hard")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02D) + 1
                }
                
                if (any(get_nchar_number(time_vector) < 3)) {
                        nam <- names(time_vector[get_nchar_number(time_vector) < 3])
                        INPUT_DATA_FINAL_02D <- INPUT_DATA_FINAL_02D[!(INPUT_DATA_FINAL_02D$PKEY %in% nam),]
                        OUTPUT_FLAGS_02D[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02D[(INPUT_DATA_FINAL_02D$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("less than 3 numbers in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "hard")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02D) + 1
                }
                
                if (any(get_nchar_punct(time_vector) > 2)) {
                        nam <- names(time_vector[get_nchar_punct(time_vector) > 2])
                        INPUT_DATA_FINAL_02D <- INPUT_DATA_FINAL_02D[!(INPUT_DATA_FINAL_02D$PKEY %in% nam),]
                        OUTPUT_FLAGS_02D[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02D[(INPUT_DATA_FINAL_02D$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("more than 2 punctuation marks in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "hard")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02D) + 1
                }
                
                OUTPUT_FLAGS_02D[[OUTPUT_FLAGS_start]] <-
                        INPUT_DATA_FINAL_02D %>%
                        filter_at(vars(!!SCRIPT_MAP$COL_NAME[i]), any_vars(is.na(hm(.)))) %>%
                        filter_all(any_vars(is.na(.))) %>%
                        mutate(FLAG_REASON = "time failed to parse", FLAG_TYPE = "hard")
                
                 OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02D) + 1
                
                OUTPUT_FLAGS_02D <- bind_rows(OUTPUT_FLAGS_02D)
                
                if (nrow(OUTPUT_FLAGS_02D) == 0) {
                        rm(OUTPUT_FLAGS_02D)
                }
                
                rm(OUTPUT_FLAGS_start)
                
                save(list = (objects(pattern = "OUTPUT_FLAGS_|SCRIPT_MAP$")), file = "02d_time_qa.RData")
                
                rm(i)
                
        }
}
