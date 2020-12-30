# 02c: Date
source('~/R/qa_functions.R')

if (file.exists("02c_date_qa.RData")) {
        load("02c_date_qa.RData")
} else {
        # Assigning `SCRIPT_MAP_02c_data` object as the current operating SCRIPT_MAP,
        SCRIPT_MAP <- SCRIPT_MAP_02c_date %>% call_mr_clean()
        INPUT_DATA_FINAL_02C <- INPUT_DATA_FINAL
        
        for (i in 1:nrow(SCRIPT_MAP)) {
                date_vector <-
                        col_to_vector(INPUT_DATA_FINAL_02C, SCRIPT_MAP$COL_NAME[i])
                names(date_vector) <- INPUT_DATA_FINAL$PKEY
                
                if (!(exists("OUTPUT_FLAGS_02C"))) {
                        OUTPUT_FLAGS_02C <- list()
                }
                
                OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02C) + 1
                
                if (any(get_nchar_number(date_vector) > 8)) {
                        nam <- names(date_vector[get_nchar_number(date_vector) > 8])
                        INPUT_DATA_FINAL_02C <- INPUT_DATA_FINAL_02C[!(INPUT_DATA_FINAL_02C$PKEY %in% nam),]
                        OUTPUT_FLAGS_02C[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02C[(INPUT_DATA_FINAL_02C$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("more than 8 numbers in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "hard")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02C) + 1
                }
                
                if (any(get_nchar_number(date_vector) < 5)) {
                        nam <- names(date_vector[get_nchar_number(date_vector) < 5])
                        INPUT_DATA_FINAL_02C <- INPUT_DATA_FINAL_02C[!(INPUT_DATA_FINAL_02C$PKEY %in% nam),]
                        OUTPUT_FLAGS_02C[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02C[(INPUT_DATA_FINAL_02C$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("less than 5 numbers in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "hard")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02C) + 1
                }
                
                if (any(get_nchar_punct(date_vector) > 2)) {
                        nam <- names(date_vector[get_nchar_punct(date_vector) > 2])
                        INPUT_DATA_FINAL_02C <- INPUT_DATA_FINAL_02C[!(INPUT_DATA_FINAL_02C$PKEY %in% nam),]
                        OUTPUT_FLAGS_02C[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02C[(INPUT_DATA_FINAL_02C$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("more than 2 punctuation marks in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "hard")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02C) + 1
                }
                
                OUTPUT_FLAGS_02C[[OUTPUT_FLAGS_start]] <-
                        data.frame(date_vector) %>%
                        mutate(conv_date_vector = ymd(date_vector)) %>%
                        filter_all(any_vars(is.na(.))) %>%
                        mutate(FLAG_REASON = "date failed to parse", FLAG_TYPE = "hard")
                
                OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02C) + 1
                
                OUTPUT_FLAGS_02C[[OUTPUT_FLAGS_start]] <-
                        data.frame(date_vector) %>%
                        mutate(conv_date_vector = ymd(date_vector)) %>%
                        mutate_all(all_vars(gsub("", NA, .))) %>%
                        filter_all(any_vars(. > Sys.Date())) %>%
                        mutate(FLAG_REASON = "future date") %>%
                        mutate(FLAG_TYPE = "hard")
                OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02C) + 1
                        
                        OUTPUT_FLAGS_02C <- bind_rows(OUTPUT_FLAGS_02C)
                
                if (nrow(OUTPUT_FLAGS_02C) == 0) {
                        rm(OUTPUT_FLAGS_02C)
                }
                
                rm(OUTPUT_FLAGS_start)
                
                save(list = (objects(pattern = "OUTPUT_FLAGS_|SCRIPT_MAP$")), file = "02c_date_qa.RData")
                
                rm(i)
                
        }
}