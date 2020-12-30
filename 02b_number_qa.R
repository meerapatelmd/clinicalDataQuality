# 02b: A "number" is any value that contains only 1 number and a maximum of only 1 decimal point
source('~/R/qa_functions.R')

if (!(exists("02b_number_qa.RData"))) {
        # Assigning `SCRIPT_MAP_02b_number` object as the current operating SCRIPT_MAP,
        SCRIPT_MAP <- SCRIPT_MAP_02b_number %>% call_mr_clean()
        INPUT_DATA_FINAL_02B <- INPUT_DATA_FINAL
        
        for (i in 1:nrow(SCRIPT_MAP)) {
                number_vector <-
                        col_to_vector(INPUT_DATA_FINAL, SCRIPT_MAP$COL_NAME[i])
                names(number_vector) <- INPUT_DATA_FINAL$PKEY
                
                if (!(exists("OUTPUT_FLAGS_02B"))) {
                        OUTPUT_FLAGS_02B <- list()
                }
                
                OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02B) + 1
                
                if (any(get_nchar_decimal(number_vector)) > 1) {
                        nam <- names(number_vector[get_nchar_decimal(number_vector) > 1])
                        INPUT_DATA_FINAL_02B <- INPUT_DATA_FINAL_02B[!(INPUT_DATA_FINAL_02B$PKEY %in% nam),]
                        OUTPUT_FLAGS_02B[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02B[(INPUT_DATA_FINAL_02B$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("multiple decimals in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "hard")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02B) + 1
                }
                
                if (any(get_nchar_letter(number_vector)) > 0) {
                        nam <- names(number_vector[get_nchar_letter(number_vector) > 0])
                        INPUT_DATA_FINAL_02B <- INPUT_DATA_FINAL_02B[!(INPUT_DATA_FINAL_02B$PKEY %in% nam),]
                        OUTPUT_FLAGS_02B[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02B[(INPUT_DATA_FINAL_02B$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("letters in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "soft")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02B) + 1
                }
                
                if (any((get_nchar_punct(number_vector) != get_nchar_decimal(number_vector)) == FALSE)) {
                        nam <- names(number_vector[get_nchar_letter(number_vector) > 0])
                        INPUT_DATA_FINAL_02B <- INPUT_DATA_FINAL_02B[!(INPUT_DATA_FINAL_02B$PKEY %in% nam),]
                        OUTPUT_FLAGS_02B[[OUTPUT_FLAGS_start]] <- INPUT_DATA_FINAL_02B[(INPUT_DATA_FINAL_02B$PKEY %in% nam),] %>%
                                mutate(FLAG_REASON = paste0("non-decimal punct in ", SCRIPT_MAP$COL_NAME[i])) %>%
                                mutate(FLAG_TYPE = "soft")
                        OUTPUT_FLAGS_start <- length(OUTPUT_FLAGS_02B) + 1
                }
        }
        
        OUTPUT_FLAGS_02B <- bind_rows(OUTPUT_FLAGS_02B)
        
        if (nrow(OUTPUT_FLAGS_02B) == 0) {
                rm(OUTPUT_FLAGS_02B)
        }
        
        rm(OUTPUT_FLAGS_start)
        
        save(list = (objects(pattern = "OUTPUT_FLAGS_|SCRIPT_MAP$")), file = "02b_number_qa.RData")
} else {
        load("02b_number_qa.RData")
}
