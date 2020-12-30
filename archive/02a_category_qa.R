# 02a: A "category" datatype is defined as a vector of possible values that the content of the column can fall under.
# Therefore a requisite requirement for a QA process for this datatype is having the necessary control data to 
# compare it to.

source('~/R/qa_functions.R')

if (!(file.exists("02a_category_qa.RData"))) {
        # Assigning `SCRIPT_MAP_02a_category` object as the current operating SCRIPT_MAP, while assigning the columns
        # that would require controls for the QA process as TRUE under "CONTROL_EXISTS". A control is not needed for
        # category values that serve as identifiers
        SCRIPT_MAP <- SCRIPT_MAP_02a_category %>% call_mr_clean() %>%
                mutate("CONTROL_EXISTS" = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)) %>%
                mutate("CONTROL_OBJ_NAME" = ifelse(CONTROL_EXISTS == TRUE, paste0(COL_NAME, "_CONTROL"), ""))
        
        PATIENT_DIAGNOSIS_CONTROL <- tolower(c("Prostate Cancer","Kidney Cancer","Bladder Cancer","Testicular Cancer"))
        INSURANCE_TYPE_CONTROL <- tolower(c("Private Insurance","Medicaid","Self Insured","Medicare",""))
        SEX_CONTROL <- tolower(c("M", "F"))
        PROVIDER_NAME_CONTROL <- tolower(c("L. Svenson","I. Petrov", "E. Ahuja","J. Smith","N. Fulano","M. Dupont", "S. Moreau","W. Plinge","C. S. Ming","C. Siu Ming"))
        
        for (i in 1:length(SCRIPT_MAP$CONTROL_OBJ_NAME)) {
                x <- SCRIPT_MAP$CONTROL_OBJ_NAME[i]
                if (x != "") {
                        if (!(exists(x))) {
                                say <- paste0("Control is needed for the ", SCRIPT_MAP$COL_NAME[i], " column. ")
                                tell_me(say)
                                rm(say)
                                readline("Press [ENTER] to continue or [ESC] to end. ")
                        }
                }
        }
        
        # Checking the INPUT_DATA category data against the controls. If there is a mismatch, the unmatched values
        # are printed back
        for (i in 1:nrow(SCRIPT_MAP)) {
                if (SCRIPT_MAP$CONTROL_OBJ_NAME[i] != "") {
                        
                        if (!(exists("OUTPUT_FLAGS_02A"))) {
                                OUTPUT_FLAGS_02A <- list()
                        }
                        
                        COL_NAME <- SCRIPT_MAP$COL_NAME[i]
                        
                        NATIVE_VALUES <- as.character(get_unique_values(INPUT_DATA_FINAL, COL_NAME))
                        CONTROL_VALUES <- get(SCRIPT_MAP$CONTROL_OBJ_NAME[i]) 
                        
                        FLAGGED_VALUES <- NATIVE_VALUES[!(NATIVE_VALUES %in% CONTROL_VALUES)]
                        
                        if (length(FLAGGED_VALUES) > 0) {
                                VAR_NAME <- COL_NAME
                                OUTPUT_FLAGS_02A[[i]] <-
                                        INPUT_DATA_FINAL %>%
                                        filter_at(VAR_NAME, all_vars((. %in% FLAGGED_VALUES) == TRUE)) %>%
                                        mutate(FLAG_REASON = "not in control") %>%
                                        mutate(FLAG_TYPE = "soft")
                                names(OUTPUT_FLAGS_02A)[i] <- SCRIPT_MAP$COL_NAME[i]
                        }
                        
                }
        }
        if (length(OUTPUT_FLAGS_02A) == 0) {
                rm(OUTPUT_FLAGS_02A)
        }
        
        rm(x, i)
        
        save(list = c(objects(pattern = "OUTPUT_FLAGS_|_CONTROL"), "SCRIPT_MAP"), 
             file = "02a_category_qa.RData")
} else {
        load("02a_category_qa.RData")
}

