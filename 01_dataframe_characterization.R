# 01: This step focuses on characterizing the dataset as  whole, such as defining the data type each column is,
#       getting summary statistics on the dataset to flag any values that may appear unreliable since they do not
#       follow established rules

# Sourcing functions, setup, and project configuration file
source('00_config.R')
rdata_fn <- "01_dataframe_characterization.RData"

if (!(file.exists(rdata_fn))) {
        # Loading the DATATYPE_RULEBOOK standalone rules. These rules give all the possible datatypes available, with the rules,
        # constraints, soft flags (clinically improbable data), and hard flags (clinically impossible data)
        STANDALONE_RULES <- my_read_csv("INPUT/DATATYPE_RULEBOOK - standalone_rules.csv")
        
        
        # Viewing the summary data in the Console, I create a vector of the datatype for each column. In more complex cases,
        # I would also include a definition of each column along with the datatype assignment, but since the columns are
        # straightforward in this case, I will skip this step
        COL_DATATYPE <- c("category", "category", "category", "category", "number", "category", "date", "time", "time", "time", "time", "category")
        
        # Creating the `SCRIPT_MAP` object that will drive the remainder of the QA process
        SCRIPT_MAP <- data.frame(COL_NAME = colnames(INPUT_DATA),
                                 COL_DATATYPE) %>%
                left_join(STANDALONE_RULES %>%
                                  select(COL_DATATYPE, QA_STEP))
        
        # `SCRIPT_MAP` is then split according to data type, creating `SCRIPT_MAP` objects for downstream QA
        SCRIPT_MAP <- split(SCRIPT_MAP, SCRIPT_MAP$COL_DATATYPE)
        
        for (i in 1:length(SCRIPT_MAP)) {
                obj_name <- paste0("SCRIPT_MAP_02", SCRIPT_MAP[[i]]$QA_STEP[1], "_", names(SCRIPT_MAP)[i])
                assign(obj_name, SCRIPT_MAP[[i]] %>% select(-QA_STEP))
                rm(obj_name,i)
        }
        
        rm(COL_DATATYPE)
        save(list = c(objects(pattern = "^SCRIPT_MAP_"),"SCRIPT_MAP","STANDALONE_RULES"), file = "01_dataframe_characterization.RData")
} else {
        load(rdata_fn)
}

rm(rdata_fn)
