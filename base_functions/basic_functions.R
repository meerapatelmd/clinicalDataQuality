# Makes a new blank line in Console
#       @param  blank_lines is number of blank lines desired
#       @return blank line
make_new_line <- 
        function(blank_lines = 1) {
                cat(paste(rep("\n", times = blank_lines), collapse = ""))
        }

# Prints back raw input with clearer spacing
#       @param  say is the phrase that is printed back
#       @param  tabs is the number of indents that phrase will have
#       @return finput phrase with spacing
tell_me <-
        function(say, tabs = 1) {
                make_new_line()
                cat(paste(rep("\t", times = tabs), say, collapse = "", sep = ""))
                make_new_line()
        }

# Returns timestamp in YYYYMMDD_HHMMSS format
#       @return timestamp
get_timestamp <- function(x) {
        format(Sys.time(), "%Y%m%d_%H%M%S")
}

# Returns all files in the INPUT directory
#       @return files list
get_input_list <- function() {
        list.files("INPUT", full.names = TRUE)
}

# Returns all files in the OUTPUT directory
#       @return files list
get_output_list <- function() {
        list.files("OUTPUT", full.names = TRUE)
}

# Mutates all columns of a dataframe to character and trims all white spaces
#       @param  dataframe
#       
#       @return cleaned up dataframe
call_mr_clean <- function(dataframe) {
        dataframe %>%
                mutate_all(as.character) %>%
                mutate_all(trimws, "both")
}

#Takes a column and brings it to the lead of the dataframe
#       @param  dataframe
#       @param  column name
#
#       @return dataframe with column at the leftmost position
bring_col_to_front <-
        function(dataframe, col_name) {
                col_name <- enquo(col_name)
                
                dataframe %>%
                        select(!! col_name, everything())
        }


#Takes a dataframe, creates a new primary key ("pkey") column according to row number in the leftmost position with the option of a prefix
#       @param  dataframe
#       @param  primary key prefix is the prefix that will be added to "_pkey" if desired
#
#       @return dataframe with primary key column at the leftmost position
add_seasoning_to_input <- 
        function(dataframe, primary_key_prefix = "", duplicates = FALSE) {
                if (primary_key_prefix == "") {
                        primary_key <- "PKEY"
                } else {
                        primary_key <- paste0(toupper(primary_key_prefix), "_PKEY")
                }
                
                if (duplicates == FALSE) {
                        dataframe %>%
                                mutate(!! primary_key := 1:n()) %>%
                                bring_col_to_front(!! primary_key)
                } else {
                        dataframe %>%
                                distinct() %>%
                                mutate(!! primary_key := 1:n()) %>%
                                bring_col_to_front(!! primary_key)
                }
        }



# Reads dataframe copied to clipboard
#       @param  dataframe in clipboard
#       @return dataframe
read_clipboard <- function(header = TRUE) {
        if (header == TRUE) {
                read.table(pipe("pbpaste"), sep= "\t", header= TRUE)
        } else {
                read.table(pipe("pbpaste"), sep= "\t", header= FALSE)
        }
}

# Copies dataframe copied to clipboard
#       @param  dataframe object
#       @return dataframe in clipboard
copy_to_clipboard <- function(dataframe) {
                                clip <- pipe("pbcopy", "wb")
                                write.table(dataframe, clip, sep = "\t", row.names = FALSE)
                                close(clip)
}

# Writes csv to the OUTPUT directory with the option of having the activity logged and guardrails in case the chosen filename already exists
#       @param  dataframe object
#       @param  basename of the desired output file, without the path and file extension
#       @param  path_folder is the name of folder in the working directory that the output should be written to;
#                               defaults to OUTPUT folder and '.' can be used for home directory
#       @param  timestamp if TRUE appends the base filename with 'YYYYMMDD_HHMMSS' string; defaults to FALSE
#       @param  log  if TRUE, logs the output in the log with the option of adding a description
#       @param  interactive unquoted description of activity
#
#       @return new or overwritten csv file in the designated path_folder with or without a log entry
my_write_csv <-
        function(dataframe, basename, path_folder = "OUTPUT", timestamp = FALSE, log = TRUE) {
                if (path_folder != ".") {
                        stopifnot(dir.exists(path_folder)) 
                }

                if (timestamp == FALSE) {
                        fn <- paste0(path_folder, "/", basename, ".csv")
                        if (!(file.exists(fn))) {
                                tell_me(paste0("Writing ", fn, "..."))
                                make_new_line()
                                write_csv(dataframe, fn)
                                tell_me(paste0("Successfully wrote ", fn, "."))
                                make_new_line()
                        } else {
                                tell_me(paste0(fn, " already exists!"))
                                make_new_line()
                                readline("Press [ENTER] to overwrite or [ESC] to end. ")
                                write_csv(dataframe, fn)
                                tell_me(paste0("Successfully wrote ", fn, "."))
                                make_new_line()
                        }
                        
                } else {
                        fn <- paste0(path_folder, "/", basename, "_", get_timestamp(), ".csv")
                        if (!(file.exists(fn))) {
                                tell_me(paste0("Writing ", fn, "..."))
                                make_new_line()
                                write_csv(dataframe, fn)
                                tell_me(paste0("Successfully wrote ", fn, "."))
                                make_new_line()
                        } else {
                                tell_me(paste0(fn, " already exists!"))
                                make_new_line()
                                readline("Press [ENTER] to overwrite or [ESC] to end. ")
                                write_csv(dataframe, fn)
                                tell_me(paste0("Successfully wrote ", fn, "."))
                                make_new_line()
                        }
                }
                
                if (log == TRUE) {
                        source('~/R/log_functions.R')
                        
                        desc <- readline("Describe this output. ")
                        write_log_entry(fn = fn,
                                        input_or_output = "output",
                                        description = desc)
                }
        }

my_write_xlsx <-
        function(dataframe, basename, path_folder = "OUTPUT", timestamp = FALSE, log = TRUE) {
                require(openxlsx)
                if (path_folder != ".") {
                        stopifnot(dir.exists(path_folder)) 
                }
                
                if (timestamp == FALSE) {
                        fn <- paste0(path_folder, "/", basename, ".xlsx")
                        if (!(file.exists(fn))) {
                                tell_me(paste0("Writing ", fn, "..."))
                                make_new_line()
                                write.xlsx(dataframe, fn)
                                tell_me(paste0("Successfully wrote ", fn, "."))
                                make_new_line()
                        } else {
                                tell_me(paste0(fn, " already exists!"))
                                make_new_line()
                                readline("Press [ENTER] to overwrite or [ESC] to end. ")
                                write.xlsx(dataframe, fn)
                                tell_me(paste0("Successfully wrote ", fn, "."))
                                make_new_line()
                        }
                        
                } else {
                        fn <- paste0(path_folder, "/", basename, "_", get_timestamp(), ".xlsx")
                        if (!(file.exists(fn))) {
                                tell_me(paste0("Writing ", fn, "..."))
                                make_new_line()
                                write.xlsx(dataframe, fn)
                                tell_me(paste0("Successfully wrote ", fn, "."))
                                make_new_line()
                        } else {
                                tell_me(paste0(fn, " already exists!"))
                                make_new_line()
                                readline("Press [ENTER] to overwrite or [ESC] to end. ")
                                write.xlsx(dataframe, fn)
                                tell_me(paste0("Successfully wrote ", fn, "."))
                                make_new_line()
                        }
                }
                
                if (log == TRUE) {
                        source('~/R/log_functions.R')
                        
                        desc <- readline("Describe this output. ")
                        write_log_entry(fn = fn,
                                        input_or_output = "output",
                                        description = desc)
                }
        }


# Reads csv quickly
#       @param  fn      full path of csv file
#
#       @return dataframe object with all data converted to character
my_read_csv_2 <- 
        function(fn) {
                read.csv(fn) %>%
                        call_mr_clean()
        }


# Reads csv with the option of having the activity logged
#       @param  fn      filename without path unless path_folder is ""
#       @param  path_folder that stores the file that defaults to INPUT;'.' can be used for home directory and
#                        "" can be used if the full filename is supplied as fn
#       @param  log  if TRUE, logs the output in the log with the option of adding a description
#       @param  interactive unquoted description of activity
#
#       @return dataframe object with all data in character type
my_read_csv <-
        function(fn, path_folder = "INPUT", log = TRUE) {
                if (path_folder == "") {
                        fn <<- fn
                } else {
                        fn <<- paste0(path_folder, "/", fn)
                }
                
                if (log == TRUE) {
                        source('~/R/log_functions.R')
                        desc <- readline("Describe this input. ")
                        write_log_entry(fn = fn,
                                        input_or_output = "input",
                                        description = desc)
                }
                
                my_read_csv_2(fn = fn)
        }
                