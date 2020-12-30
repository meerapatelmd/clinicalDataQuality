
get_unique_count <-
        function(dataframe, col_name) {
                length(unique(dataframe %>% select(col_name) %>% distinct() %>% unlist()))
        }

get_unique_values <-
        function(dataframe, col_name) {
                unique(dataframe %>% select(col_name) %>% distinct() %>% unlist())
        }


col_to_vector <-
        function(dataframe, col_name) {
                dataframe %>% select(col_name) %>% mutate_all(as.character) %>% unlist() %>% unname()
        }

get_nchar_number <-
        function(vector) {
                nchar(gsub("[^0-9]", "", as.character(vector)))
        }

get_nchar_letter <-
        function(vector) {
                nchar(gsub("[^A-Za-z]", "", as.character(vector)))
        }

get_nchar_decimal <-
        function(vector) {
                nchar(gsub("[^\\.]", "", as.character(vector)))
        }

get_nchar_punct <-
        function(vector) {
                nchar(gsub("[^[:punct:]]", "", as.character(vector)))
        }
