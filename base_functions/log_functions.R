create_log <-
        function() {
                
                log_fn <- "project_log.csv"
                
                if (!(file.exists(log_fn))) {
                                write.csv(data.frame(
                                                date_and_time = Sys.time(),
                                                activity = "log created",
                                                filename = "./project_log.csv",
                                                summary = "initiated log for project") %>%
                                                call_mr_clean(),
                                          log_fn,
                                          row.names = FALSE
                                )
                }
        }


write_log_entry <-
        function(fn = "",
                 input_or_output = "",
                 description = "") {
                
                log_fn <- "project_log.csv"
                
                if (!(file.exists(log_fn))) {
                        create_log()
                }
                
                entire_log <- read.csv(log_fn) %>%
                                        call_mr_clean()
                
                new_log <- bind_rows(entire_log,
                                     data.frame(
                                             date_and_time = Sys.time(),
                                             activity = input_or_output,
                                             filename = fn,
                                             summary = description) %>%
                                             call_mr_clean())
                
                write.csv(new_log, log_fn, row.names = FALSE)
                }

