DIRECTORIES <- c("INPUT", "OUTPUT")
sapply(1:length(DIRECTORIES), function(i) ifelse(!(dir.exists(DIRECTORIES[i])), dir.create(DIRECTORIES[i]), print(paste0(DIRECTORIES[i], " exists."))))
rm(DIRECTORIES)

