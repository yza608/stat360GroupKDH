data <- read.csv("data-raw/CASP.csv")
names(data)[2:ncol(data)] <- c("x1",paste0("x",2:(ncol(data)-1)))
names(data)[1] <- "y"

data1 <- data[sample(nrow(data), 200),]
usethis::use_data(data1, overwrite = TRUE)
