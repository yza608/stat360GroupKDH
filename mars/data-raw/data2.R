data <- read.table("data-raw/yacht_hydrodynamics.data")

names(data) <- c("x1",paste0("x",2:(ncol(data)-1)))
names(data)[ncol(data)] <- "y"


data2<-data.frame(data)
data2 <- data2[sample(nrow(data2), 300),]
usethis::use_data(data2, overwrite = TRUE)
