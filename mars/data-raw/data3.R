library(readxl)

data3 <- read_excel("data-raw/Concrete_Data.xls")

names(data3) <- c("x1",paste0("x",2:(ncol(data3)-1)))
names(data3)[ncol(data3)] <- "y"

data3<-data.frame(data3)
data3 <- data3[sample(nrow(data3), 200),]
usethis::use_data(data3, overwrite = TRUE)
