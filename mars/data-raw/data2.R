data <- read.csv2("data-raw/qsar_fish_toxicity.csv", header = FALSE)

names(data) <- c("x1",paste0("x",2:(ncol(data)-1)))
names(data)[ncol(data)] <- "y"


data2<-data.frame(data)
data2 <- data2[sample(nrow(data2), 200),]
usethis::use_data(data2, overwrite = TRUE)
