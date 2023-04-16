library(mars)


#test using data2
test1 <- mars(y~., data1[sample(nrow(data1), 100),], mars.control(Mmax=6))
print(test1)
summary(test1)
plot(test1)
anova(test1)
predict(test1,data1[sample(nrow(data1), 100),2:10])

#test using data2
test2 <- mars(y~., data2[sample(nrow(data2), 100),], mars.control(Mmax=2))
print(test2)
summary(test2)
plot(test2)
anova(test2)
predict(test2,data2[sample(nrow(data2), 100),1:6])

#test using data3
test3 <- mars(y~., data3[sample(nrow(data3), 100),], mars.control(Mmax=4))
print(test3)
summary(test3)
plot(test3)
anova(test3)
predict(test3,data3[sample(nrow(data3), 100),1:8])

