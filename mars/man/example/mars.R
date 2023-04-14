
test <- mars(y~.,data=data1[1:100,])
print(test)
summary(test)
plot(test)
anova(test)
predict(test,data1[101:200,2:10])
