########### Applying Model to the Test Data set to generate O/P

predicted<-predict(model1,type = "class",newdata = Test)
length(predicted)


head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)


Test$Survived<-predicted


submission<-Test%>%select(PassengerId,Survived)


write.csv(x = submission,file = "Gender_submission.csv",row.names = F)
