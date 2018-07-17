setwd("D:\\titanic")

getwd()

TTrain<-read.csv("train.csv",header = T,sep = ",",na.strings = "")

TTest<-read.csv("test.csv",header = T,sep = ",",na.strings = "")

TTrain$Set<-"Train"
TTest$Set<-"TTest"