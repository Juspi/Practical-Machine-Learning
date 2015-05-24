#Load packages, set working directory and read data
library(caret)

setwd("~/Coursera/Practical Machine Learning/Viikko 3/Write Up")
pmlTreeni<-read.csv("pml-training.csv", header=T, na.strings=c("NA", "#DIV/0!"))
pmlTesti<-read.csv("pml-testing.csv", header=T, na.string=c("NA", "#DIV/0!"))

#Remove columns with NA:s
TreeniNA<-apply(pmlTreeni, 2, function(x) { sum(is.na(x)) })
Treenivalidi <- pmlTreeni[, which(TreeniNA == 0)]

TreeniNA<-apply(pmlTesti, 2, function(x) { sum(is.na(x)) })
Testivalidi <- pmlTesti[, which(TreeniNA == 0)]

#Take just the sensor values data
Treenivalitut<-Treenivalidi[,c(8:60)]
Testivalitut<-Testivalidi[,c(8:60)]

#data partitioning
Treenissä<-createDataPartition(y=Treenivalitut$classe, p=0.60, list=F)
treeni<-Treenivalitut[Treenissä,] 
testi<-Treenivalitut[-Treenissä,] 

#Random Forest testing
fitTreeni<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
modFit<-train(classe~., data=treeni, method="rf", trControl=fitTreeni, verbose=F)
print(modFit)

ennuste<-predict(modFit, newdata=testi)
confusionMatrix(ennuste, testi$classe)

ennuste20<-predict(modFit, newdata=Testivalitut)
ennuste20

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(ennuste20)
