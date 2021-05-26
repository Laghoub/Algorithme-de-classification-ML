#chargement des packages
library(nnet)
library(e1071)
library(FSelector)
library( randomForest)
library(rpart)
# Dataset: Shuttle 
data=Shuttle
data
# Partition test et apprentissage
n=dim(data)[1]
index = sample(n, 0.7 * n)
Appren = data[index, ]
Test = data[-index, ]
# NAIVE BAyse
t1=Sys.time()
nb=naiveBayes(Class~.,Appren)
t2=Sys.time()
t=t2-t1
#Temps d'exécution
t
pred=predict(object = nb, newdata =Test)
p
confusion = table(Test$Class,pred)
confusion
err <- 1-sum(diag(Confusion))/sum(Confusion)
#Erreur
err
#SVM
t1 = Sys.time()
model <- svm(Class ~. ,Appren)
t2 = Sys.time()
t=t2-t1
#Temps
t
pred=predict(object = model, newdata =Test)
confusion = table(Test$Class,pred)
err <- 1-sum(diag(Confusion))/sum(Confusion)
#Erreur
err

#Random Forest
t1 = Sys.time()
RF=randomForest(Class~., data=Appren)
t2 = Sys.time()
t=t2-t1
#Temps
t
pred=predict(RF,Test, type="class")
confusion = table(Test$Class,pred)
err <- 1-sum(diag(Confusion))/sum(Confusion)
#Erreur
err

#Réseau de neurones
t1 = Sys.time()
RN <- nnet(Class ~ ., data = Appren,decay=0.1, size = 4)
t2 = Sys.time()
t=t2-t1
#Temps
t
pred=predict(RN,Test, type="class")
Confusion = table(Test$Class,pred)
err <- 1-sum(diag(Confusion))/sum(Confusion)
err


#Arebres de décisions
t1 = Sys.time()
tree <- rpart(Class~.,data=Appren)
t2 = Sys.time()
t=t2-t1
#Temps
t
pred=predict(tree,Test, type="class")
Confusion = table(Test$Class,pred)
err <- 1-sum(diag(Confusion))/sum(Confusion)
err