tab = read.table("Test_Classif_dpt.txt",header = T)

plot(tab$x1,tab$x2,col=tab$Y)

tab$Y = as.factor(tab$Y)

#bases d'apprentissage et test
n = nrow(tab)
ntrain = floor(2*n/3)
index = sample(1:n,800,replace=F) #sans remise -> replace = F
train = tab[index,]
test = tab[-index,]

prop.table(table(tab$Y))
prop.table(table(train$Y))
prop.table(table(test$Y))

#modele 
model.NB = naiveBayes(Y~., data=train)


#modele lda
model.lda=lda(Y~.,data=train)
print(model.lda)
prev.lda = predict(model.lda,train)$class  #erreur ajustement #pose pb 
matConf = table(train$Y,prev.lda)
print(matConf)
tb = sum(diag(matConf))/nrow(train)
print(tb)
plot(train$x1,train$x2,col = as.numeric(prev.lda))

#avec lda on a la même corrélation partout

#modele qdq
model.qda = qda(Y~.,data=train)
print(model.qda)
prev.qda = predict(model.qda,train)$class  #erreur ajustement #pose pb 
matConf.qda = table(train$Y,prev.qda)
print(matConf.qda)
tb = sum(diag(matConf.qda))/nrow(train)
print(tb)
plot(train$x1,train$x2,col = as.numeric(prev.qda))

#pour qda on a pas la même corrélation partout


#comparaison des méthodes sur la base test

#modele lda
model.lda=lda(Y~.,data=test)
print(model.lda)
prev.lda = predict(model.lda,test)$class  #erreur ajustement #pose pb 
matConf = table(test$Y,prev.lda)
print(matConf)
tb.lda = sum(diag(matConf))/nrow(test)
print(tb.lda)
plot(test$x1,test$x2,col = as.numeric(prev.lda))
plot(test$x1,test$x2,col = Y,main="base test")
#avec lda on a la même corrélation partout

#modele qdq
model.qda = qda(Y~.,data=test)
print(model.qda)
prev.qda = predict(model.qda,test)$class  #erreur ajustement #pose pb 
matConf.qda = table(test$Y,prev.qda)
print(matConf.qda)
tb.qda = sum(diag(matConf.qda))/nrow(test)
print(tb.qda)
plot(test$x1,test$x2,col = as.numeric(prev.qda))


par(mfcol=c(1,3))
plot(test$x1,test$x2,col = test$Y,main="base test")
plot(test$x1,test$x2,col = as.numeric(prev.lda),main="lda")
plot(test$x1,test$x2,col = as.numeric(prev.qda),main="qda")
























#model qda
model.qda = qda(Y~., data=train)

#erreur ajustement
prev.NB = predict(model.NB,train)

prev.qda = predict(model.qda,train)    #pose pb 

MatConf.NB = table(tab$Y,prev.NB)
MatConf.lda = table(tab$Y,prev.lda)$class
MatConf.qda = table(tab$Y,prev.qda)$class

tb.NB = sum(diag(MatConf.NB))/ntrain
tb.lda = sum(diag(MatConf.lda))/ntrain
tb.qda = sum(diag(MatConf.qda))/ntrain