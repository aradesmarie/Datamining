tab = read.table("Test_Classif_Correl.txt",header = T)

plot(tab$X1,tab$X2,col=tab$classes)
tab$classes=as.factor(tab$classes)

# Étude des variables
c1 = subset(tab,classes=="1")
hist(c1$X1)
hist(c1$X2)
c2 = subset(tab,classes=="2")
hist(c2$X1)
hist(c2$X2)
c3 = subset(tab,classes=="3")
hist(c3$X1)
hist(c3$X2)
cor(c1[,1:2])
cor(c2[,1:2])
cor(c3[,1:2])

##peut eêtre qda sera bien mais surtout pas lda


#base d'apprentissage et test
#naive bayes
model.NB = naiveBayes(classes~., data=tab)
print(model.NB)
prev.NB = predict(model.NB,tab)
MatConf.NB = table(tab$classes,prev.NB)
tb.NB = sum(diag(MatConf.NB))/1000

#modele lda
model.lda=lda(classes~.,data=tab)
print(model.lda)
prev.lda = predict(model.lda,tab)$class  #erreur ajustement #pose pb 
matConf.lda = table(tab$classes,prev.lda)
print(matConf.lda)
tb.lda = sum(diag(matConf.lda))/nrow(tab)
print(tb.lda)
plot(tab$X1,tab$X2,col = as.numeric(prev.lda))

#modele qdq
model.qda = qda(classes~.,data=tab)
print(model.qda)
prev.qda = predict(model.qda,tab)$class  #erreur ajustement #pose pb 
matConf.qda = table(tab$classes,prev.qda)
print(matConf.qda)
tb.qda = sum(diag(matConf.qda))/nrow(tab)
print(tb.qda)
plot(tab$X1,tab$X2,col = as.numeric(prev.qda))


par(mfcol=c(1,4))
plot(tab$X1,tab$X2,col = tab$classes,main="base test")
plot(tab$X1,tab$X2,col = as.numeric(prev.lda),main="lda")
plot(tab$X1,tab$X2,col = as.numeric(prev.qda),main="qda")
plot(tab$X1,tab$X2,col = as.numeric(prev.NB),main="NB")
