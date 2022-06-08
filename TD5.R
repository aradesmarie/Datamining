data(iris)

#toujours centrer réduire avant de faire un réseau de neurones : 
iris[,1:4] = scale(iris[,1:4])
apply(iris[,1:4],2,mean)  #2 pour dire de faire colonne par colonne. Si on avait voulu ligne on aurait mis 1
apply(iris[,1:4],2,sd)  #sd = standard déviation = écart type
NN=nnet(Species~.,data=iris,size=5,maxit=1000,decay=0)
View(NN)
print(NN)
summary(NN)
plotnet(NN)

res=tune.nnet(Species~.,data=iris,size=c(2,5,10),maxit=1000,decay=c(0,0.5))
plot(res) #on veut la meilleure perf donc le bleu foncé
NN2 = res$best.model
#question 1d) c'est softmax
#fonction tune.nnet  est dans le package e1071

predict(NN2,iris[1:4,]) #prévision des probabilités
predict(NN2,iris[1:4,],type="class")  #prévision des classes

prev=predict(NN2,iris,type="class")
MatConf=table(iris$Species,prev)
print(MatConf)
NN=nnet(Sepal.Length~.,data=iris,size=2)
plotnet(NN)

######### autre jeu de données  ########
tab=read.table("Landsat.txt",header=T)
tab[,1:36] = scale(tab[,1:36])
n=nrow(tab)
ntrain=floor(2/3 * n)
index = sample(1:n,ntrain,replace=F) #sans remise -> replace = F
train = tab[index,]
test = tab[-index,]
## sur la base d'apprentissage ##
apply(train[,1:36],2,mean)  #2 pour dire de faire colonne par colonne. Si on avait voulu ligne on aurait mis 1
apply(train[,1:36],2,sd)  #sd = standard déviation = écart type
NN=nnet(classes~.,data=train,size=5,maxit=1000,decay=0)
print(NN)
summary(NN)
plotnet(NN)
# res=tune.nnet(classes~.,data=train,size=c(2,5,10),maxit=1000,decay=c(0,0.5))
plot(res) #on veut la meilleure perf donc le bleu foncé
NN2 = res$best.model

predict(NN2,train[1:36,]) #prévision des probabilités
predict(NN2,train[1:36,],type="class")  #prévision des classes

prev=predict(NN2,train,type="class")
MatConf=table(train$classes,prev)
print(MatConf)

