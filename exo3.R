data("iris")

modele=randomForest(Species~.,iris,importance=T)
show(modele)
modele$oob.times

modele$importance
varImpPlot(modele)

Newiris=as.data.frame(t(c(4,3,1,2))) # Il faut un data.frame dans un predict # t=transpose
names(Newiris)=names(iris)[1:4]
# Il faut les mêmes noms que ceux de la base d’apprentissage
predict(modele,Newiris)
predict(modele,Newiris,type="prob")  #prob pour avoir la probabilité de chaque classe

foret1=randomForest(Species ~ ., iris, ntree = 100)
foret2=randomForest(Species ~ ., iris, ntree = 500)
modele=combine(foret1, foret2)
#varImpPlot(modele)
