tab = read.table("letter-recognition.data", header = F,sep = ',')

n = nrow(tab)
ntrain = floor(2*n/3)
index = sample(1:n,ntrain,replace=F) #sans remise -> replace = F
train = tab[index,]
test = tab[-index,]


tab$V1 = as.factor(tab$V1)
prop.table(table(tab$V1))
prop.table(table(train$V1))
prop.table(table(test$V1))

foret = randomForest(V1~.,data = train,mtry=2,ntree=100)
show(foret)
plot(foret$err.rate[,1],type='l')
varImpPlot(foret)


arbre = rpart(V1~.,data = train)
prp(arbre)
#V14 et V16 apparissent souvent, V13 et V15 et V10 aussi -> 


varImpPlot(foret)

#calcul des erreurs
prev = predict(arbre,test,type='class')
MatConf=table(test$V1,prev)
tb.foret=sum(diag(MatConf))/nrow(test)


arbre = ntree