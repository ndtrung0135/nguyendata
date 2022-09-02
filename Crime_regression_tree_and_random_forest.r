library(tree)
library(randomForest)
library(rpart)
library(rpart.plot)

#a) regression tree model
tree = rpart(Crime~., data=uscrime, control=rpart.control(cp=0.01))
printcp(tree)
#identify best cp to use
optimal_cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree_pruned = prune(tree,cp = optimal_cp)
prp(tree_pruned,faclen=0,extra=1, roundint=F,digits=7)

#prediction for new city
new_city = data.frame(M= 14.0, So = 0, Ed = 10.0, Po1 = 12.0, Po2 = 15.5,
                       LF = 0.640, M.F = 94.0, Pop = 150, NW = 1.1, U1 = 0.120, 
                       U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.040, 
                       Time = 39.0)
predict1 = predict(tree_pruned, new_city)

SSE = sum((predict1-uscrime[,16])^2)
SST = sum((uscrime[,16]-mean(uscrime[,16]))^2)
Rsq = 1 - SSE/SST
Rsq# R-squared

Rsqadj
#b) random forest model
forest = randomForest(Crime~., uscrime)
forest
which.min(forest$mse)#number of trees that produce lowest MSE
sqrt(forest$mse[which.min(forest$mse)])
#model that produced the lowest test mean squared error (MSE) used 286 trees.
plot(forest)
varImpPlot(forest)
#from the plot we can see that Po1 and Po2 are the most important predictor
predict2 = predict(forest,new_city)
predict2
SSE = sum((predict2-uscrime[,16])^2)
SST = sum((uscrime[,16]-mean(uscrime[,16]))^2)
Rsq = 1 - SSE/SST
Rsq# R-squared
