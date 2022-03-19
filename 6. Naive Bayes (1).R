library(e1071)
library(caret)
porcentaje<-0.7
datos<-iris
set.seed(1234)

corte <- sample(nrow(datos),nrow(datos)*porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

modelo<-naiveBayes(train$Species~., data=train)
str(test$Petal.Length)
predBayes<-predict(modelo, newdata = test[,1:4])
cm<-caret::confusionMatrix(predBayes,test$Species)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   setosa versicolor virginica
# setosa         12          0         0
# versicolor      0         16         2
# virginica       0          1        14
# 
# Overall Statistics
# 
# Accuracy : 0.9333         
# 95% CI : (0.8173, 0.986)
# No Information Rate : 0.3778         
# P-Value [Acc > NIR] : 6.255e-15      
# 
# Kappa : 0.8989         
# Mcnemar's Test P-Value : NA             

# Statistics by Class:
# 
#                      Class: setosa Class: versicolor Class: virginica
# Sensitivity                 1.0000            0.9412           0.8750
# Specificity                 1.0000            0.9286           0.9655
# Pos Pred Value              1.0000            0.8889           0.9333
# Neg Pred Value              1.0000            0.9630           0.9333
# Prevalence                  0.2667            0.3778           0.3556
# Detection Rate              0.2667            0.3556           0.3111
# Detection Prevalence        0.2667            0.4000           0.3333
# Balanced Accuracy           1.0000            0.9349           0.9203

ct<-trainControl(method = "cv",train[,1:4],number=10, verboseIter=T)
modeloCaret<-train(Species~.,data=train,method="nb",trControl = ct)
prediccionCaret<-predict(modeloCaret,newdata = test[,1:4])
caret::confusionMatrix(prediccionCaret,test$Species)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   setosa versicolor virginica
# setosa         12          0         0
# versicolor      0         15         2
# virginica       0          2        14
# 
# Overall Statistics
# 
# Accuracy : 0.9111          
# 95% CI : (0.7878, 0.9752)
# No Information Rate : 0.3778          
# P-Value [Acc > NIR] : 1.099e-13       
# 
# Kappa : 0.8653          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: setosa Class: versicolor Class: virginica
# Sensitivity                 1.0000            0.8824           0.8750
# Specificity                 1.0000            0.9286           0.9310
# Pos Pred Value              1.0000            0.8824           0.8750
# Neg Pred Value              1.0000            0.9286           0.9310
# Prevalence                  0.2667            0.3778           0.3556
# Detection Rate              0.2667            0.3333           0.3111
# Detection Prevalence        0.2667            0.3778           0.3556
# Balanced Accuracy           1.0000            0.9055           0.9030