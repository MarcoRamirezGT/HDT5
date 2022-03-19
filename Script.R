library(e1071)
library(caret)

#Calculo de percentiles
#base de datos a utilzar
db<-read.csv('train.csv')
#Encontramos los percentiles
percentil <- quantile(db$SalePrice)
#Percentiles
estado<-c('Estado')
db$Estado<-estado
db <- within(db, Estado[SalePrice<=129975] <- 'Economica')

db$Estado[(db$SalePrice>129975 & db$SalePrice<=163000)] <- 'Intermedia'
db$Estado[db$SalePrice>163000] <- 'Cara'

#Bayes 
#Usamos el 70% de datos
porcentaje<-0.7
set.seed(1234)

corte <- sample(nrow(db),nrow(db)*porcentaje)
#Entrenamiento
train<-db[corte,]
#Prueba
test<-db[-corte,]
#Entrenar el modelo
modelo<-naiveBayes(train$Estado~., data=train)

test$GrLivArea<-as.numeric(test$GrLivArea)
test$YearBuilt<-as.numeric(test$YearBuilt)
test$BsmtUnfSF<-as.numeric(test$BsmtUnfSF)
test$TotalBsmtSF<-as.numeric(test$TotalBsmtSF)
test$GarageArea<-as.numeric(test$GarageArea)
test$YearRemodAdd<-as.numeric(test$YearRemodAdd)
test$SalePrice<-as.numeric(test$SalePrice)
test$LotArea<-as.numeric(test$LotArea)




table(predBayes)
table(test$Estado)
predBayes<-as.factor(predBayes)

predBayes<-predict(modelo, newdata = test[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])
cm<-caret::confusionMatrix(as.factor(predBayes),as.factor(test$Estado))
cm

#Cross Validation
#GarageYrBlt  MasVnrArea LotFrontage /PoolQC Fence MiscFeature FireplaceQu Alley
#Cambiamos los NA

train[c("GarageYrBlt")][is.na(train[c("GarageYrBlt")])] <- 1979
train[c("MasVnrArea")][is.na(train[c("MasVnrArea")])] <- 0
train[c("LotFrontage")][is.na(train[c("LotFrontage")])] <- 68.00

sum(is.na(train$BsmtExposure))

names(which(colSums(is.na(train))>0)) 


sum(is.na(train$GrLivArea))


ct<-trainControl(method = "cv",train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")],number=10, verboseIter=T)
modeloCaret<-train(Estado~ .,data=train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea","Estado")],method="nb",trControl = ct)

prediccionCaret<-predict(modeloCaret,newdata = test[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])
caret::confusionMatrix(prediccionCaret,as.factor(test$Estado))



