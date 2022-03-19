library(e1071)
library(caret)

#Calculo de percentiles
db<-read.csv('train.csv')
percentil <- quantile(db$SalePrice)


#Percentiles
estado<-c('Estado')
db$Estado<-estado


#Economica=0
#Intermedia=1
#Cara=2
db <- within(db, Estado[SalePrice<=129975] <- 'Economica')

db$Estado[(db$SalePrice>129975 & db$SalePrice<=163000)] <- 'Intermedia'
db$Estado[db$SalePrice>163000] <- 'Cara'

#Bayes 
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
str(test$Estado)
predBayes<-predict(modelo, newdata = test[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])
cm<-caret::confusionMatrix(as.factor(predBayes),as.factor(test$Estado))
cm

#Cross Validation
#GarageYrBlt  MasVnrArea LotFrontage /PoolQC Fence MiscFeature FireplaceQu Alley
#Cambiamos los NA
# 
train[c("GarageYrBlt")][is.na(train[c("GarageYrBlt")])] <- 1979
train[c("MasVnrArea")][is.na(train[c("MasVnrArea")])] <- 0
train[c("LotFrontage")][is.na(train[c("LotFrontage")])] <- 68.00

# train[is.na(train)] <- 0
# #summary(train)
# 
# #train <- subset( train, select = -c('PoolQC') )
# # 
# drop <- c("PoolQC","Fence","MiscFeature","FireplaceQu","Alley")
# train = train[,!(names(train) %in% drop)]
# 
train <- train[ , ! names(train) %in% c("PoolQC","Fence","MiscFeature","FireplaceQu","Alley","MasVnrType","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","GarageType","GarageFinish","GarageQual",'GarageCond')]
# is.na(train)
#train[, colSums(is.na(train)) != nrow(train)]
str(train$MasVnrType)
sum(is.na(train))
#is.na(train[,1])
View(train)
ct<-trainControl(method = "cv",train[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")],number=10, verboseIter=T)
modeloCaret<-train(Estado~ .,data=train,method="nb",trControl = ct,na.action=na.omit)

prediccionCaret<-predict(modeloCaret,newdata = test[,c("GrLivArea","YearBuilt","BsmtUnfSF","TotalBsmtSF","GarageArea","YearRemodAdd", "SalePrice","LotArea")])
caret::confusionMatrix(prediccionCaret,test$Estado)



