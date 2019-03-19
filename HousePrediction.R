#Importing Data Set
trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")

#Creating Versions of Data

train <- read.csv("train.csv")
test <- read.csv("test.csv")

#Performing EDA

library(Hmisc)
library(psych)

#Column Names
names(trainData)

describe(trainData)
(tail(trainData,6))

#Box Plot representations 
summary(trainData)

cubeRoot <- function(x) {
  sign(x) * abs(x)^(1/3)
}


#Performing EDA on each variable:
trainData$MSSubClass <- ordered(trainData$MSSubClass , levels=c(min(trainData$MSSubClass):max(trainData$MSSubClass)))
str(trainData$MSSubClass)
unique(trainData$MSSubClass)

#MSZoning
sum(is.na(trainData$MSZoning))
unique(trainData$MSZoning)
trainData$MSZoning <-  ordered(trainData$MSZoning, levels = c("FV","RL","RM","RH","C (all)"))
class(trainData$MSZoning)

#LotFrontage
sum(is.na(trainData$LotFrontage)) #259 Missing Values
median(trainData$LotFrontage,na.rm = T)
boxplot(trainData$LotFrontage,horizontal = T)
trainData$LotFrontage <- ifelse(trainData$LotFrontage==313,182,trainData$LotFrontage)
trainData$LotFrontage <- ifelse(is.na(trainData$LotFrontage),median(trainData$LotFrontage,na.rm = T),trainData$LotFrontage)
describe(trainData$LotFrontage)
class(trainData$LotFrontage)
#From Numeric to Integer
trainData$LotFrontage <- as.integer(trainData$LotFrontage)

#LotArea
sum(is.na(trainData$LotArea)) # No missing Values

#Street
sum(is.na(trainData$Street)) #Categrical and No Missing Values
trainData$Street <- ordered(trainData$Street , levels = c("Grvl","Pave"))
str(trainData$Street)

#Alley
(unique(trainData$Alley))
#Converting it to string to avoid lable encoding
trainData$Alley <- as.character(trainData$Alley)
sum(is.na(trainData$Alley))
dummyVar <- ifelse(is.na(trainData$Alley),"No Alley" , trainData$Alley)
unique(trainData$Alley)
trainData$Alley <- dummyVar
trainData$Alley <- ordered(trainData$Alley , levels = c("No Alley","Grvl","Pave"))

#LotShape
sum(is.na(trainData$LotShape)) #NO Missing values and inconsistencies
trainData$LotShape <- ordered(trainData$LotShape,levels = c("IR3","IR2","IR1","Reg"))
unique(trainData$LotShape)

#LandContour
sum(is.na(trainData$LandContour)) #NO Missing values and inconsistencies
unique(trainData$LandContour)
trainData$LandContour <- ordered(trainData$LandContour,levels = c("Low","HLS","Bnk","Lvl"))
unique(trainData$LandContour)

#Utilities
sum(is.na(trainData$Utilities)) #NO Missing values and inconsistencies
unique(trainData$Utilities)
trainData$Utilities <- ordered(trainData$Utilities,levels = c("ELO","NoSeWa","NoSeWr","AllPub"))
unique(trainData$Utilities)

#LotConfig
sum(is.na(trainData$LotConfig)) #NO Missing values and inconsistencies
unique(trainData$LotConfig)

#LandSlope
sum(is.na(trainData$LandSlope)) #NO Missing values and inconsistencies
unique(trainData$LandSlope)
trainData$LandSlope <- ordered(trainData$LandSlope,levels = c("Sev","Mod","Gtl"))

#Neighborhood
sum(is.na(trainData$Neighborhood)) #NO Missing values and inconsistencies
unique(trainData$Neighborhood)

#Condition1
sum(is.na(trainData$Condition1)) #NO Missing values and inconsistencies
unique(trainData$Condition1)

#Condition2
sum(is.na(trainData$Condition2)) #NO Missing values and inconsistencies
unique(trainData$Condition2)

#BldgType
sum(is.na(trainData$BldgType))
unique(trainData$BldgType)

#HouseStyle
sum(is.na(trainData$HouseStyle))
unique(trainData$HouseStyle)

#OverallQual
sum(is.na(trainData$OverallQual))
trainData$OverallQual <- ordered(trainData$OverallQual , levels= c(1:10))
unique(trainData$OverallQual)

#OverallCond
sum(is.na(trainData$OverallCond))
trainData$OverallCond <- ordered(trainData$OverallCond , levels= c(1:10))
unique(trainData$OverallCond)

#YearBuilt
CurrentYear <- c(as.integer(format(Sys.Date(),"%Y")))
dummyVar1 <- CurrentYear - trainData$YearBuilt
dummyVar1 <- ordered(dummyVar1,levels = c(max(dummyVar1):min(dummyVar1)))
trainData$BuildingAge <- dummyVar1
unique(trainData$BuildingAge)

#YearRemodAdd     ##NEED TO LOOK INTO IT IN A DIFFERENT WAY
dummyVar1 <- CurrentYear - trainData$YearRemodAdd
dummyVar1 <- ordered(dummyVar1,levels = c(max(dummyVar1):min(dummyVar1)))
trainData$Revonated <- dummyVar1
unique(trainData$Revonated)

#RoofStyle
sum(is.na(trainData$RoofStyle))
unique(trainData$RoofStyle)

#RoofMatl
sum(is.na(trainData$RoofMatl))
unique(trainData$RoofMatl)

#Exterior1st
sum(is.na(trainData$Exterior1st))
unique(trainData$Exterior1st)

#Exterior2nd
sum(is.na(trainData$Exterior2nd))
unique(trainData$Exterior2nd)

#MasVnrType
sum(is.na(trainData$MasVnrType)) #8 NAs present. Chaning it to NONE
trainData$MasVnrType <- as.character(trainData$MasVnrType)
unique(trainData$MasVnrType)
trainData$MasVnrType <- ifelse(is.na(trainData$MasVnrType),"None",trainData$MasVnrType)
trainData$MasVnrType <- as.factor(trainData$MasVnrType)
unique(trainData$MasVnrType)

#MasVnrArea
sum(is.na(trainData$MasVnrArea))
trainData$MasVnrArea <- ifelse(is.na(trainData$MasVnrArea),0,trainData$MasVnrArea)

# #ExterQual
# sum(is.na(trainData$ExterQual))
# trainData$ExterQual <- ordered(trainData$ExterQual,levels = c("Po","Fa","TA","Gd","Ex"))
# unique(trainData$ExterQual)

#ExterQual
trainData$ExterQual <-  as.factor(trainData$ExterQual)
class(trainData$ExterQual)

#ExterCond
sum(is.na(trainData$ExterCond))
trainData$ExterCond <- ordered(trainData$ExterCond,levels = c("Po","Fa","TA","Gd","Ex"))
unique(trainData$ExterCond)

#Foundation
sum(is.na(trainData$Foundation))
trainData$Foundation <- ordered(trainData$Foundation,levels = c("Wood","BrkTil","Stone","Slab","PConc","CBlock"))
unique(trainData$Foundation)

# #BsmtQual
# sum(is.na(trainData$BsmtQual))
# trainData$BsmtQual <- train$BsmtQual
# trainData$BsmtQual <- as.character(trainData$BsmtQual)
# unique(trainData$BsmtQual)
# trainData$BsmtQual <- ifelse(is.na(trainData$BsmtQual),"NoBase",trainData$BsmtQual)
# trainData$BsmtQual <- ordered(trainData$BsmtQual,levels = c("NoBase","Po","Fa","TA","Gd","Ex"))

#BsmtQual
class(trainData$BsmtQual)
trainData$BsmtQual <- as.character(trainData$BsmtQual)
trainData$BsmtQual <- as.factor(trainData$BsmtQual)
unique(trainData$BsmtQual)

#BsmtCond
sum(is.na(trainData$BsmtCond))
trainData$BsmtCond <- train$BsmtCond
trainData$BsmtCond <- as.character(trainData$BsmtCond)
unique(trainData$BsmtCond)
trainData$BsmtCond <- ifelse(is.na(trainData$BsmtCond),"NoBase",trainData$BsmtCond)
trainData$BsmtCond <- ordered(trainData$BsmtCond , levels = c("NoBase","Po","Fa","TA","Gd","Ex"))

#BsmtExposure
sum(is.na(trainData$BsmtExposure))
trainData$BsmtExposure <- as.character(trainData$BsmtExposure)
unique(trainData$BsmtExposure)
trainData$BsmtExposure <- ifelse(is.na(trainData$BsmtExposure),"NoBase",trainData$BsmtExposure)
trainData$BsmtExposure <- ordered(trainData$BsmtExposure, levels =c("NoBase","No","Mn","Av","Gd"))

#BsmtFinType1
sum(is.na(trainData$BsmtFinType1))
trainData$BsmtFinType1 <- as.character(trainData$BsmtFinType1)
unique(trainData$BsmtFinType1)
trainData$BsmtFinType1 <- ifelse(is.na(trainData$BsmtFinType1),"NoBase",trainData$BsmtFinType1)
trainData$BsmtFinType1 <- ordered(trainData$BsmtFinType1, levels=c("NoBase","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

#BsmtFinSF1
sum(is.na(trainData$BsmtFinSF1))

#BsmtFinType2
sum(is.na(trainData$BsmtFinType2))
trainData$BsmtFinType2 <- as.character(trainData$BsmtFinType2)
unique(trainData$BsmtFinType2)
trainData$BsmtFinType2 <- ifelse(is.na(trainData$BsmtFinType2),"NoBase",trainData$BsmtFinType2)
trainData$BsmtFinType2 <- ordered(trainData$BsmtFinType2, levels= c("NoBase","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))

#BsmtFinSF2
sum(is.na(trainData$BsmtFinSF2))

#BsmtUnfSF
sum(is.na(trainData$BsmtUnfSF))

#TotalBsmtSF  # THis is sum of BSMTUNFSF + BSMTFINSF 2 + BSMTFINSF 1
sum(is.na(trainData$TotalBsmtSF))

#Heating
sum(is.na(trainData$Heating))
unique(trainData$Heating)

#HeatingQC
sum(is.na(trainData$HeatingQC))
trainData$HeatingQC <- train$HeatingQC
unique(trainData$HeatingQC)
trainData$HeatingQC <- ordered(trainData$HeatingQC,levels = c("Po","Fa","TA","Gd","Ex"))
unique(trainData$HeatingQC)

#CentralAir
sum(is.na(trainData$CentralAir))
trainData$CentralAir <- ordered(trainData$CentralAir , levels = c("N","Y"))
unique(trainData$CentralAir)

#Electrical
sum(is.na(trainData$Electrical))
trainData$Electrical <- as.character(trainData$Electrical)
table(trainData$Electrical)
trainData$Electrical <- ifelse(is.na(trainData$Electrical), "SBrkr" , trainData$Electrical)
trainData$Electrical <- ordered(trainData$Electrical, levels=c("Mix","FuseP","FuseF","FuseA","SBrkr"))
unique(trainData$Electrical)

#1stFlrSF
sum(is.na(trainData$X1stFlrSF))

#2ndFlrSF
sum(is.na(trainData$X2ndFlrSF))

#LowQualFinSF
sum(is.na(trainData$LowQualFinSF))

#GrLivArea #THis is sum of 1st floor sf + 2 floor sf + low qual fin sf
sum(is.na(trainData$GrLivArea))

#BsmtFullBath
sum(is.na(trainData$BsmtFullBath))
trainData$BsmtFullBath <- ordered(trainData$BsmtFullBath,levels = c(min(trainData$BsmtFullBath):max(trainData$BsmtFullBath)))
unique(trainData$BsmtFullBath)

#BsmtHalfBath
sum(is.na(trainData$BsmtHalfBath))
trainData$BsmtHalfBath <- ordered(trainData$BsmtHalfBath , levels = c(min(trainData$BsmtHalfBath):max(trainData$BsmtHalfBath)))
unique(trainData$BsmtHalfBath)

#FullBath
sum(is.na(trainData$FullBath))
trainData$FullBath <- ordered(trainData$FullBath , levels =c(min(trainData$FullBath):max(trainData$FullBath)))
unique(trainData$FullBath)

#HalfBath
sum(is.na(trainData$HalfBath))
trainData$HalfBath <- ordered(trainData$HalfBath,levels = c(min(trainData$HalfBath):max(trainData$HalfBath)))
unique(trainData$HalfBath)

#BedroomAbvGr
sum(is.na(trainData$BedroomAbvGr))
trainData$BedroomAbvGr <- ordered(trainData$BedroomAbvGr , levels = c(min(trainData$BedroomAbvGr):max(trainData$BedroomAbvGr)))
unique(trainData$BedroomAbvGr)

#KitchenAbvGr
sum(is.na(trainData$KitchenAbvGr))
trainData$KitchenAbvGr <- ordered(trainData$KitchenAbvGr , levels = c(min(trainData$KitchenAbvGr):max(trainData$KitchenAbvGr)))
unique(trainData$KitchenAbvGr)

# #KitchenQual
# sum(is.na(trainData$KitchenQual))
# trainData$KitchenQual <- train$KitchenQual
# unique(trainData$KitchenQual)
# trainData$KitchenQual <- ordered(trainData$KitchenQual,levels = c("Po","Fa","TA","Gd","Ex"))

#KitchenQual
trainData$KitchenQual <- as.character(trainData$KitchenQual)
trainData$KitchenQual <- as.factor(trainData$KitchenQual)
unique(trainData$KitchenQual)

#TotRmsAbvGrd
sum(is.na(trainData$TotRmsAbvGrd))
trainData$TotRmsAbvGrd <- ordered(trainData$TotRmsAbvGrd , levels = c(min(trainData$TotRmsAbvGrd):max(trainData$TotRmsAbvGrd)))
unique(trainData$TotRmsAbvGrd)

#Functional
sum(is.na(trainData$Functional))
trainData$Functional <- ordered(trainData$Functional,levels = c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"))
unique(trainData$Functional)

#Fireplaces
sum(is.na(trainData$Fireplaces))
trainData$Fireplaces <- ordered(trainData$Fireplaces , levels = c(min(trainData$Fireplaces):max(trainData$Fireplaces)))
unique(trainData$Fireplaces)

#FireplaceQu
sum(is.na(trainData$FireplaceQu))
trainData$FireplaceQu <- as.character(trainData$FireplaceQu)
trainData$FireplaceQu <- ifelse(is.na(trainData$FireplaceQu),"NoFirePlc",trainData$FireplaceQu)
unique(trainData$FireplaceQu)
trainData$FireplaceQu <- ordered(trainData$FireplaceQu , levels=c("NoFirePlc","Po","Fa","TA","Gd","Ex"))

#GarageType
sum(is.na(trainData$GarageType))
trainData$GarageType <- as.character(trainData$GarageType)
trainData$GarageType <- ifelse(is.na(trainData$GarageType),"NoGarage",trainData$GarageType)
unique(trainData$GarageType)
trainData$GarageType <- ordered(trainData$GarageType, levels=c("NoGarage","Detchd","CarPort","BuiltIn","Basment","Attchd","2Types"))

#GarageYrBlt We are going to eliminate this column.
sum(is.na(trainData$GarageYrBlt))
dummyVar <- trainData$GarageYrBlt
dummyVar <- ifelse(is.na(dummyVar),CurrentYear,dummyVar)

#GarageFinish
unique(trainData$GarageFinish)
sum(is.na(trainData$GarageFinish))
trainData$GarageFinish <- as.character(trainData$GarageFinish)
trainData$GarageFinish <- ifelse(is.na(trainData$GarageFinish),"NoGarage",trainData$GarageFinish)
trainData$GarageFinish <-  ordered(trainData$GarageFinish,levels=c("NoGarage","Unf","RFn","Fin"))
unique(trainData$GarageFinish)

#GarageCars
unique(trainData$GarageCars)
trainData$GarageCars <- ordered(trainData$GarageCars, levels= c(min(trainData$GarageCars):max(trainData$GarageCars)))

#GarageArea
class(trainData$GarageArea)
sum(is.na(trainData$GarageArea))

#GarageQual
sum(is.na(trainData$GarageQual))
trainData$GarageQual <- as.character(trainData$GarageQual)
trainData$GarageQual <- ifelse(is.na(trainData$GarageQual),"NoGarage",trainData$GarageQual)
unique(trainData$GarageQual)
trainData$GarageQual <- ordered(trainData$GarageQual,levels =c("NoGarage","Po","Fa","TA","Gd","Ex"))

#GarageCond
sum(is.na(trainData$GarageCond))
trainData$GarageCond <- as.character(trainData$GarageCond)
trainData$GarageCond <- ifelse(is.na(trainData$GarageCond),"NoGarage",trainData$GarageCond)
unique(trainData$GarageCond)
trainData$GarageCond <- ordered(trainData$GarageCond,levels =c("NoGarage","Po","Fa","TA","Gd","Ex"))

#PavedDrive
sum(is.na(trainData$PavedDrive))
unique(trainData$PavedDrive)
trainData$PavedDrive <- ordered(trainData$PavedDrive,levels=c("N","P","Y"))

#WoodDeckSF
sum(is.na(trainData$WoodDeckSF))

#OpenPorchSF
sum(is.na(trainData$OpenPorchSF))

#EnclosedPorch
sum(is.na(trainData$EnclosedPorch))

#3SsnPorch
sum(is.na(trainData$X3SsnPorch))

#ScreenPorch
sum(is.na(trainData$ScreenPorch))

#PoolArea
sum(is.na(trainData$PoolArea))

#PoolQC
sum(is.na(trainData$PoolQC))
trainData$PoolQC <- as.character(trainData$PoolQC)
unique(trainData$PoolQC)
trainData$PoolQC <- ifelse(is.na(trainData$PoolQC),"NoPool",trainData$PoolQC)
unique(trainData$PoolQC)
trainData$PoolQC  <-  ordered(trainData$PoolQC,levels=c("NoPool","Fa","TA","Gd","Ex"))

#Fence
sum(is.na(trainData$Fence))
trainData$Fence <- as.character(trainData$Fence)
trainData$Fence <- ifelse(is.na(trainData$Fence),"NoFence",trainData$Fence)
trainData$Fence <- ordered(trainData$Fence,levels=c("NoFence","MnWw","GdWo","MnPrv","GdPrv"))
unique(trainData$Fence)

#MiscFeature
sum(is.na(trainData$MiscFeature))
trainData$MiscFeature <- as.character(trainData$MiscFeature)
trainData$MiscFeature <- ifelse(is.na(trainData$MiscFeature),"None",trainData$MiscFeature)
trainData$MiscFeature <- as.factor(trainData$MiscFeature)
unique(trainData$MiscFeature)

#MiscVal
sum(is.na(trainData$MiscVal))

#MoSold
sum(is.na(trainData$MoSold))
class(trainData$MoSold)
trainData$MoSold <- as.factor(trainData$MoSold)

#YrSold
sum(is.na(trainData$YrSold))
class(trainData$YrSold)
trainData$YrSold <- as.factor(trainData$YrSold)

#SaleType
sum(is.na(trainData$SaleType))
class(trainData$SaleType)

#SaleCondition
sum(is.na(trainData$SaleCondition))
class(trainData$SaleCondition)

#Checking for missing Values for Any post Missing Value imputation
sapply(trainData,function(x) sum(is.na(x)))

#Considering Only required Column Based on EDA performed
#Taking Up Backup 
beforeRemovalOfColumns <- trainData
trainData <- trainData[ , !(names(trainData) %in% c("Id","LowQualFinSF","X2ndFlrSF","X1stFlrSF","YearBuilt","YearRemodAdd","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","GarageYrBlt"))]

write.csv(trainData,"SoFarManipulated.csv")

describe(trainData)
#Skewness are there in LotArea,
#In other variales having skewness, we cannot perform transformations since most of the values are 0

trainData$LotArea <- cubeRoot(trainData$LotArea)
describe(trainData$LotArea)

#Splitting the data to train and test
# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(trainData$SalePrice, SplitRatio = 0.8)
training_Data = subset(trainData, split == TRUE)
test_Data = subset(trainData, split == FALSE)

#Debugging the code based on the error
#Error that I got 

#Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#contrasts can be applied only to factors with 2 or more levels

(chechingValues <- sapply(trainData, function(x) is.factor(x)))
m <- trainData[,chechingValues]
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
str(trainData)

#Performing Analysis Part
LinearModel_Train <- lm(SalePrice ~ . , data = training_Data)


#ERROR that I get:

#Error in ctrfn(levels(x), contrasts = contrasts) : 
#  orthogonal polynomials cannot be represented accurately enough for 106 degrees of freedom
