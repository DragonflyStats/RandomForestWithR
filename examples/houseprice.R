This is my first attempt in any machine learning problem . Tried to build a basic model and open to suggestions.

In this house price predicted problem i have used random forest approach with regression trees. Initially i made a simple model including all the variables and then omitted the variables whose contribution is not significant .

# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(textir) ## needed to standardize the data
library(class) ## needed for knn
library(ggplot2) # visualization
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(randomForest) # random forest
library(corrplot) # correlation
library(gridExtra)
library(GGally)
library(e1071)
## read data
house_train <- read.csv("../input/train.csv")




# Summary of each of the variables in training set
str(house_train)




# Check missing values . Identify the columns which have missing values 
mvc = 0
for (i in 1:ncol(house_train))
{
  m = sum(is.na(house_train[,i]))
  print(paste("Column ",colnames(house_train[i])," has ",m," missing values"))
  if(m>0){
    mvc = mvc+1
  }
  else{
    mvc
    }
}  
print(paste("Dataset has overall ",mvc," columns with missing values"))
# Output shows we have around 19 columns with missing values


# Check relationship between LotFrontage and LotArea
# Assigned LotFrontage values as 500 to the missing value in order to plot the relationship of all the LotFrontage
# including the missing values with price
house_train$LotFrontage[is.na(house_train$LotFrontage)] <- 500





# Plotting median of LotArea for each LotFrontage
ggplot(house_train, aes(x = LotFrontage, y = LotArea)) +
  stat_summary(fun.y = "median", geom  = "bar", fill="red")+
  labs(x = 'LotFrontage') +
  theme_few()




# Checking mean and median of LotArea for missing LotFrontage
median(subset(house_train, house_train$LotFrontage == 500)$LotArea)
mean(subset(house_train, house_train$LotFrontage == 500)$LotArea)

# Plotting reference line with the median
plot1 <- ggplot(house_train, aes(x = LotFrontage, y = LotArea)) +
  stat_summary(fun.y = "median", geom  = "bar", fill="red")+geom_hline(aes(yintercept=10624), 
                                                                       colour='red', linetype='dashed', lwd=1)+
  labs(x = 'LotFrontage') +
  scale_x_continuous(breaks=house_train$LotFrontage) +
  theme_few()
# plot with "zoomed region"
plot1 + coord_cartesian(xlim = c(90,150),ylim= c(0,150000))

# Assign missing values of LotFrontage to 96 as missing values have same median with LotFrontage ranging from 90 t0 100
house_train$LotFrontage[house_train$LotFrontage == 500] <- 96


# Alley access with NA means no alley access. So we need to replace it with empty or blank value
house_train$Alley <- as.character(house_train$Alley)
house_train$Alley[is.na(house_train$Alley)] <- 'No alley access'
house_train$Alley <- factor(house_train$Alley)
str(house_train$Alley)



# Missing value for MasVnrType, MasVnrArea and Electrical
# Since only 9 rows altoghether are missing we can omit them from the dataset
house_train <- house_train[!is.na(house_train$MasVnrType),]
house_train <- house_train[!is.na(house_train$Electrical),]

# Missing value for houses without basement 
house_train$BsmtQual <- as.character(house_train$BsmtQual)
house_train$BsmtQual[is.na(house_train$BsmtQual)] <- 'No Basement'
house_train$BsmtQual <- factor(house_train$BsmtQual)

house_train$BsmtCond <- as.character(house_train$BsmtCond)
house_train$BsmtCond[is.na(house_train$BsmtCond)] <- 'No Basement'
house_train$BsmtCond <- factor(house_train$BsmtCond)


house_train$BsmtExposure <- as.character(house_train$BsmtExposure)
house_train$BsmtExposure[is.na(house_train$BsmtExposure)] <- 'No Basement'
house_train$BsmtExposure <- factor(house_train$BsmtExposure)




house_train$BsmtFinType1 <- as.character(house_train$BsmtFinType1)
house_train$BsmtFinType1[is.na(house_train$BsmtFinType1)] <- 'No Basement'
house_train$BsmtFinType1 <- factor(house_train$BsmtFinType1)


house_train$BsmtFinType2 <- as.character(house_train$BsmtFinType2)
house_train$BsmtFinType2[is.na(house_train$BsmtFinType2)] <- 'No Basement'
house_train$BsmtFinType2 <- factor(house_train$BsmtFinType2)

# Missing value for fire  place
house_train$FireplaceQu <- as.character(house_train$FireplaceQu)
house_train$FireplaceQu[is.na(house_train$FireplaceQu)] <- 'No Fireplace'
house_train$FireplaceQu <- factor(house_train$FireplaceQu)

# Missing value for garage  
house_train$GarageType <- as.character(house_train$GarageType)
house_train$GarageType[is.na(house_train$GarageType)] <- 'No Garage'
house_train$GarageType <- factor(house_train$GarageType)


house_train$GarageYrBlt[is.na(house_train$GarageYrBlt)] <- 0


house_train$GarageFinish <- as.character(house_train$GarageFinish)
house_train$GarageFinish[is.na(house_train$GarageFinish)] <- 'No Garage'
house_train$GarageFinish <- factor(house_train$GarageFinish)


house_train$GarageQual <- as.character(house_train$GarageQual)
house_train$GarageQual[is.na(house_train$GarageQual)] <- 'No Garage'
house_train$GarageQual <- factor(house_train$GarageQual)


house_train$GarageCond <- as.character(house_train$GarageCond)
house_train$GarageCond[is.na(house_train$GarageCond)] <- 'No Garage'
house_train$GarageCond <- factor(house_train$GarageCond)


# Missing value for pool  
house_train$PoolQC <- as.character(house_train$PoolQC)
house_train$PoolQC[is.na(house_train$PoolQC)] <- 'No Pool'
house_train$PoolQC <- factor(house_train$PoolQC)


# Missing value for fence
house_train$Fence <- as.character(house_train$Fence)
house_train$Fence[is.na(house_train$Fence)] <- 'No Fence'
house_train$Fence <- factor(house_train$Fence)


# Missing value for other miscellaneous feature
house_train$MiscFeature <- as.character(house_train$MiscFeature)
house_train$MiscFeature[is.na(house_train$MiscFeature)] <- 'None'
house_train$MiscFeature <- factor(house_train$MiscFeature)

# Check if any missing values are there
mvc = 0
for (i in 1:ncol(house_train))
{
  m = sum(is.na(house_train[,i]))
  print(paste("Column ",colnames(house_train[i])," has ",m," missing values"))
  if(m>0){
    mvc = mvc+1
  }
  else{
    mvc
  }
}  
print(paste("Dataset has overall ",mvc," columns with missing values"))

# Thus there are no missing values left in the training dataset. We can start witho our initial prediction

train <- house_train[1:1000,]
test <- house_train[1001:1451,]

# Initial model
# Random forest 
library(randomForest)

set.seed(1000)
output.forest <- randomForest(SalePrice ~ . ,
                              data = train, importance = T)
print(output.forest)
importance(output.forest)

fitForest1 <-predict(output.forest, newdata = test)


importance    <- importance(output.forest)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'%IncMSE'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
print("Plot of variable importance")
print("Variable importance of initial model")
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

first_SSE <- mean((fitForest1-test$SalePrice)^2)
mean(train$SalePrice)
print("Inital prediction")
plot(fitForest1,test$SalePrice,
     xlab="predicted",ylab="actual", xaxt="n")
abline(a=0,b=1)

# Improving the model and feature engineering
# From importance plot it seems Gr living area is the most important variable
# Lets plot its relation with sale price
ggplot(train, aes(x = GrLivArea, y = SalePrice)) +
  geom_bar(stat='identity', position='dodge', fill='red') +
  labs(x = 'BedroomAbvGr') +
  theme_few()

ggplot(train, aes(x = BedroomAbvGr, y = SalePrice)) +
  stat_summary(fun.y = "mean", geom  = "bar", fill="red") +
  labs(x = 'BedroomAbvGr') +
  theme_few()

# Lets create a variable of total rooms
house_train$totalrooms <- house_train$TotRmsAbvGrd+house_train$FullBath+house_train$HalfBath+house_train$BsmtFullBath+house_train$BsmtHalfBath
ggplot(house_train, aes(x = totalrooms, y = SalePrice)) +
  stat_summary(fun.y = "mean", geom  = "bar", fill="red") +
  labs(x = 'totalrooms') +
  theme_few()



# Hence we see a linear relation between our new variable and sale price






# Lets omit variables which are not important from the model


train <- house_train[1:1000,]
test <- house_train[1001:1451,]


set.seed(1000)
output.forest <- randomForest(SalePrice ~ GrLivArea + GarageArea + GarageCars + X2ndFlrSF + LotArea + FullBath + YearBuilt + ExterQual + FireplaceQu
                              + KitchenQual + GarageFinish + BsmtQual + GarageType + totalrooms + OpenPorchSF + YearRemodAdd
                              + MSZoning + Exterior1st + HouseStyle + Fireplaces + BsmtUnfSF + WoodDeckSF + GarageYrBlt + GarageQual
                              + CentralAir ,
                              data = train,  importance = T)
print(output.forest)
importance(output.forest)

fitForest1 <-predict(output.forest, newdata = test)
second_SSE <-  mean((fitForest1-test$SalePrice)^2)
mean(train$SalePrice)
plot(fitForest1,test$SalePrice,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


# Variable importance of the latest model
importance    <- importance(output.forest)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'%IncMSE'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
print("Variable importance after second iteration")
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Further improvement to the model
# Lets separate continous variables and check their correlation with sale price
num <- sapply(house_train, is.numeric)


house_train_num <-  house_train[ , num]
house_train_cat <- house_train[ , !num]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}


# Barplots for the categorical features
print("Barplots for the categorical features")
doPlots(house_train_cat, fun = plotHist, ii = 1:4, ncol = 2)
doPlots(house_train_cat, fun = plotHist, ii = 5:8, ncol = 2)
doPlots(house_train_cat, fun = plotHist, ii = 9:12, ncol = 2)
doPlots(house_train_cat, fun = plotHist, ii = 13:16, ncol = 2)
doPlots(house_train_cat, fun = plotHist, ii = 17:20, ncol = 2)
doPlots(house_train_cat, fun = plotHist, ii = 21:24, ncol = 2)
doPlots(house_train_cat, fun = plotHist, ii = 25:28, ncol = 2)


correlations <- cor(house_train_num)

# correlations
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)

correlations<- correlations[row_indic ,row_indic ]
    print("Correlation plot for the continous features")
corrplot(correlations, method="square")
    
    
# Select all the continous variables with high correlation


train <- house_train[1:1000,]
test <- house_train[1001:1451,]


set.seed(1000)
output.forest <- randomForest(SalePrice ~ OverallQual + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + TotalBsmtSF + X1stFlrSF + 
                                 GrLivArea + totalrooms + Fireplaces + GarageArea + GarageCars + MSZoning + HouseStyle + ExterQual + 
                                KitchenQual + GarageFinish + BsmtQual + GarageType + CentralAir ,
                              data = train,  importance = T)
print(output.forest)
importance(output.forest)

fitForest1 <-predict(output.forest, newdata = test)
third_SSE <- mean((fitForest1-test$SalePrice)^2)
mean(train$SalePrice)
plot(fitForest1,test$SalePrice,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


# Variable importance of the latest model
importance    <- importance(output.forest)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'%IncMSE'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
    print("Variable importance after second iteration")
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()
Loading required package: distrom
Loading required package: Matrix
Loading required package: gamlr
Loading required package: parallel

Attaching package: ‘dplyr’

The following object is masked from ‘package:distrom’:

    collapse

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

randomForest 4.6-12
Type rfNews() to see new features/changes/bug fixes.

Attaching package: ‘randomForest’

The following object is masked from ‘package:dplyr’:

    combine

The following object is masked from ‘package:ggplot2’:

    margin


Attaching package: ‘gridExtra’

The following object is masked from ‘package:randomForest’:

    combine

The following object is masked from ‘package:dplyr’:

    combine


Attaching package: ‘GGally’

The following object is masked from ‘package:dplyr’:

    nasa

'data.frame':	1460 obs. of  81 variables:
 $ Id           : int  1 2 3 4 5 6 7 8 9 10 ...
 $ MSSubClass   : int  60 20 60 70 60 50 20 60 50 190 ...
 $ MSZoning     : Factor w/ 5 levels "C (all)","FV",..: 4 4 4 4 4 4 4 4 5 4 ...
 $ LotFrontage  : int  65 80 68 60 84 85 75 NA 51 50 ...
 $ LotArea      : int  8450 9600 11250 9550 14260 14115 10084 10382 6120 7420 ...
 $ Street       : Factor w/ 2 levels "Grvl","Pave": 2 2 2 2 2 2 2 2 2 2 ...
 $ Alley        : Factor w/ 2 levels "Grvl","Pave": NA NA NA NA NA NA NA NA NA NA ...
 $ LotShape     : Factor w/ 4 levels "IR1","IR2","IR3",..: 4 4 1 1 1 1 4 1 4 4 ...
 $ LandContour  : Factor w/ 4 levels "Bnk","HLS","Low",..: 4 4 4 4 4 4 4 4 4 4 ...
 $ Utilities    : Factor w/ 2 levels "AllPub","NoSeWa": 1 1 1 1 1 1 1 1 1 1 ...
 $ LotConfig    : Factor w/ 5 levels "Corner","CulDSac",..: 5 3 5 1 3 5 5 1 5 1 ...
 $ LandSlope    : Factor w/ 3 levels "Gtl","Mod","Sev": 1 1 1 1 1 1 1 1 1 1 ...
 $ Neighborhood : Factor w/ 25 levels "Blmngtn","Blueste",..: 6 25 6 7 14 12 21 17 18 4 ...
 $ Condition1   : Factor w/ 9 levels "Artery","Feedr",..: 3 2 3 3 3 3 3 5 1 1 ...
 $ Condition2   : Factor w/ 8 levels "Artery","Feedr",..: 3 3 3 3 3 3 3 3 3 1 ...
 $ BldgType     : Factor w/ 5 levels "1Fam","2fmCon",..: 1 1 1 1 1 1 1 1 1 2 ...
 $ HouseStyle   : Factor w/ 8 levels "1.5Fin","1.5Unf",..: 6 3 6 6 6 1 3 6 1 2 ...
 $ OverallQual  : int  7 6 7 7 8 5 8 7 7 5 ...
 $ OverallCond  : int  5 8 5 5 5 5 5 6 5 6 ...
 $ YearBuilt    : int  2003 1976 2001 1915 2000 1993 2004 1973 1931 1939 ...
 $ YearRemodAdd : int  2003 1976 2002 1970 2000 1995 2005 1973 1950 1950 ...
 $ RoofStyle    : Factor w/ 6 levels "Flat","Gable",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ RoofMatl     : Factor w/ 8 levels "ClyTile","CompShg",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ Exterior1st  : Factor w/ 15 levels "AsbShng","AsphShn",..: 13 9 13 14 13 13 13 7 4 9 ...
 $ Exterior2nd  : Factor w/ 16 levels "AsbShng","AsphShn",..: 14 9 14 16 14 14 14 7 16 9 ...
 $ MasVnrType   : Factor w/ 4 levels "BrkCmn","BrkFace",..: 2 3 2 3 2 3 4 4 3 3 ...
 $ MasVnrArea   : int  196 0 162 0 350 0 186 240 0 0 ...
 $ ExterQual    : Factor w/ 4 levels "Ex","Fa","Gd",..: 3 4 3 4 3 4 3 4 4 4 ...
 $ ExterCond    : Factor w/ 5 levels "Ex","Fa","Gd",..: 5 5 5 5 5 5 5 5 5 5 ...
 $ Foundation   : Factor w/ 6 levels "BrkTil","CBlock",..: 3 2 3 1 3 6 3 2 1 1 ...
 $ BsmtQual     : Factor w/ 4 levels "Ex","Fa","Gd",..: 3 3 3 4 3 3 1 3 4 4 ...
 $ BsmtCond     : Factor w/ 4 levels "Fa","Gd","Po",..: 4 4 4 2 4 4 4 4 4 4 ...
 $ BsmtExposure : Factor w/ 4 levels "Av","Gd","Mn",..: 4 2 3 4 1 4 1 3 4 4 ...
 $ BsmtFinType1 : Factor w/ 6 levels "ALQ","BLQ","GLQ",..: 3 1 3 1 3 3 3 1 6 3 ...
 $ BsmtFinSF1   : int  706 978 486 216 655 732 1369 859 0 851 ...
 $ BsmtFinType2 : Factor w/ 6 levels "ALQ","BLQ","GLQ",..: 6 6 6 6 6 6 6 2 6 6 ...
 $ BsmtFinSF2   : int  0 0 0 0 0 0 0 32 0 0 ...
 $ BsmtUnfSF    : int  150 284 434 540 490 64 317 216 952 140 ...
 $ TotalBsmtSF  : int  856 1262 920 756 1145 796 1686 1107 952 991 ...
 $ Heating      : Factor w/ 6 levels "Floor","GasA",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ HeatingQC    : Factor w/ 5 levels "Ex","Fa","Gd",..: 1 1 1 3 1 1 1 1 3 1 ...
 $ CentralAir   : Factor w/ 2 levels "N","Y": 2 2 2 2 2 2 2 2 2 2 ...
 $ Electrical   : Factor w/ 5 levels "FuseA","FuseF",..: 5 5 5 5 5 5 5 5 2 5 ...
 $ X1stFlrSF    : int  856 1262 920 961 1145 796 1694 1107 1022 1077 ...
 $ X2ndFlrSF    : int  854 0 866 756 1053 566 0 983 752 0 ...
 $ LowQualFinSF : int  0 0 0 0 0 0 0 0 0 0 ...
 $ GrLivArea    : int  1710 1262 1786 1717 2198 1362 1694 2090 1774 1077 ...
 $ BsmtFullBath : int  1 0 1 1 1 1 1 1 0 1 ...
 $ BsmtHalfBath : int  0 1 0 0 0 0 0 0 0 0 ...
 $ FullBath     : int  2 2 2 1 2 1 2 2 2 1 ...
 $ HalfBath     : int  1 0 1 0 1 1 0 1 0 0 ...
 $ BedroomAbvGr : int  3 3 3 3 4 1 3 3 2 2 ...
 $ KitchenAbvGr : int  1 1 1 1 1 1 1 1 2 2 ...
 $ KitchenQual  : Factor w/ 4 levels "Ex","Fa","Gd",..: 3 4 3 3 3 4 3 4 4 4 ...
 $ TotRmsAbvGrd : int  8 6 6 7 9 5 7 7 8 5 ...
 $ Functional   : Factor w/ 7 levels "Maj1","Maj2",..: 7 7 7 7 7 7 7 7 3 7 ...
 $ Fireplaces   : int  0 1 1 1 1 0 1 2 2 2 ...
 $ FireplaceQu  : Factor w/ 5 levels "Ex","Fa","Gd",..: NA 5 5 3 5 NA 3 5 5 5 ...
 $ GarageType   : Factor w/ 6 levels "2Types","Attchd",..: 2 2 2 6 2 2 2 2 6 2 ...
 $ GarageYrBlt  : int  2003 1976 2001 1998 2000 1993 2004 1973 1931 1939 ...
 $ GarageFinish : Factor w/ 3 levels "Fin","RFn","Unf": 2 2 2 3 2 3 2 2 3 2 ...
 $ GarageCars   : int  2 2 2 3 3 2 2 2 2 1 ...
 $ GarageArea   : int  548 460 608 642 836 480 636 484 468 205 ...
 $ GarageQual   : Factor w/ 5 levels "Ex","Fa","Gd",..: 5 5 5 5 5 5 5 5 2 3 ...
 $ GarageCond   : Factor w/ 5 levels "Ex","Fa","Gd",..: 5 5 5 5 5 5 5 5 5 5 ...
 $ PavedDrive   : Factor w/ 3 levels "N","P","Y": 3 3 3 3 3 3 3 3 3 3 ...
 $ WoodDeckSF   : int  0 298 0 0 192 40 255 235 90 0 ...
 $ OpenPorchSF  : int  61 0 42 35 84 30 57 204 0 4 ...
 $ EnclosedPorch: int  0 0 0 272 0 0 0 228 205 0 ...
 $ X3SsnPorch   : int  0 0 0 0 0 320 0 0 0 0 ...
 $ ScreenPorch  : int  0 0 0 0 0 0 0 0 0 0 ...
 $ PoolArea     : int  0 0 0 0 0 0 0 0 0 0 ...
 $ PoolQC       : Factor w/ 3 levels "Ex","Fa","Gd": NA NA NA NA NA NA NA NA NA NA ...
 $ Fence        : Factor w/ 4 levels "GdPrv","GdWo",..: NA NA NA NA NA 3 NA NA NA NA ...
 $ MiscFeature  : Factor w/ 4 levels "Gar2","Othr",..: NA NA NA NA NA 3 NA 3 NA NA ...
 $ MiscVal      : int  0 0 0 0 0 700 0 350 0 0 ...
 $ MoSold       : int  2 5 9 2 12 10 8 11 4 1 ...
 $ YrSold       : int  2008 2007 2008 2006 2008 2009 2007 2009 2008 2008 ...
 $ SaleType     : Factor w/ 9 levels "COD","Con","ConLD",..: 9 9 9 9 9 9 9 9 9 9 ...
 $ SaleCondition: Factor w/ 6 levels "Abnorml","AdjLand",..: 5 5 5 1 5 5 5 5 1 5 ...
 $ SalePrice    : int  208500 181500 223500 140000 250000 143000 307000 200000 129900 118000 ...
[1] "Column  Id  has  0  missing values"
[1] "Column  MSSubClass  has  0  missing values"
[1] "Column  MSZoning  has  0  missing values"
[1] "Column  LotFrontage  has  259  missing values"
[1] "Column  LotArea  has  0  missing values"
[1] "Column  Street  has  0  missing values"
[1] "Column  Alley  has  1369  missing values"
[1] "Column  LotShape  has  0  missing values"
[1] "Column  LandContour  has  0  missing values"
[1] "Column  Utilities  has  0  missing values"
[1] "Column  LotConfig  has  0  missing values"
[1] "Column  LandSlope  has  0  missing values"
[1] "Column  Neighborhood  has  0  missing values"
[1] "Column  Condition1  has  0  missing values"
[1] "Column  Condition2  has  0  missing values"
[1] "Column  BldgType  has  0  missing values"
[1] "Column  HouseStyle  has  0  missing values"
[1] "Column  OverallQual  has  0  missing values"
[1] "Column  OverallCond  has  0  missing values"
[1] "Column  YearBuilt  has  0  missing values"
[1] "Column  YearRemodAdd  has  0  missing values"
[1] "Column  RoofStyle  has  0  missing values"
[1] "Column  RoofMatl  has  0  missing values"
[1] "Column  Exterior1st  has  0  missing values"
[1] "Column  Exterior2nd  has  0  missing values"
[1] "Column  MasVnrType  has  8  missing values"
[1] "Column  MasVnrArea  has  8  missing values"
[1] "Column  ExterQual  has  0  missing values"
[1] "Column  ExterCond  has  0  missing values"
[1] "Column  Foundation  has  0  missing values"
[1] "Column  BsmtQual  has  37  missing values"
[1] "Column  BsmtCond  has  37  missing values"
[1] "Column  BsmtExposure  has  38  missing values"
[1] "Column  BsmtFinType1  has  37  missing values"
[1] "Column  BsmtFinSF1  has  0  missing values"
[1] "Column  BsmtFinType2  has  38  missing values"
[1] "Column  BsmtFinSF2  has  0  missing values"
[1] "Column  BsmtUnfSF  has  0  missing values"
[1] "Column  TotalBsmtSF  has  0  missing values"
[1] "Column  Heating  has  0  missing values"
[1] "Column  HeatingQC  has  0  missing values"
[1] "Column  CentralAir  has  0  missing values"
[1] "Column  Electrical  has  1  missing values"
[1] "Column  X1stFlrSF  has  0  missing values"
[1] "Column  X2ndFlrSF  has  0  missing values"
[1] "Column  LowQualFinSF  has  0  missing values"
[1] "Column  GrLivArea  has  0  missing values"
[1] "Column  BsmtFullBath  has  0  missing values"
[1] "Column  BsmtHalfBath  has  0  missing values"
[1] "Column  FullBath  has  0  missing values"
[1] "Column  HalfBath  has  0  missing values"
[1] "Column  BedroomAbvGr  has  0  missing values"
[1] "Column  KitchenAbvGr  has  0  missing values"
[1] "Column  KitchenQual  has  0  missing values"
[1] "Column  TotRmsAbvGrd  has  0  missing values"
[1] "Column  Functional  has  0  missing values"
[1] "Column  Fireplaces  has  0  missing values"
[1] "Column  FireplaceQu  has  690  missing values"
[1] "Column  GarageType  has  81  missing values"
[1] "Column  GarageYrBlt  has  81  missing values"
[1] "Column  GarageFinish  has  81  missing values"
[1] "Column  GarageCars  has  0  missing values"
[1] "Column  GarageArea  has  0  missing values"
[1] "Column  GarageQual  has  81  missing values"
[1] "Column  GarageCond  has  81  missing values"
[1] "Column  PavedDrive  has  0  missing values"
[1] "Column  WoodDeckSF  has  0  missing values"
[1] "Column  OpenPorchSF  has  0  missing values"
[1] "Column  EnclosedPorch  has  0  missing values"
[1] "Column  X3SsnPorch  has  0  missing values"
[1] "Column  ScreenPorch  has  0  missing values"
[1] "Column  PoolArea  has  0  missing values"
[1] "Column  PoolQC  has  1453  missing values"
[1] "Column  Fence  has  1179  missing values"
[1] "Column  MiscFeature  has  1406  missing values"
[1] "Column  MiscVal  has  0  missing values"
[1] "Column  MoSold  has  0  missing values"
[1] "Column  YrSold  has  0  missing values"
[1] "Column  SaleType  has  0  missing values"
[1] "Column  SaleCondition  has  0  missing values"
[1] "Column  SalePrice  has  0  missing values"
[1] "Dataset has overall  19  columns with missing values"
10624
13137.3706563707

 Factor w/ 3 levels "Grvl","No alley access",..: 2 2 2 2 2 2 2 2 2 2 ...
[1] "Column  Id  has  0  missing values"
[1] "Column  MSSubClass  has  0  missing values"
[1] "Column  MSZoning  has  0  missing values"
[1] "Column  LotFrontage  has  0  missing values"
[1] "Column  LotArea  has  0  missing values"
[1] "Column  Street  has  0  missing values"
[1] "Column  Alley  has  0  missing values"
[1] "Column  LotShape  has  0  missing values"
[1] "Column  LandContour  has  0  missing values"
[1] "Column  Utilities  has  0  missing values"
[1] "Column  LotConfig  has  0  missing values"
[1] "Column  LandSlope  has  0  missing values"
[1] "Column  Neighborhood  has  0  missing values"
[1] "Column  Condition1  has  0  missing values"
[1] "Column  Condition2  has  0  missing values"
[1] "Column  BldgType  has  0  missing values"
[1] "Column  HouseStyle  has  0  missing values"
[1] "Column  OverallQual  has  0  missing values"
[1] "Column  OverallCond  has  0  missing values"
[1] "Column  YearBuilt  has  0  missing values"
[1] "Column  YearRemodAdd  has  0  missing values"
[1] "Column  RoofStyle  has  0  missing values"
[1] "Column  RoofMatl  has  0  missing values"
[1] "Column  Exterior1st  has  0  missing values"
[1] "Column  Exterior2nd  has  0  missing values"
[1] "Column  MasVnrType  has  0  missing values"
[1] "Column  MasVnrArea  has  0  missing values"
[1] "Column  ExterQual  has  0  missing values"
[1] "Column  ExterCond  has  0  missing values"
[1] "Column  Foundation  has  0  missing values"
[1] "Column  BsmtQual  has  0  missing values"
[1] "Column  BsmtCond  has  0  missing values"
[1] "Column  BsmtExposure  has  0  missing values"
[1] "Column  BsmtFinType1  has  0  missing values"
[1] "Column  BsmtFinSF1  has  0  missing values"
[1] "Column  BsmtFinType2  has  0  missing values"
[1] "Column  BsmtFinSF2  has  0  missing values"
[1] "Column  BsmtUnfSF  has  0  missing values"
[1] "Column  TotalBsmtSF  has  0  missing values"
[1] "Column  Heating  has  0  missing values"
[1] "Column  HeatingQC  has  0  missing values"
[1] "Column  CentralAir  has  0  missing values"
[1] "Column  Electrical  has  0  missing values"
[1] "Column  X1stFlrSF  has  0  missing values"
[1] "Column  X2ndFlrSF  has  0  missing values"
[1] "Column  LowQualFinSF  has  0  missing values"
[1] "Column  GrLivArea  has  0  missing values"
[1] "Column  BsmtFullBath  has  0  missing values"
[1] "Column  BsmtHalfBath  has  0  missing values"
[1] "Column  FullBath  has  0  missing values"
[1] "Column  HalfBath  has  0  missing values"
[1] "Column  BedroomAbvGr  has  0  missing values"
[1] "Column  KitchenAbvGr  has  0  missing values"
[1] "Column  KitchenQual  has  0  missing values"
[1] "Column  TotRmsAbvGrd  has  0  missing values"
[1] "Column  Functional  has  0  missing values"
[1] "Column  Fireplaces  has  0  missing values"
[1] "Column  FireplaceQu  has  0  missing values"
[1] "Column  GarageType  has  0  missing values"
[1] "Column  GarageYrBlt  has  0  missing values"
[1] "Column  GarageFinish  has  0  missing values"
[1] "Column  GarageCars  has  0  missing values"
[1] "Column  GarageArea  has  0  missing values"
[1] "Column  GarageQual  has  0  missing values"
[1] "Column  GarageCond  has  0  missing values"
[1] "Column  PavedDrive  has  0  missing values"
[1] "Column  WoodDeckSF  has  0  missing values"
[1] "Column  OpenPorchSF  has  0  missing values"
[1] "Column  EnclosedPorch  has  0  missing values"
[1] "Column  X3SsnPorch  has  0  missing values"
[1] "Column  ScreenPorch  has  0  missing values"
[1] "Column  PoolArea  has  0  missing values"
[1] "Column  PoolQC  has  0  missing values"
[1] "Column  Fence  has  0  missing values"
[1] "Column  MiscFeature  has  0  missing values"
[1] "Column  MiscVal  has  0  missing values"
[1] "Column  MoSold  has  0  missing values"
[1] "Column  YrSold  has  0  missing values"
[1] "Column  SaleType  has  0  missing values"
[1] "Column  SaleCondition  has  0  missing values"
[1] "Column  SalePrice  has  0  missing values"
[1] "Dataset has overall  0  columns with missing values"

Call:
 randomForest(formula = SalePrice ~ ., data = train, importance = T) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 26

          Mean of squared residuals: 896124313
                    % Var explained: 86.13
%IncMSE	IncNodePurity
Id	-0.7309632	2.184602e+10
MSSubClass	6.6709133	1.263858e+10
MSZoning	5.9589282	7.092455e+09
LotFrontage	5.1007343	3.555114e+10
LotArea	8.7385606	9.259493e+10
Street	1.3346510	1.066220e+08
Alley	2.3601859	1.347609e+09
LotShape	2.7243514	1.182065e+10
LandContour	1.8818245	8.984289e+09
Utilities	0.0000000	3.666082e+06
LotConfig	-0.4035049	5.804652e+09
LandSlope	1.5442287	4.245871e+09
Neighborhood	25.2635490	7.479725e+11
Condition1	1.3565780	5.272166e+09
Condition2	-3.0125582	2.204454e+09
BldgType	4.9510440	4.601363e+09
HouseStyle	6.6289652	1.202963e+10
OverallQual	21.0943132	1.349167e+12
OverallCond	3.7671258	2.043394e+10
YearBuilt	10.5700174	1.611455e+11
YearRemodAdd	8.1913051	4.821761e+10
RoofStyle	2.8648900	6.090335e+09
RoofMatl	-1.3948489	9.255648e+09
Exterior1st	6.2093119	3.465482e+10
Exterior2nd	5.6385035	4.378061e+10
MasVnrType	2.7207490	6.233998e+09
MasVnrArea	5.2158590	5.905355e+10
ExterQual	9.9713750	4.341951e+11
ExterCond	1.8874099	3.534937e+09
Foundation	2.8325798	8.111024e+09
⋮	⋮	⋮
HalfBath	3.2065183	7644177400
BedroomAbvGr	3.8781551	13464500213
KitchenAbvGr	4.9529546	2568002213
KitchenQual	7.4286626	141158410764
TotRmsAbvGrd	9.2680714	80908102292
Functional	-0.3683461	4193104395
Fireplaces	5.3787648	19539192309
FireplaceQu	8.1039744	57074566020
GarageType	8.8232021	30515566537
GarageYrBlt	8.3984078	61281802862
GarageFinish	8.2504159	33447423883
GarageCars	12.5576331	602033492431
GarageArea	11.3708029	206074529361
GarageQual	3.4532558	5468578745
GarageCond	3.5932220	3243964228
PavedDrive	3.7819269	3390118356
WoodDeckSF	3.6796454	26272516807
OpenPorchSF	4.4330186	28216704537
EnclosedPorch	2.0444983	4024608709
X3SsnPorch	-1.1402059	504390104
ScreenPorch	-0.9195965	11011309928
PoolArea	0.0000000	22061742
PoolQC	0.0000000	1562752
Fence	1.2291826	2852741728
MiscFeature	-0.9874915	326350563
MiscVal	-0.5657842	568623655
MoSold	-0.2091290	29529069769
YrSold	-0.5029634	11045329590
SaleType	2.6592441	5782285073
SaleCondition	2.4688015	9844837351
[1] "Plot of variable importance"
[1] "Variable importance of initial model"

181962.382
[1] "Inital prediction"




Call:
 randomForest(formula = SalePrice ~ GrLivArea + GarageArea + GarageCars +      X2ndFlrSF + LotArea + FullBath + YearBuilt + ExterQual +      FireplaceQu + KitchenQual + GarageFinish + BsmtQual + GarageType +      totalrooms + OpenPorchSF + YearRemodAdd + MSZoning + Exterior1st +      HouseStyle + Fireplaces + BsmtUnfSF + WoodDeckSF + GarageYrBlt +      GarageQual + CentralAir, data = train, importance = T) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 8

          Mean of squared residuals: 1095919059
                    % Var explained: 83.04
%IncMSE	IncNodePurity
GrLivArea	34.679265	1.152000e+12
GarageArea	19.326356	4.253803e+11
GarageCars	17.704740	9.908616e+11
X2ndFlrSF	10.837445	1.408464e+11
LotArea	15.109135	2.511752e+11
FullBath	10.844010	1.053959e+11
YearBuilt	14.442687	4.048395e+11
ExterQual	14.284550	8.886066e+11
FireplaceQu	13.341438	1.412777e+11
KitchenQual	12.899158	3.643649e+11
GarageFinish	11.301642	8.571668e+10
BsmtQual	12.441538	2.903546e+11
GarageType	12.089201	6.862555e+10
totalrooms	12.163809	2.338203e+11
OpenPorchSF	5.702912	6.869864e+10
YearRemodAdd	11.649551	1.299112e+11
MSZoning	17.356975	3.089281e+10
Exterior1st	12.214907	1.235212e+11
HouseStyle	11.669511	3.552158e+10
Fireplaces	8.183869	6.920866e+10
BsmtUnfSF	7.970018	8.606952e+10
WoodDeckSF	3.920225	7.218529e+10
GarageYrBlt	8.519769	1.466352e+11
GarageQual	7.112760	1.429842e+10
CentralAir	10.595131	1.759602e+10
181962.382

[1] "Variable importance after second iteration"

[1] "Barplots for the categorical features"







[1] "Correlation plot for the continous features"

Call:
 randomForest(formula = SalePrice ~ OverallQual + YearBuilt +      YearRemodAdd + MasVnrArea + BsmtFinSF1 + TotalBsmtSF + X1stFlrSF +      GrLivArea + totalrooms + Fireplaces + GarageArea + GarageCars +      MSZoning + HouseStyle + ExterQual + KitchenQual + GarageFinish +      BsmtQual + GarageType + CentralAir, data = train, importance = T) 
               Type of random forest: regression
                     Number of trees: 500
No. of variables tried at each split: 6

          Mean of squared residuals: 978734205
                    % Var explained: 84.86
%IncMSE	IncNodePurity
OverallQual	15.030449	1.550346e+12
YearBuilt	15.536163	3.076711e+11
YearRemodAdd	10.172117	1.134074e+11
MasVnrArea	6.318911	1.268060e+11
BsmtFinSF1	12.989945	2.011826e+11
TotalBsmtSF	15.704295	3.446242e+11
X1stFlrSF	18.615446	3.327211e+11
GrLivArea	36.937640	8.699635e+11
totalrooms	14.178327	2.969870e+11
Fireplaces	11.924941	6.976621e+10
GarageArea	14.516065	3.338646e+11
GarageCars	10.837635	6.882478e+11
MSZoning	14.282908	3.006090e+10
HouseStyle	9.970663	3.609271e+10
ExterQual	11.901334	6.185400e+11
KitchenQual	9.683159	1.997657e+11
GarageFinish	12.446006	6.773031e+10
BsmtQual	12.062088	1.516445e+11
GarageType	11.742864	6.423473e+10
CentralAir	6.571424	1.530330e+10
181962.382

[1] "Variable importance after second iteration"


# Plotting SSE
    plot(c(1:2),c(second_SSE,third_SSE),
     xlab="iteration",ylab="SSE")
