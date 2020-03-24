#  .............................CAB PREDICTIOn....................................  #
rm(list=ls(all=T))
# setting up working directory for project cab prediction 
setwd("R:/vishakha r progaram/projects")
getwd()
#loading required libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
#installing packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
#.........................loding data for prdiction..................................#

train = read.csv("train_cab.csv", header = T)[,-2] #skipping pickup data

#checking structure of  given data
str(train)


#.........................Expolratory data analys.......................................#
#unique value of each count
apply(train, 2,function(x) length(table(x)))

#convert the data with required data type
train$fare_amount=as.numeric(as.character(train$fare_amount))
train$passenger_count=as.integer(train$passenger_count)


# Eliminate cells with same pickup and dropoff location
train=subset(train, !(train$pickup_longitude==train$dropoff_longitude & train$pickup_latitude==train$dropoff_latitude))
#replace "0's" with NA
train[train==0]= NA

cab_1= train

#.....................data pre processing........................................#
#missing value analysis
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
#convert row name into column
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
# rename the column to missing percentage
names(missing_val)[1] =  "Missing_percentage"
# calculate the missing percentage 
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train)) * 100
# arrange the in decending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
#rearrange the column 
missing_val = missing_val[,c(2,1)]
#saving mising value back to the disk
write.csv(missing_val, "Miising_perc.csv", row.names = F)
#analysis with gragh
ggplot(data = missing_val, aes(x=reorder(Columns,-Missing_percentage*100),y =  Missing_percentage))+
  geom_bar(stat = "identity",fill = "blue")+xlab("Variables")+
  ggtitle("Missing data percentage (train)") + theme_bw()


# here we have analyse that all the value of missing percentage is less than 30% so lest have impute 
#As PAssenger_count is a categorical Variable , so we will use mode for Imputation
#calculate mode - create function
mode= function(data){
  uniq=unique(data)
  as.numeric(as.character(uniq[which.max(tabulate(match(data,uniq)))]))
  #print(mode_d)
}
mode(train$passenger_count)

#impute with the mode
train$passenger_count[is.na(train$passenger_count)] = mode(train$passenger_count)
sum(is.na(train$passenger_count))
#expriment to choees exact method
# atual value = 17.5 
# mean =  15.15801
#median = 8.5
#kNN = 15.90051

#Mean method
 #train$fare_amount[is.na(train$fare_amount)] = mean(train$fare_amount, na.rm = T)
 
#Median Method 
 #train$fare_amount[is.na(train$fare_amount)] = median(train$fare_amount, na.rm = T)
 
# KNN method 
 train= knnImputation(train, k = 5)
sum(is.na(train))

#lest have copy of data

cab_2= train

#.........................Outlier Analysis...................................#
#outliers in fare_amount
#Remove negative values from 'fare_amount'
train$fare_amount=ifelse(train$fare_amount<0, NA, train$fare_amount)
train$fare_amount=ifelse(train$fare_amount>30,NA, train$fare_amount)


#all values greater than 8 are converted to NA
unique(train$passenger_count)
#Convert more then 8 to NA
for (i in 1:nrow(train)){
  if (as.integer(train$passenger_count[i]) > 8){
    train$passenger_count[i]=NA
  }
}


cnames=colnames(train[,c(2:5)])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount"), data = train)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="y")+
           ggtitle(paste("Box plot of fare amount",cnames[i])))
}

# Plotting plots together
gridExtra::grid.arrange(gn1, gn2, ncol=2)
gridExtra::grid.arrange(gn3, gn4, ncol=2)

#Replace all outliers with NA and impute
#create NA on outliers
for(i in cnames){
  val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  print(length(val))
  train[,i][train[,i] %in% val] = NA
}

#replace missing value with mode
mode(train$passenger_count)
train$passenger_count[is.na(train$passenger_count)] = mode(train$passenger_count)
train=train[complete.cases(train[,1]), ]

#replace all other missing value with mean
train$fare_amount[is.na(train$fare_amount)] = mean(train$fare_amount, na.rm=T)
train$pickup_longitude[is.na(train$pickup_longitude)] = mean(train$pickup_longitude, na.rm=T)
train$pickup_latitude[is.na(train$pickup_latitude)] = mean(train$pickup_latitude, na.rm=T)
train$dropoff_longitude[is.na(train$dropoff_longitude)] = mean(train$dropoff_longitude, na.rm=T)
train$dropoff_latitude[is.na(train$dropoff_latitude)] = mean(train$dropoff_latitude, na.rm=T)

sum(is.na(train))

#now convert Passenger_count into factor
unique(train$passenger_count)
train$passenger_count=as.factor(train$passenger_count)
#copy data                                
cab_3 = train                                
#..................................feature engineering.................................#
#by using haversine formula we have  creating new variable distance 
#calculate the distance by using lattitude and longitude
deg_to_rad = function(deg){
  (deg * pi) / 180

}
haversine = function(long1,lat1,long2,lat2){
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371 # radius of earth
  (R * c)
}

#Using haversine formula to calculate distance fr both train and test
train$dist = haversine(train$pickup_longitude,train$pickup_latitude,train$dropoff_longitude,train$dropoff_latitude)


#copy data
cab_4 = train

#.........................correlation/multicolinearity..............................#

#correlation
corrgram(train[,-6], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


# checking for multicolinearity
#check multicollearity
library(usdm)
vif(train[,-c(1,6)])

vifcor(train[,-c(1,6)], th = 0.9)
#.............................checking disrtibution of data........................#

hist(train$dist)

hist(train$fare_amount)

#No variable from the 5 input variables has collinearity problem.
rm(cab_1,cab_2,cab_3,cab_4,gn1,gn2,gn3,gn4,cnames,i,val,missing_val)

#.....................................model development.............................#
#create sampling and divide data into train and test

set.seed(123)
train_index = sample(1:nrow(train), 0.8 * nrow(train))

train1 = train[train_index,]
test1 = train[-train_index,]
#..........................decesion tree.............................................#
# Define Mape - The error matrix to calculate the error and accuracy
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y*100))
}
#decision tree
fit = rpart(fare_amount ~. , data = train1, method = "anova", minsplit=5)

summary(fit)
predictions_DT = predict(fit, test1[,-1])

MAPE(test1[,1], predictions_DT)

write.csv(predictions_DT, "DT_Data.csv", row.names = F)

# accuracy = 71.96569
# error = 28.03431
#....................................Random forest..............#

RF_model = randomForest(fare_amount ~.  , train1, importance = TRUE, ntree=100)

RF_Predictions = predict(RF_model, test1[,-1])

write.csv(RF_Predictions, "DF_Data.csv", row.names = F)

MAPE(test1[,1], RF_Predictions)
importance(RF_model, type = 1)

#accuracy = 77.88859
#Error = 22.11141
#...........................Linear regression model......................#

lm_model = lm(fare_amount ~. , data = train1)

summary(lm_model)

predictions_LR = predict(lm_model, test1[,-1])

write.csv(predictions_LR, "LR_Data.csv", row.names = F)

MAPE(test1[,1], predictions_LR)

#accuracy = 73.84562
#error = 26.15438

#............................KNN Implementation......................................#

library(class)

KNN_Predictions = knn(train1[, 2:7], test1[, 2:7], train1$fare_amount, k = 1)

#convert the values into numeric
KNN_Predictions=as.numeric(as.character((KNN_Predictions)))

#Calculate MAPE
MAPE(test1[,1], KNN_Predictions)

write.csv(KNN_Predictions, "KNN1_Data.csv", row.names = F)

#accuracy =  65.62963  
# error = 34.37037 ,

#.............................Predict VAlues in Test Data...............................#
#read test data...........
test_c=read.csv("test.csv", header= T)[,-1]

#create distance variable
test_c=subset(test_c, !(test_c$pickup_longitude==test_c$dropoff_longitude & test_c$pickup_latitude==test_c$dropoff_latitude))
test_c[test_c==0]= NA

str(test_c)
# convert data in require format for test_c
test_c$passenger_count=as.factor(test_c$passenger_count)

#creating distance variable
test_c$dist = haversine(test_c$pickup_longitude,test_c$pickup_latitude,test_c$dropoff_longitude,test_c$dropoff_latitude)

#create the variable for maching to train data orginally
test_c$fare_amount=0
test_c=test_c[,c(1,2,3,4,5,6,7)]
 
#Random Forest
RF_model = randomForest(fare_amount ~.  , train, importance = TRUE, ntree=200, mtry=2)

test_c$fare_amount = predict(RF_model, test_c[,0:6])


write.csv(test_c, "Cabprediction_Data.csv", row.names = F)

