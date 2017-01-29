
####################################################################################################
#################################Data pre-processing################################################
####################################################################################################

data<- read.csv("AcademyBestPictureNominationDataset.csv", header = TRUE)
summary(data_)#summary statistics
str(data_) #shows data type of variables
dim(data_)#shows how big the data is
head(data_)#shows what data looks like

NA_count <-function (x,MAR=2,testing_mode=F){
  
  partial<-apply(data,MAR=MAR,FUN=function( x ){sum(is.na( x ))})
  if (testing_mode==F){
    partial<-partial[partial>0]
  }
  total<- sum(partial)
  sparsity<-total/prod(dim(data))
  output<-list(partial, total, sparsity)
  names(output)<-c("per selected dimension","total","sparsity ")
  return(output)
}

NA_count(data) ###Check, where are the NAs, how many there are, and what % overall are they of the population

data<- data[complete.cases(data),]####Remove NAs


#####Get substrings for Date
library(stringr)

Day<- as.numeric(sub("/.*", "", data$Release.Date..month.day.year.))
Month <- as.numeric(str_sub(data$Release.Date..month.day.year., start = 4, end = 5))
data2<-cbind(data,Day,Month)

############Extracting Genre(s)

extractGenre <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("comedy", Genres)) >0) {
    return("1")
  } else if(length(grep("comedy,", Genres))>0) {return("1")}
  else if(length(grep(",comedy,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Comedy<-NULL

for (i in 1:nrow(data2)){
  Genre_Comedy<-c(Genre_Comedy, extractGenre(data[i,"Genres"]))}

Genre_Comedy

###########
extractGenre2 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("drama", Genres)) >0) {
    return("1")
  } else if(length(grep("drama,", Genres))>0) {return("1")}
  else if(length(grep(",drama,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Drama<-NULL

for (i in 1:nrow(data2)){
  Genre_Drama<-c(Genre_Drama, extractGenre2(data[i,"Genres"]))}

Genre_Drama
########

extractGenre3 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("adventure", Genres)) >0) {
    return("1")
  } else if(length(grep("adventure,", Genres))>0) {return("1")}
  else if(length(grep(",adventure,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Adventure<-NULL

for (i in 1:nrow(data2)){
  Genre_Adventure<-c(Genre_Adventure, extractGenre3(data[i,"Genres"]))}

Genre_Adventure

#######

extractGenre4 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("crime", Genres)) >0) {
    return("1")
  } else if(length(grep("crime,", Genres))>0) {return("1")}
  else if(length(grep(",crime,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Crime<-NULL

for (i in 1:nrow(data2)){
  Genre_Crime<-c(Genre_Crime, extractGenre4(data[i,"Genres"]))}

Genre_Crime

#####

extractGenre5 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("western", Genres)) >0) {
    return("1")
  } else if(length(grep("western,", Genres))>0) {return("1")}
  else if(length(grep(",western,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Western<-NULL

for (i in 1:nrow(data2)){
  Genre_Western<-c(Genre_Western, extractGenre5(data[i,"Genres"]))}

Genre_Western

#####
extractGenre6 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("sci_fi", Genres)) >0) {
    return("1")
  } else if(length(grep("sci_fi,", Genres))>0) {return("1")}
  else if(length(grep(",sci_fi,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Sci_Fi<-NULL

for (i in 1:nrow(data2)){
  Genre_Sci_Fi<-c(Genre_Sci_Fi, extractGenre6(data[i,"Genres"]))}

Genre_Sci_Fi

######
extractGenre7 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("horror", Genres)) >0) {
    return("1")
  } else if(length(grep("horror,", Genres))>0) {return("1")}
  else if(length(grep(",horror,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Horror<-NULL

for (i in 1:nrow(data2)){
  Genre_Horror<-c(Genre_Horror, extractGenre7(data[i,"Genres"]))}

Genre_Horror

#####

extractGenre8 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("thriller", Genres)) >0) {
    return("1")
  } else if(length(grep("thriller,", Genres))>0) {return("1")}
  else if(length(grep(",thriller,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Thriller<-NULL

for (i in 1:nrow(data2)){
  Genre_Thriller<-c(Genre_Thriller, extractGenre8(data[i,"Genres"]))}

Genre_Thriller

#####

extractGenre9 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("family", Genres)) >0) {
    return("1")
  } else if(length(grep("family,", Genres))>0) {return("1")}
  else if(length(grep(",family,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Family<-NULL

for (i in 1:nrow(data2)){
  Genre_Family<-c(Genre_Family, extractGenre9(data[i,"Genres"]))}

Genre_Family

######
extractGenre10 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("fantasy", Genres)) >0) {
    return("1")
  } else if(length(grep("fantasy,", Genres))>0) {return("1")}
  else if(length(grep(",fantasy,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Fantasy<-NULL

for (i in 1:nrow(data2)){
  Genre_Fantasy<-c(Genre_Fantasy, extractGenre10(data[i,"Genres"]))}

Genre_Fantasy

#####

extractGenre11 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("war", Genres)) >0) {
    return("1")
  } else if(length(grep("war,", Genres))>0) {return("1")}
  else if(length(grep(",war,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_War<-NULL

for (i in 1:nrow(data2)){
  Genre_War<-c(Genre_War, extractGenre11(data[i,"Genres"]))}

Genre_War

#####

extractGenre12 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("romance", Genres)) >0) {
    return("1")
  } else if(length(grep("romance,", Genres))>0) {return("1")}
  else if(length(grep(",romance,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Romance<-NULL

for (i in 1:nrow(data2)){
  Genre_Romance<-c(Genre_Romance, extractGenre12(data[i,"Genres"]))}

Genre_Romance

######

extractGenre13 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("animation", Genres)) >0) {
    return("1")
  } else if(length(grep("animation,", Genres))>0) {return("1")}
  else if(length(grep(",animation,", Genres))>0) {return("1")} 
  else{ return("0")}
}

Genre_Animation<-NULL

for (i in 1:nrow(data2)){
  Genre_Animation<-c(Genre_Animation, extractGenre13(data[i,"Genres"]))}

Genre_Animation

####
extractGenre14 <- function(Genres){
  Genres <- as.character(Genres)
  
  if (length(grep("biography", Genres)) >0) {
    return(1)
  } else if(length(grep("biography,", Genres))>0) {return("1")}
  else if(length(grep(",biography,", Genres))>0) {return("1")} 
  else{ return(0)}
}

Genre_Biography<-NULL

for (i in 1:nrow(data2)){
  Genre_Biography<-c(Genre_Biography, extractGenre14(data[i,"Genres"]))}



data.combined<-cbind(data2,Genre_Biography,Genre_Animation, Genre_Romance, Genre_War,Genre_Fantasy, Genre_Crime, Genre_Family, Genre_Thriller,Genre_Horror, Genre_Sci_Fi, Genre_Comedy, Genre_Western,Genre_Adventure, Genre_Drama)

#####Turn from a factor to numeric
library(varhandle)
data.combined$Genre_Biography <- unfactor(data.combined$Genre_Biography)
data.combined$Genre_Animation <- unfactor(data.combined$Genre_Animation)
data.combined$Genre_Romance <- unfactor(data.combined$Genre_Romance)
data.combined$Genre_War <- unfactor(data.combined$Genre_War)
data.combined$Genre_Fantasy <- unfactor(data.combined$Genre_Fantasy)
data.combined$Genre_Crime<- unfactor(data.combined$Genre_Crime)
data.combined$Genre_Family <- unfactor(data.combined$Genre_Family)
data.combined$Genre_Thriller <- unfactor(data.combined$Genre_Thriller)
data.combined$Genre_Horror <- unfactor(data.combined$Genre_Horror)
data.combined$Genre_Sci_Fi <- unfactor(data.combined$Genre_Sci_Fi)
data.combined$Genre_Comedy <- unfactor(data.combined$Genre_Comedy)
data.combined$Genre_Western <- unfactor(data.combined$Genre_Western)
data.combined$Genre_Adventure <- unfactor(data.combined$Genre_Adventure)
data.combined$Genre_Drama <- unfactor(data.combined$Genre_Drama)

str(data.combined)##Check if all are now numeric

###########Renaming columns
names(data.combined)[names(data.combined)=="Genre_War"] <- "War"
names(data.combined)[names(data.combined)=="Nominated.AA"] <- "AA"
names(data.combined)[names(data.combined)=="Nominated.WGA.BAS"] <- "WGA.BAS"
names(data.combined)[names(data.combined)=="Nominated.WGA.BOS"] <- "WGA.BOS"
names(data.combined)[names(data.combined)=="Nominated.Baftas.BD"] <- "B.BD"
names(data.combined)[names(data.combined)=="Nominated.CC.BD"] <- "CC.BD"
names(data.combined)[names(data.combined)=="Nominated.GG.BActress.Supp"] <- "GG.BATS.S"
names(data.combined)[names(data.combined)=="Nominated.GG.BActor.Supp"] <- "GG.BA.S"
names(data.combined)[names(data.combined)=="Nominated.GG.BActress.Comedy.Musical"] <- "GG.BATS.CM"
names(data.combined)[names(data.combined)=="Nominated.GG.BActress.Drama"] <- "GG.BATS.D"
names(data.combined)[names(data.combined)=="Nominated.GG.BActor.Comedy.Musical"] <- "GG.BA.CM"
names(data.combined)[names(data.combined)=="Nominated.GG.BActor.Drama"] <- "GG.BA.D"
names(data.combined)[names(data.combined)=="Nominated.SAG.BActress.Supp"] <- "SAG.BATS.S"
names(data.combined)[names(data.combined)=="Nominated.SAG.BActress"] <- "SAG.BATS"
names(data.combined)[names(data.combined)=="Nominated.SAG.BA"] <- "SAG.BA"
names(data.combined)[names(data.combined)=="Nominated.SAG.BActor.Supp"] <- "SAG.BA.S"
names(data.combined)[names(data.combined)=="Nominated.GG.SP"] <- "GG.SP"
names(data.combined)[names(data.combined)=="Nominated.GG.BD"] <- "GG.BD"
names(data.combined)[names(data.combined)=="Nominated.GG.BP.Drama"] <- "GG.BP.D"
names(data.combined)[names(data.combined)=="Nominated.GG.BP.Comedy.Musical"] <- "GG.BP.CM"
names(data.combined)[names(data.combined)=="Nominated.Baftas.BP"] <- "B.BP"
names(data.combined)[names(data.combined)=="Nominated.CC.BP"] <- "CC.BP"
names(data.combined)[names(data.combined)=="Nominated.DGA"] <- "DGA"
names(data.combined)[names(data.combined)=="Nominated.SAG.Best.Ensemble"] <- "SAG.B.E"
names(data.combined)[names(data.combined)=="Nominated.PGA.BP"] <- "PGA.BP"
names(data.combined)[names(data.combined)=="IMDb.Rating"] <- "IMDb.R"
names(data.combined)[names(data.combined)=="Runtime..mins."] <- "RT.M"
names(data.combined)[names(data.combined)=="Num..Votes.IMDb."] <- "V.IMDb"
names(data.combined)[names(data.combined)=="Release.Date..month.day.year."] <- "Rel.D.M"
names(data.combined)[names(data.combined)=="Genre_Biography"] <- "Bio"
names(data.combined)[names(data.combined)=="Genre_Animation"] <- "Ani"
names(data.combined)[names(data.combined)=="Genre_Romance"] <- "Rom"
names(data.combined)[names(data.combined)=="Genre_Fantasy"] <- "Fan"
names(data.combined)[names(data.combined)=="Genre_Crime"] <- "Crim"
names(data.combined)[names(data.combined)=="Genre_Family"] <- "Fam"
names(data.combined)[names(data.combined)=="Genre_Thriller"] <- "Thril"
names(data.combined)[names(data.combined)=="Genre_Horror"] <- "Hor"
names(data.combined)[names(data.combined)=="Genre_Sci_Fi"] <- "Sci_F"
names(data.combined)[names(data.combined)=="Genre_Comedy"] <- "Com"
names(data.combined)[names(data.combined)=="Genre_Western"] <- "Wes"
names(data.combined)[names(data.combined)=="Genre_Adventure"] <- "Adv"
names(data.combined)[names(data.combined)=="Genre_Drama"] <- "Dram"


###Remove unnecessary columns
data.combined$Genres<- NULL
data.combined$Rel.D.M<- NULL


table(data.combined$AA)######important to note for class imbalance, this means we need a different model measurement instead of accuracy


###Output of data preprocessing
write.csv(data.combined, "DataProcessedOscars.csv")


###################################################################################################################
###########################################Modelling###############################################################
###################################################################################################################


data.combined<- read.csv("DataProcessedOscars.csv", header = TRUE)
data.combined$X<-NULL ##unnecessary variable that was in the file

#######Random Forests is the chosen alorgorithm

library(ggplot2)
library(caret)


#Create random samples of the dataset and split into training and test sets
inTraining <- createDataPartition(data.combined$AA, p = .75, list = FALSE) #default stratified
traindata <- data.combined[inTraining,] #training set
testdata  <- data.combined[-inTraining,] #testing set
row.names(traindata)<-NULL
row.names(testdata)<-NULL

#10-fold cross validation, 10 times 
cvControl = trainControl(method="repeatedcv",number=10,repeats=10, savePredictions = TRUE)

#Train the model
modelRF <- train(as.factor(AA)~.-Film-Year, data = traindata, method = "rf",
                 trControl = cvControl,na.action = na.omit,keep.forest=TRUE)

predictionsProb <-predict(modelRF,testdata, type="prob") ###the probability for each prediction

predictions <- predict(modelRF, testdata)
actual <- (testdata$AA)

confusionMatrix(predictions,actual) #confusion matrix

DataForPredict <- names(testdata) %in% c("Year", "Film", "AA") 
testdatapredict <- testdata[DataForPredict]
TestDataPredictions<- cbind(testdatapredict, predictionsProb, predictions) ###the predictions, probability; put together with film names, and year


library(MLmetrics)

F1_Score(y_true = actual, y_pred =predictions, positive = NULL) #F1 score
Precision(y_true = actual, y_pred = predictions, positive = NULL) #Precision
Recall(y_true = actual, y_pred = predictions, positive = NULL) #Recall

################################################################################################
###############################Feature Selection################################################
################################################################################################

control <- rfeControl(functions=rfFuncs, method="cv", number=10)

rfe.train <- rfe(traindata[,4:44], as.factor(traindata[,3]), rfeControl=control)

rfe.train

predictors(rfe.train)###what are the top predictors

plot(rfe.train, type=c("g", "o"), cex = 1.0)####Accuracy plot for how many predictors

data.combined_<- read.csv("DataTop8Predictors.csv", header = TRUE)###Dataset with the optimal number of predictors
data.combined_$X<-NULL ##unnecessary variable that was in the file
data.combined_$Year<-NULL
data.combined_$Film<-NULL

################################################################################################
###############################Optimisation of model############################################
################################################################################################

#Create random samples of the dataset and split into training and test sets
inTraining2 <- createDataPartition(data.combined_$AA, p = .75, list = FALSE) #default stratified
traindata2 <- data.combined_[inTraining2,] #training set
testdata2  <- data.combined_[-inTraining2,] #testing set
row.names(traindata2)<-NULL
row.names(testdata2)<-NULL


#10-fold cross validation, 10 times 
cvControl = trainControl(method="repeatedcv",number=10,repeats=10, savePredictions = TRUE)

#Train the model
modelRFOp <- train(as.factor(AA)~., data = traindata2, method = "rf",
                   trControl = cvControl,na.action = na.omit,keep.forest=TRUE)

predictionsProb_ <-predict(modelRFOp,testdata2, type="prob") ###the probability for each prediction

predictions_<- predict(modelRFOp, testdata2)
actual_ <- (testdata2$AA)

confusionMatrix2<-confusionMatrix(predictions_,actual_) #confusion matrix
confusionMatrix2

DataForPredict2 <- names(testdata2) %in% c("Year", "Film", "AA") 
testdatapredict2 <- testdata2[DataForPredict2]
TestDataPredictions2<- cbind(testdatapredict2, predictionsProb_, predictions_) ###the predictions, probability; put together with film names, and year

########################################################################################################################
################2016 (89th Academy Awards) Reminder List of Eligible Releases###########################################
########################################################################################################################

reminder2016_2<- read.csv("Reminder List of Eligible Releases Dataset 2017.csv", header = TRUE)###Dataset with the optimal number of predictors
reminder2016$Year<-NULL ##unnecessary variable that was in the file
reminder2016$Film<-NULL ##unnecessary variable that was in the file
predictionsProb_2 <-predict(modelRFOp,reminder2016, type="prob") ###the probability for each prediction
predictionsProb_3 <-predict(modelRFOp,reminder2016) 

predictions_<- predict(modelRFOp, testdata2)
actual_ <- (testdata2$AA)

total<- cbind(predictionsProb_2,reminder2017_2)
Scores_2016<-cbind(total,predictionsProb_3 )

write.csv(Scores_2016, "Nominations89thAcademyAwards.csv")