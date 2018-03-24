library(corrplot)
library(MASS)


PLData<-read.csv(file.choose())
PLDataOrig<-PLData
str(PLData)
head(PLData)
tail(PLData)
attach(PLData)



#Cleaning Data
#Removing NA values 
PLData<-na.omit(PLData)
summary(PLData)

NonFactMatrix<-PLData[,-c(1:7,12:15,17:18)]
str(NonFactMatrix)
Cormatrix<-cor(NonFactMatrix)

corrplot(cor(NonFactMatrix))
plot(NonFactMatrix$Grouped.Team.Rating,NonFactMatrix$Fixture.Rating)
?abline()

#Removing the correlated factors
print(Cormatrix, digits = 2,sort=TRUE)
str(NonFactMatrix)
TransformedPLData<-NonFactMatrix[,c(1,2,10,11,12:16)]

#Running a Test Linear Regression
model1<-lm(TransformedPLData$Fixture.Rating~.,TransformedPLData)
summary(model1)
hist(TransformedPLData$Fixture.Rating)


#Storing Man City data set into another Dataframe
ManCityData<-PLData[Team.Name=="Man City",]

#Storing Fixture Rating in a vector describing form of the team
form<-PLData$Fixture.Rating[PLData$Team.Name=="Man City"]
form
length(form)
for(i in seq(1:5)){print(form[length-i])}

#Testing if last 2 games form can be iterated 
a=seq(1:100)
a
for(i in seq(1:length(form))) { print(form[i-1]+form[i-2])}


#Storing values of two games form
Twogamesform<-c(0,0)
typeof(Twogamesform)
#Last 2 games form
for(i in 3:length(form)){ 
  Twogamesform[i]=form[i-1]+form[i-2]
}
length(Twogamesform)

#storing Values of three games form
Threegamesform<-c(0,0,0)
for(i in 4:length(form)){ 
  Threegamesform[i]=form[i-1]+form[i-2]+form[i-3]
}
length(Threegamesform)

##storing Values of four games form
Fourgamesform<-c(0,0,0,0)
for(i in 5:length(form)){ 
  Fourgamesform[i]=form[i-1]+form[i-2]+form[i-3]+form[i-4]
}
length(Fourgamesform)

##storing Values of five games form
Fivegamesform<-c(0,0,0,0,0)
for(i in 6:length(form)){ 
  Fivegamesform[i]=form[i-1]+form[i-2]+form[i-3]+form[i-4]+form[i-5]
}
length(Fivegamesform)

#storing form Values of the previous games
prevgameform<-c(0)
for(i in 2:length(form)){ 
  prevgameform[i]=form[i-1]
}
length(prevgameform)

summary(ManCityData)
MainMCData<-ManCityData[,-c(1:7,12:15,17:18,25)]
str(MainMCData)

#adding the forms columns to the main dataframe
MainMCData<-cbind(MainMCData,"currentform"=prevgameform,"twogameform"=Twogamesform,"threegameform"=Threegamesform,"fourgameform"=Fourgamesform,"fivegameform"=Fivegamesform)
str(MainMCData)
attach(MainMCData)
cor(MainMCData)
corrplot(cor(MainMCData))

#Test model2
model2<-lm(Fixture.Rating~.,MainMCData)
summary(model2)

hist(Fixture.Rating)

#Divide into training and test dataset
set.seed(555)
ind <- sample(2, nrow(MainMCData),
              replace = TRUE,
              prob = c(0.7, 0.3))
training<-MainMCData[ind==1,]
testing<-MainMCData[ind==2,]

#Logistic REgression
install.packages("nnet")
library(nnet)
model3 <- multinom(MainTeamW_LBin~+Against.Team.Rating+Home.Team.Rating+Away.Team.Rating+currentform+twogameform+threegameform+fourgameform+fivegameform,training)
summary(model3)

predicted_class<-predict(model3,testing)
table(predicted_class,testing$MainTeamW_LBin)

#Linear discriminant analysis
model4<-lda(MainTeamW_LBin~.+Against.Team.Rating+Home.Team.Rating+Away.Team.Rating+currentform+twogameform+threegameform+fourgameform+fivegameform,training)
corrplot(cor(training))
summary(model4)

#Linear discriminant analysis trial 2
model5<-lda(MainTeamW_LBin~.+Against.Team.Rating+Home.Team.Rating+Away.Team.Rating+currentform,training)
summary(model5)

#since there is collinearity in data remove the collinear variables in the data
str(MainMCData)
ManCityDataLDA<-MainMCData[,c(5,12:14,16:20)]
str(ManCityDataLDA)
corrplot(cor(ManCityDataLDA))

#Dividing training and testing
set.seed(444)
ind <- sample(2, nrow(ManCityDataLDA),
              replace = TRUE,
              prob = c(0.7, 0.3))
trainingLDA<-ManCityDataLDA[ind==1,]
testingLDA<-ManCityDataLDA[ind==2,]
model5<-lda(MainTeamW_LBin~.,trainingLDA)
summary(model5)
model5
p <- predict(model5, testingLDA)
p

p2 <- predict(model5, testingLDA)
p2$class
table(predicted=p2$class,actual=testingLDA$MainTeamW_LBin)

#Retrying Multinom
model6 <- multinom(trainingLDA$MainTeamW_LBin~.,trainingLDA)
summary(model6)

#on training
predicted_class<-predict(model6,trainingLDA)
table(predicted_class,trainingLDA$MainTeamW_LBin)
#on testing
predicted_class<-predict(model6,testingLDA)
table(predicted_class,testingLDA$MainTeamW_LBin)


tail(trainingLDA)
tail(testingLDA)
