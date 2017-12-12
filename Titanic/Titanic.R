#Load the training set
train = read.csv(choose.files())
str(train)

#Load the testing set
test = read.csv(choose.files())
str(test)
test$Survived = NA

#Create New variable for train & test
train$Istrain = TRUE
test$Istrain = FALSE

#Combine train and test
full = rbind(train, test)
str(full)
summary(full)

#Convert datatype for certain variables
full$Name = as.character(full$Name)
full$Ticket = as.character(full$Ticket)
full$Cabin = as.character(full$Cabin)
full$Embarked = as.character(full$Embarked)
str(full)
summary(full)

#Missing values
#Survived - 418(Missing Values in test data)
#Age - 263 NA's
#Fare - 1 NA's
#Embarked - 2

#Replace Fare NA's with median value
median(full$Fare, na.rm = TRUE)
full[is.na(full$Fare), "Fare"] = median(full$Fare, na.rm = TRUE)

#Replace Embarked with max occurance 
full[full$Embarked == '', "Embarked"] = "S"
table(full$Embarked)

#Split the Name and create new variable "Title"
head(full$Name, 10)
full$Title = gsub('(.*, )|(\\..*)', '', full$Name)
full$Title
str(full$Title)
table(full$Sex, full$Title)

#Combine rare titles into one
full$Title = as.character(full$Title)

officer = c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty = c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')

full$Title[full$Title == 'Mlle'] = 'Miss' 
full$Title[full$Title == 'Ms'] = 'Miss'
full$Title[full$Title == 'Mme'] = 'Mrs' 
full$Title[full$Title %in% royalty] = 'Royalty'
full$Title[full$Title %in% officer] = 'Officer'

#Create new variable Surname from Name
full$Surname = sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

#Create Family size by joining Sibling spouse(SibSp), Parent Children (Parch) and 1 
full$Fsize = full$SibSp + full$Parch + 1

#Create new variable Family SizeD based on Fsize
full$FsizeD[full$Fsize == 1] = 'Alone'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] = 'Small'
full$FsizeD[full$Fsize > 4] = 'Big'

summary(full)

tapply(full$Age, full$Pclass,median, na.rm=TRUE)
tapply(full$Age, full$Title,median, na.rm=TRUE)

#Fill the missing values in Age variable
title.age = aggregate(full$Age,by = list(full$Title), FUN = function(x) median(x, na.rm = T))
full[is.na(full$Age), "Age"] = apply(full[is.na(full$Age), ] , 1, function(x) title.age[title.age[, 1]==x["Title"], 2])

sum(is.na(full$Age))

#Create variable Child based on Age
full$Child[full$Age < 18] = 'Child'
full$Child[full$Age >= 18] = 'Adult'

#Convert the variables to factor
full$Child = as.factor(full$Child)
full$Sex = as.factor(full$Sex)
full$Embarked = as.factor(full$Embarked)
full$Title = as.factor(full$Title)
full$Pclass = as.factor(full$Pclass)
full$FsizeD = as.factor(full$FsizeD)

#Split the data to train & test
train <- full[1:891,]
test <- full[892:1309,]

#Make prediction using randomForest
library(randomForest)
set.seed(123)

Model1 = randomForest(factor(Survived) ~ Pclass + Sex + Fare + Embarked + Title + 
                        FsizeD + Child, data = train)
summary(Model1)

Survived = predict(Model1, newdata = test, type = "response")
Survived 

str(Survived)
table(Survived)

#Create PassengerId from test & add it to Output dataframe
PassengerId= test$PassengerId
Output= as.data.frame(PassengerId)

#Add Survived column to the dataframe
Output$Survived = Survived
Output

#Save the Output as csv file
write.csv(Output, file = "kaggle_titanic_submission.csv", row.names = FALSE)
