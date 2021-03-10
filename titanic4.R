train <- read.csv("titanic/train.csv", na.strings = "")
train

# 

set.seed(210309)
sample(train, )


train$age.cut <- cut(train$Age,
                     breaks = c(0,20,40,max(train$Age)+1),
                     right = F)

train$fare.cut <- cut(train$Fare, breaks = 4)

train$mysurvived <- with(train, 
                        ifelse(Sex=="female",
                               ifelse(Pclass==1,1,ifelse(age.cut=="[0,20)"|age.cut=="[20,40)",1,0)),
                               ifelse(fare.cut=="(384,513]"&age.cut=="[20,40)",1,0))
)

sum(train$Survived)
sum(train$mysurvived)
sum(ifelse(train$mysurvived == train$Survived, 1, 0)) / nrow(train)




test <- read.csv("titanic/test.csv", na.strings = "")

test$age.cut <- cut(test$Age,
                    breaks = c(0,20,40,max(test$Age)+1),
                    right = F)

test$fare.cut <- cut(test$Fare, breaks = 4)


test$mysurvived <- with(test, 
                        ifelse(Sex=="female",
                               ifelse(Pclass==1,1,ifelse(age.cut=="[0,20)"|age.cut=="[20,40)",1,ifelse(Parch==3|Parch==1,1,0))),
                               ifelse(fare.cut=="(384,513]"&age.cut=="[20,40)",1,0))
)



submit <- test[c("PassengerId","Survived")]
write.csv(submit, "titanic_submission7.csv", row.names = FALSE)                 
