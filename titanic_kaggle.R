train <- read.csv("titanic/train.csv", na.strings = "")

# 모델
train$mysurvived <- ifelse(train$Sex == "male", 0, 1)

# 정확도
sum(ifelse(train$mysurvived == train$Survived, 1, 0)) / nrow(train) # 0.7867565

# test데이터에 모델 적용
test <- read.csv("titanic/test.csv", na.strings = "")

test$Survived <- ifelse(test$Sex == "male", 0, 1)

# 제출파일 작성
submit <- test[c("PassengerId","Survived")]
write.csv(submit, "titanic_submission.csv", row.names = FALSE)
