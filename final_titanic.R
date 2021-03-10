# 2. 타이타닉 문제에 다양한 조건식 적용

train <- read.csv("titanic/train.csv", na.strings = "")
str(train)
train.copy <- train

# 1) Name의 호칭, Cabin, Embarked, Sex 항목의 데이터를 수치화하여 바꿔줌

# 1-1) Name의 호칭
name.words <- unlist(strsplit(train.copy$Name, split=" "))
name.title <- grep(".{2,}\\.$", name.words, value=T)
name.title
name.title <- gsub(pattern = "(Mlle|Ms|Lady|Dona|Miss)[.]", 
                   replacement = 1,
                   x = name.title)
name.title <- gsub(pattern = "(Mme|Mrs)[.]",
                   replacement = 2,
                   x = name.title)
name.title <- gsub(pattern = "Mr[.]",
                   replacement = 3,
                   x = name.title)
name.title <- gsub(pattern = "(Capt|Col|Major|Dr|Rev|Don|Sir|Countess|Jonkheer)[.]",
                   replacement = 4,
                   x = name.title)
name.title <- gsub(pattern = "Master[.]",
                   replacement = 5,
                   x = name.title)
table(name.title)
name.title <- as.numeric(name.title)
train.copy$name.title.num <- name.title

# name.title.num
# 1: Miss
# 2: Mrs
# 3: Mr
# 4: Officer
# 5: Master


# 1-2) Cabin

cabin.num <- substr(train.copy$Cabin,1,1)
cabin.num <- gsub("A", 1, cabin.num)
cabin.num <- gsub("B", 2, cabin.num)
cabin.num <- gsub("C", 3, cabin.num)
cabin.num <- gsub("D", 4, cabin.num)
cabin.num <- gsub("E", 5, cabin.num)
cabin.num <- gsub("F", 6, cabin.num)
cabin.num <- gsub("G", 7, cabin.num)
cabin.num <- gsub("T", 8, cabin.num)

cabin.num <- as.numeric(cabin.num)
train.copy$cabin.num <- cabin.num


# 1-3) Embarked

embarked.num <- train.copy$Embarked
embarked.num <- gsub("C", 1, embarked.num)
embarked.num <- gsub("Q", 2, embarked.num)
embarked.num <- gsub("S", 3, embarked.num)

which(is.na(train.copy$Embarked)) # 62 830
train.copy$Ticket[c(62,830)] # "113572"
grep('1135[\\d]*', train.copy$Ticket, value=T) # "113509" "113572" "113505" "113514" "113510" "113505" "113503" "113501" "113572"
names(which.max(table(train.copy$embarked.num[grep('1135[\\d]*', train.copy$Ticket)]))) # 3

grep('113[\\d]*', train.copy$Ticket, value=T)
names(which.max(table(train.copy$embarked.num[grep('113[\\d]*', train.copy$Ticket)]))) # 3
embarked.num[c(62,830)] <- '3'

embarked.num <- as.numeric(embarked.num)
train.copy$embarked.num <- embarked.num



# 1-4) Sex

sex.num <- train.copy$Sex
sex.num <- gsub("^male$", 1, sex.num)
sex.num <- gsub("^female$", 2, sex.num)

sex.num <- as.numeric(sex.num)
train.copy$sex.num <- sex.num


str(train.copy)



# 2) 유클리디안 거리 확인 & 가장 가까운 거리에 있는 다섯명 나이 평균을 NA에 넣기

st.d <- train.copy[c("name.title.num","Fare","Pclass","SibSp","Parch","cabin.num","embarked.num","sex.num")]
str(st.d)
age.st <- scale(st.d)

na.idx <- which(is.na(train.copy$Age))

for(i in 1:length(na.idx)){
  distance <- sqrt(rowSums((age.st-age.st[na.idx[i],])^2))
  train.copy$Age[na.idx[i]] <- mean(head(na.omit(train.copy$Age[order(distance)]), 5))
}

train.copy$Age


# 3) 나이와 생존의 상관관계

train.copy$age.cut <- cut(train.copy$Age,
                          breaks = c(0,10,20,30,40,50,max(train.copy$Age)+1),
                          right = F)
table(train.copy$age.cut)
prop.table(xtabs(~age.cut+Survived, train.copy), 1)
prop.table(xtabs(~age.cut+Survived, train.copy), 2)

# 4) fare과 생존의 상관관계

train.copy$fare.cut <- cut(train.copy$Fare, breaks = 7)
table(train.copy$fare.cut)
prop.table(xtabs(~fare.cut+Survived, train.copy), 1)
prop.table(xtabs(~fare.cut+Survived, train.copy), 2)

# 5) 그 외
prop.table(xtabs(~embarked.num+Survived, train.copy), 1)
prop.table(xtabs(~SibSp+Survived, train.copy), 1)
prop.table(xtabs(~Parch+Survived, train.copy), 1)
prop.table(xtabs(~Pclass+Survived, train.copy), 1)

train.copy[which(train.copy$sex.num==1)]

prop.table(xtabs(~embarked.num+Survived, train.copy[train.copy$sex.num==1]), 1)
prop.table(xtabs(~SibSp+Survived, train.copy), 1)
prop.table(xtabs(~Parch+Survived, train.copy), 1)
prop.table(xtabs(~Pclass+Survived, train.copy), 1)

# 4) 모델

train.copy$mysurvived <- with(train.copy, 
                              ifelse(fare.cut=="(439,513]",
                                     1,
                                     ifelse(SibSp==5|SibSp==8|Parch==4|Parch==6,
                                            0,
                                            ifelse(Sex=="female",
                                                   1,
                                                   0)
                                     )
                              )
)



sum(train.copy$Survived)
sum(train.copy$mysurvived)
sum(ifelse(train.copy$mysurvived == train.copy$Survived, 1, 0)) / nrow(train.copy)
# 0.7968575


# 5) titanic파일의 test.csv파일의 데이터 동일한 방식으로 전처리

test <- read.csv("titanic/test.csv")
test

# 5-1) Name의 호칭, Cabin, Embarked, Sex 항목의 데이터를 수치화하여 바꿔줌

# 5-1-1) Name의 호칭
name.words <- unlist(strsplit(test$Name, split=" "))
name.title <- grep(".{2,}\\.$", name.words, value=T)
name.title
name.title <- gsub(pattern = "(Mlle|Ms|Lady|Dona|Miss)[.]", 
                   replacement = 1,
                   x = name.title)
name.title <- gsub(pattern = "(Mme|Mrs)[.]",
                   replacement = 2,
                   x = name.title)
name.title <- gsub(pattern = "Mr[.]",
                   replacement = 3,
                   x = name.title)
name.title <- gsub(pattern = "(Capt|Col|Major|Dr|Rev|Don|Sir|Countess|Jonkheer)[.]",
                   replacement = 4,
                   x = name.title)
name.title <- gsub(pattern = "Master[.]",
                   replacement = 5,
                   x = name.title)
table(name.title)
name.title <- as.numeric(name.title)
test$name.title.num <- name.title

# 5-1-2) Cabin

cabin.num <- substr(test$Cabin,1,1)
cabin.num <- gsub("A", 1, cabin.num)
cabin.num <- gsub("B", 2, cabin.num)
cabin.num <- gsub("C", 3, cabin.num)
cabin.num <- gsub("D", 4, cabin.num)
cabin.num <- gsub("E", 5, cabin.num)
cabin.num <- gsub("F", 6, cabin.num)
cabin.num <- gsub("G", 7, cabin.num)
cabin.num <- gsub("T", 8, cabin.num)

cabin.num <- as.numeric(cabin.num)
test$cabin.num <- cabin.num


# 5-1-3) Embarked

embarked.num <- test$Embarked
embarked.num <- gsub("C", 1, embarked.num)
embarked.num <- gsub("Q", 2, embarked.num)
embarked.num <- gsub("S", 3, embarked.num)

embarked.num <- as.numeric(embarked.num)
test$embarked.num <- embarked.num


# 5-1-4) Sex

sex.num <- test$Sex
sex.num <- gsub("^male$", 1, sex.num)
sex.num <- gsub("^female$", 2, sex.num)

sex.num <- as.numeric(sex.num)
test$sex.num <- sex.num


str(test)




# 6-2) 유클리디안 거리 확인 & 가장 가까운 거리에 있는 다섯명 나이 평균을 NA에 넣기

st.d <- test[c("name.title.num","Fare","Pclass","SibSp","Parch","cabin.num","embarked.num","sex.num")]
str(st.d)
age.st <- scale(st.d)

na.idx <- which(is.na(test$Age))

for(i in 1:length(na.idx)){
  distance <- sqrt(rowSums((age.st-age.st[na.idx[i],])^2))
  test$Age[na.idx[i]] <- mean(head(na.omit(test$Age[order(distance)]), 5))
}

test$Age


# train과 달리 test.csv에는 fare에도 NA가 있어 이를 추가로 진행하였다

st.d.fare <- test[c("name.title.num","Age","Pclass","SibSp","Parch","cabin.num","embarked.num","sex.num")]
str(st.d.fare)
fare.st <- scale(st.d.fare)

na.idx.fare <- which(is.na(test$Fare))

distance <- sqrt(rowSums((fare.st-fare.st[na.idx.fare,])^2))
test$Fare[na.idx.fare] <- mean(head(na.omit(test$Fare[order(distance)]), 5))
test$Fare[na.idx.fare]


test$age.cut <- cut(test$Age,
                    breaks = c(0,10,20,30,40,50,max(test$Age)+1),
                    right = F)

# 4) fare과 생존의 상관관계

test$fare.cut <- cut(test$Fare, breaks = 7)



# 7) 모델 적용
test$Survived <- with(test, 
                      ifelse(fare.cut=="(439,513]",
                             1,
                             ifelse(SibSp==5|SibSp==8|Parch==4|Parch==6,
                                    0,
                                    ifelse(Sex=="female",
                                           1,
                                           0)
                             )
                      )
)

# 7) submission파일 생성

submit <- test[c("PassengerId","Survived")]
submit
write.csv(submit, "submission0310.csv", row.names = FALSE)







# 4. 타이타닉 문제에 대해 knn 적용

train.data <- train.copy[c("Survived","Pclass","SibSp","Parch","Fare","name.title.num","sex.num","embarked.num")]


# 1) 표준화
train.data_z <- as.data.frame(scale(train.data[-1]))
train.data_z

# 2) train.data 데이터를 train700개, test191개의 비율로 나눔
set.seed(1)
idx <- sample(1:nrow(train.data_z), 700)
train.data_tr <- train.data_z[idx,]
train.data_te <- train.data_z[-idx,]

train.data_tr_labels <- train.data[idx,1]
train.data_te_labels <- train.data[-idx,1]

# 3) 트레인 데이터로 knn 모델 생성

library(class)
train.data_te_pred <- knn(train = train.data_tr,
                          test = train.data_te,
                          cl = train.data_tr_labels,
                          k=15)
train.data_te_pred

# 4) 테스트 데이터로 테스트
table(train.data_te_pred, train.data_te_labels)
library(gmodels)
CrossTable(train.data_te_pred, train.data_te_labels)
sum(train.data_te_pred==train.data_te_labels)
# 0.8062827

# 6-3) knn 적용

test.data <- test[c("Pclass","SibSp","Parch","Fare","name.title.num","sex.num","embarked.num")]
# 6-3-1) 표준화
test.data_z <- as.data.frame(scale(test.data))
test.data_z

# 6-3-2) 생성한 knn모델 적용

test.data_pred <- knn(train = train.data_tr,
                      test = test.data_z,
                      cl = train.data_tr_labels,
                      k=19)
test$Survived <- test.data_pred

# 7) submission파일 생성

submit <- test[c("PassengerId","Survived")]
submit
write.csv(submit, "submission0310knn2.csv", row.names = FALSE)




# 3. 타이타닉 문제에 대해 knn 적용

train.data <- train.copy[c("Survived","Pclass","Age","SibSp","Parch","Fare","name.title.num","sex.num","embarked.num")]


# 1) 표준화
train.data_z <- as.data.frame(scale(train.data[-1]))
train.data_z

# 2) train.data 데이터를 train700개, test191개의 비율로 나눔
set.seed(1)
idx <- sample(1:nrow(train.data_z), 700)
train.data_tr <- train.data_z[idx,]
train.data_te <- train.data_z[-idx,]

train.data_tr_labels <- train.data[idx,1]
train.data_te_labels <- train.data[-idx,1]

# 3) 트레인 데이터로 knn 모델 생성

library(class)
train.data_te_pred <- knn(train = train.data_tr,
                          test = train.data_te,
                          cl = train.data_tr_labels,
                          k=19)
train.data_te_pred

# 4) 테스트 데이터로 테스트
table(train.data_te_pred, train.data_te_labels)
library(gmodels)
CrossTable(train.data_te_pred, train.data_te_labels)
sum(train.data_te_pred==train.data_te_labels)
# 0.8219895

# 5) test.csv에 knn 적용

test.data <- test[c("Pclass","Age","SibSp","Parch","Fare","name.title.num","sex.num","embarked.num")]

# 5-1) 표준화
test.data_z <- as.data.frame(scale(test.data))
test.data_z

# 5-2) 생성한 knn모델 적용

test.data_pred <- knn(train = train.data_tr,
                      test = test.data_z,
                      cl = train.data_tr_labels,
                      k=19)
test$Survived <- test.data_pred

# 6) submission파일 생성

submit <- test[c("PassengerId","Survived")]
submit
write.csv(submit, "submission0310knn.csv", row.names = FALSE)