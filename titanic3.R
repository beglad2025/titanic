test <- read.csv("titanic/test.csv", na.strings = "")

# 1. 유클리디안 거리 활용해 Age 열 NA대체
# 1-1. 호칭 data 추출 및 수치화

name.words <- unlist(strsplit(test$Name, split=" "))
name.title <- grep(".{2,}\\.$", name.words, value=T)

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

# name.title.num
# 1: Miss
# 2: Mrs
# 3: Mr
# 4: Officer
# 5: Master


# 1-2. 표준화(data-mean/sd)

str(test)
st.d <- test[c("name.title.num","Fare","Pclass","SibSp","Parch")]
age.st <- scale(st.d)


# 1-3. 유클리디안 거리 확인 & 가장 가까운 거리에 있는 다섯명 나이 평균을 NA에 넣기

na.idx <- which(is.na(test$Age))

for(i in 1:length(na.idx)){
  distance <- sqrt(rowSums((age.st-age.st[na.idx[i],])^2))
  test$Age[na.idx[i]] <- mean(head(na.omit(test$Age[order(distance)]), 5))
}

test$Age




test$age.cut <- cut(test$Age,
                    breaks = c(0,20,40,max(test$Age)+1),
                    right = F)

test$fare.cut <- cut(test$Fare, breaks = 4)




# 1
# test$Survived <- with(test, 
#                       ifelse(Sex=="female",
#                              ifelse(age.cut=="[20,40)"|age.cut=="[0,20)",1,ifelse(Pclass==1,1,0)),
#                              ifelse(fare.cut=="(384,513]",1,ifelse(fare.cut=="(128,256]"&age.cut=="[0,20)",1,0)))
# )



# 2
test$mysurvived <- with(test,
                         ifelse(Sex=="female",
                                ifelse(age.cut=="[20,40)"|age.cut=="[0,20)",1,ifelse(Pclass==1,1,0)),
                                ifelse(fare.cut=="(384,513]",1,0))
                        )

# 3
test$mysurvived <- with(test, 
                         ifelse(Sex=="female",
                                ifelse(Pclass==1,1,ifelse(age.cut=="[0,20)"|age.cut=="[20,40)",1,ifelse(Parch==3|Parch==1,1,0))),
                                ifelse(fare.cut=="(384,513]"&age.cut=="[20,40)",1,0))
)



submit <- test[c("PassengerId","Survived")]
write.csv(submit, "titanic_submission7.csv", row.names = FALSE)                 



