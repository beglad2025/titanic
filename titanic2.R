train <- read.csv("titanic/train.csv", na.strings = "")


# 1. 유클리디안 거리 활용해 Age 열 NA대체
# 1-1. 호칭 data 추출 및 수치화

name.words <- unlist(strsplit(train$Name, split=" "))
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
train$name.title.num <- name.title

# name.title.num
# 1: Miss
# 2: Mrs
# 3: Mr
# 4: Officer
# 5: Master


# 1-2. 표준화(data-mean/sd)

str(train)
st.d <- train[c("name.title.num","Fare","Pclass","SibSp","Parch")]
age.st <- scale(st.d)


# 1-3. 유클리디안 거리 확인 & 가장 가까운 거리에 있는 다섯명 나이 평균을 NA에 넣기

na.idx <- which(is.na(train$Age))

for(i in 1:length(na.idx)){
  distance <- sqrt(rowSums((age.st-age.st[na.idx[i],])^2))
  train$Age[na.idx[i]] <- mean(head(na.omit(train$Age[order(distance)]), 5))
}

train$Age


# 2. 나이와 생존의 상관관계(20대, 30대)

train$age.cut <- cut(train$Age,
                     breaks = c(0,20,40,max(train$Age)+1),
                     right = F)
prop.table(xtabs(~age.cut+Survived, train), 1)
prop.table(xtabs(~age.cut+Survived, train), 2)


# 3. 등실과 생존의 상관관계(1>3>2 순서)

prop.table(xtabs(~Pclass+Survived, train), 1)
prop.table(xtabs(~Pclass+Survived, train), 2)

# 4. Fare과 생존의 상관관계((-0.512,102]구간이 압도적으로 높음)

train$fare.cut <- cut(train$Fare, breaks = 4)
prop.table(xtabs(~fare.cut+Survived, train), 1)

# 5. 성별과 생존의 상관관계(여>남)

prop.table(xtabs(~Sex+Survived, train), 1)
prop.table(xtabs(~Sex+Survived, train), 2)
xtabs(~Sex+Survived, train)


prop.table(xtabs(~SibSp+Survived, train), 1)
prop.table(xtabs(~Parch+Survived, train), 1)

s <- with(train, 
     tapply(Survived, 
            list(age.cut, Sex, Pclass), 
            sum)
     )

prop.table(s)

# 모델링


train$mysurvived <- with(train, 
                         ifelse(Sex=="female",
                                ifelse(Pclass==1,1,ifelse(age.cut=="[0,20)",1,0)),
                                ifelse(fare.cut=="(410,513]",1,0))
)
# 0.7833895


train$mysurvived <- with(train, 
                         ifelse(Sex=="female",
                                ifelse(age.cut=="[20,40)"|age.cut=="[0,20)",1,ifelse(Pclass==1,1,ifelse(Parch==3|Parch==1,1,0))),
                                ifelse(fare.cut=="(410,513]",1,0))
)
# 0.7867565


train$mysurvived <- with(train, 
                         ifelse(Sex=="female",
                                ifelse(age.cut=="[20,40)"|age.cut=="[0,20)",1,ifelse(Pclass==1,1,ifelse(Parch==3|Parch==1,1,0))),
                                ifelse(fare.cut=="(410,513]",1,ifelse(fare.cut=="(102,205]"&age.cut=="[20,40)",1,0)))
)
# 

train$mysurvived <- with(train, 
                         ifelse(Sex=="female",
                                ifelse(age.cut=="[20,40)"|age.cut=="[0,20)",1,ifelse(Pclass==1,1,ifelse(Parch==3|Parch==1,1,0))),
                                ifelse(fare.cut=="(384,513]",1,ifelse(fare.cut=="(128,256]"&age.cut=="[0,20)",1,0)))
)
# 

sum(train$Survived)
sum(train$mysurvived)
sum(ifelse(train$mysurvived == train$Survived, 1, 0)) / nrow(train)
                         

