'''
분류 분석
- 회귀분석, 로지스틱분석
- 의사결정나무
- KNN
- 베이지안 분류
- ANN
- SVM

dicision tree
random forest
오늘 해야할 것

'''

# Dicision Tree
# 장점
# - 시각화
# 단점
# - 추가적인 데이터는 예측 불가
# 불순도
# 카이제곱, 지니, 엔트로피

x1<- 0.5
x2<- 0.5
e1<- -x1*log2(x1) - x2*log2(x2)
e1

x1<- 0.8
x2<- 0.2
e2<- -x1*log2(x1) - x2*log2(x2)
e2

library(rpart)
result<-sample(1:nrow(iris), nrow(iris)*0.7)

train<-iris[result,]
test<-iris[-result,]
# formula 종속변수 ~ 독립변수
formula<-Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width
model<-rpart(formula=formula, data=train)
model
pred<- predict(model, test)
pred
cpred<-ifelse(pred[,1]>=0.5, "setosa", ifelse(pred[,2]>=0.5, "versicolor", "virginica"))
(tb=table(cpred, test$Species))
# Dicision Tree
sum(diag(tb))/nrow(test)
plot(model)
text(model, use.n=T, cex=0.6)
post(model, use.n=TRUE, file="")
# 노드가 선으로
x11()
formula<-Species~.
iris.df<-rpart(formula, data=iris)
iris.df
plot(iris.df)
text(iris.df, use.n=T, cex=0.6)
post(iris.df, file="")
# 알록달록
install.packages("rpart.plot")
install.packages("rattle")
library(rpart.plot)
library(rattle)
prp(model)
rpart.plot(model)

fancyRpartPlot(model)

# wdbc_data.csv를 이용하여 암진단 유무를 결정하는 dicision_tree를 생성하시오
# rpart함수를 사용하시오
# 7:3으로 데이터 분할하여 테스트하고 정분류율을 구하시오
# 오분류율, 정밀도, 특이도, 민감도를 구하시오
wdbc_data<-read.csv("C:\\Users\\acorn\\Downloads\\wdbc_data.csv")
str(wdbc_data)
head(wdbc_data)
# 전처리
wdbc_data<-wdbc_data[-1]
wdbc_data$diagnosis<-factor(wdbc_data$diagnosis, levels=c("B","M"))
wdbc_data$diagnosis[1:10]
normalize<-function(x){
	return ((x-min(x))/(max(x)-min(x)))
}
wdbc_x<-as.data.frame(lapply(wdbc_data[2:31], normalize))
wdbc_data
summary(wdbc_data)
wdbc_df<-data.frame(wdbc_data$diagnosis, wdbc_x)
head(wdbc_df)
dim(wdbc_df)

idx<-sample(1:nrow(wdbc_df), nrow(wdbc_df)*0.7)
wdbc_train<-wdbc_df[idx,]
wdbc_test<-wdbc_df[-idx,]
model2<-rpart(wdbc_data.diagnosis~., data=wdbc_train)
pred2<-predict(model2, wdbc_test,type='class')

prp(model2)
rpart.plot(model2)
fancyRpartPlot(model2)
# 분류모델 평가(정분류율, 오분류율, 정밀도, 민감도, 특이도)
tr<-table(pred2, wdbc_test$wdbc_data.diagnosis)
paste(round(sum(diag(tr))/nrow(wdbc_test)*100),'%')
paste(round(sum(tr[1,2],tr[2,1])/nrow(wdbc_test)*1000)/10,'%')
paste(round(tr[1,1] / sum(tr[1,])*100),'%')
paste(round(tr[1,1] / sum(tr[,1])*100),'%')
paste(round(tr[2,2] / sum(tr[,2])*100),'%')

library(party)
library(datasets)
str(airquality)
formula<-Temp~Solar.R+Wind+Ozone
air_ctree<-ctree(formula, data=airquality)
plot(air_ctree)

# rpart
# weather.csv 를 읽어 RainTomorrow가 y변수 data, raintoday를 
# 제외한 나머지 변수가 x변수가 되도록 하여 dt를 작성

weather<-read.csv("C:\\Users\\acorn\\Downloads\\weather.csv")
weather<-weather[-1]
weather<-weather[-(length(weather)-1)]
idx<-sample(1:nrow(weather), nrow(weather)*0.7)
weather_train<-weather[idx,]
weather_test<-weather[-idx,]
model3<-rpart(RainTomorrow~., data=weather_train)
pred3<-predict(model3, weather_test)
prp(model3)
rpart.plot(model3)
fancyRpartPlot(model3)

df<-read.csv("C:\\Users\\acorn\\Downloads\\Heart.csv")
library(caret)
set.seed(1000)
intrain<-createDataPartition(y=df$AHD,p=0.7, list=FALSE)
train<-df[intrain,]
test<-df[-intrain,]
library(tree)
treemod<-tree(AHD~., data=train)
plot(treemod)
text(treemod)

library(caret)
credit<-read.csv("C:\\Users\\acorn\\Downloads\\credit.csv")
set.seed(300)
m<-train(default~., data=credit, method="C5.0")
p<-predict(m, credit)
table(p, credit$default)
# 최고 성능의 표준편차 안에서 가장 단순한 후보자를 선택하겠다.
ctrl<-trainControl(method="cv", number=10, selectionFunction="oneSE")
grid<-expand.grid(.model="tree", .trials=c(1,5,10,15,20,25,30,35), .winnow="FALSE")
set.seed(300)
m<-train(default~., data=credit, method="C5.0", metric="Kappa", trControl=ctrl, tuneGrid=grid)
p<-predict(m, credit)
table(p, credit$default)

library(ipred)
set.seed(300)
mybag<-bagging(default~., data=credit, nbagg=25)
credit_pred<-predict(mybag, credit)
table(credit_pred, credit$default)

# random forest
# DT의 과적합 데이터 순서가 바뀌면 결과가 바뀌는 문제 해결
# bootstraping 데이터간 중복을 허용해 다양한 조합으로 데이터 생성
# boosting 모델의 강약에 가중치를 부여하여 개선한 다음 모델을 생성
# bagging = boosting + aggfregation

library(randomForest)
fit<-randomForest(Kyphosis~Age + Number + Start, data=kyphosis)
print(fit)
plot(fit)
importance(fit)
res<-getTree(fit, 1, labelVar=TRUE)

data(iris)
model = randomForest(Species~., data=iris, ntree=300, myry=4, na.action=na.omit)
ntree<-c(400,500,600)
mtry<-c(2:4)
param<-data.frame(n=ntree, m=mtry)
for( i in param$n){
	cat('ntree = ', i ,'\n')
	for(j in param$m){
		cat('mtry = ', j, '\n')
		model=randomForest(Species~., data=iris, ntree=i, myry=j, na.action=na.omit)
		print(model)
	}
}
model3=randomForest(Species~., data=iris, ntree=500, mtry=2, importance=T, na.action=na.omit)
importance(model3)
model3$importance
varImpPlot(model3)

library(foreach)
m<-matrix(1:9, 3)
result<-0
for( i in 1:ncol(m)){
	result[i]<-mean(m[i,])
}
foreach(i=1:nrow(m), .combine=c) %do% mean(m[,i])

model_iris<-foreach(i=rep(250,4), .combine=combine) %do%
randomForest(Species~., data=iris, ntree=i, mtry=2, na.action=na.omit)
model_iris
str(model_iris)
importance(model_iris)
table(model_iris$predicted, iris$Species)

library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
foreach(i=1:3) %dopar% sqrt(i)
registerDoParallel(cores=2)
foreach(i=1:3) %dopar% sqrt(i)

x<-iris[which(iris[,5] !="setosa"), c(1,5)]
trials<-10000

# weatherAUS를 100개의 트리와 2개의 분류변수를 파라미터로 하여 모델을 생성하고
# 분류 정확도를 구하시오
# 변수 중요도를 출력하시오
weather<-read.csv("C:\\Users\\acorn\\Downloads\\weather.csv")
weather=weather[,c(-1,-6, -8, -14)]
str(weather)
weather<-na.omit(weather)
set.seed(123)
result<-sample(1:nrow(weather), nrow(weather)*0.7)
train<-weather[result,]
test<-weather[-result,]
model<-randomForest(RainTomorrow~., data = weather, ntree=100, myry=2, na.action=na.omit, importance=TRUE)
p1<-predict(model, newdata=test)
c<-confusionMatrix(p1, test$RainTomorrow)
importance(model)
varImpPlot(model)

# 최적의 트리와 분류변수 파라미터의 개수를 찾아보시오
ntree <- c(200,300,400,500,600)
mtry <- c(6:10)
param <- data.frame(n=ntree, m=mtry)
param

for(i in param$n){
  cat('ntree =', i, "\n")
  for(j in param$m){
    cat("mtry =", j, "\n")
    model <-randomForest(RainTomorrow~., data=train,
                         ntree=i, mtry=j,
                         na.action=na.omit)
    print(model)
  }
}
str(model)
library(caret)
ctrl <- trainControl(method="repeatedcv", number = 10, repeats=10)
grid_rf <- expand.grid(.mtry=c(2,4,6,16))
set.seed(300)
m_rf <- train(RainTomorrow~., data=weather, method="rf",
              metric = "Kappa", trControl = ctrl,
              tuneGrid = grid_rf)

# 병렬처리를 적용하여 작업속도를 개선해 보시오
registerDoParallel(cores=4)
getDoParWorkers()
system.time(
  rf_weather <- foreach(ntree=rep(200,1), .combine = combine,
                        .packages = "randomForest", .multicombine = TRUE) %dopar%
  randomForest(RainTomorrow~., data=train,
               ntree=ntree, mtry=8, importance=TRUE, na.action=na.omit)
)
rf_weather
str(rf_weather)
pred <- predict(rf_weather, newdata=test)
tb <-table(pred, test$RainTomorrow)
paste("정분류율 =", round(sum(diag(tb))/sum(tb)*100), "%")
