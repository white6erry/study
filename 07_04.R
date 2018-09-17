#딥러닝은 행렬을 이해하지 못하면 처리못한다. 행렬을 컨트롤 해야한다. 
#회귀분석: 선형모형 분석을 통해 예측
#오차는 등분산성, 정규성, 독립성 가정
#상관분석은 PCA, FA에 의하여 활용, 회귀분석은 예측
#잔차: 기대치와 관찰치의 차이
#이상치
 #Leverage: 독립변수에 의한 이상치(좌우)
 #Influence: 종속변수에 의한 이상치(상하)
#회귀모델의 평가
 #모델이 통계적으로 유의미한가.
 #계수의 유의미 정도 파악.
 #모형이 유용한가? (=설명력이 있는가?) R-squared, Adjusted R-squared.
 #모델이 데이터를 잘 fitting하고 있는가? (잔차그래프)
 #데이터에 적용된 선형회귀는 적절한가? cor.
####################################################
# 선형 회귀, 로지스틱 회귀

y = c(1,5,3,8,5,3,10,7)
x1 = c(2,4,5,6,8,10,11,13)
x2 = c(1,2,2,4,4,4,6,6)
opar=(par(mfrow=c(1,3)))
plot(x1,y)
plot(x2,y)
plot(x1,x2)
summary(lm(y~x1))
summary(lm(y~x2))
summary(lm(y~x1+x2))

#if x1 =6; x2=4
y = -1.223*x1+ 3.6493 *x2 + 1.0355 ; y
y = -1.223*6+ 3.6493 *4 + 1.0355 ; y

str(women)
fit = lm(weight ~ height, data=women)
summary(fit)
women.fitted_weight = -87.52 + 3.45*women$height
plot(weight~height, data=women)
abline(fit, col="blue")
cor(women$weight, women$height)

#cars데이터의 speed와 dist간의 선형회귀를 실시하시오.
str(cars)
reg = lm(dist~speed, data=cars) ; summary(reg)
cor(cars$speed, cars$dist)
plot(cars$speed, cars$dist)
cof = coef(reg) ; cof
class(cof)
cof["speed"]
cof["(intercept)"]
plot(cars$speed, cars$dist, xlab="속도", ylab="정지거리", xlim=c(0,30), ylim=c(0,125))
abline(coef(reg), col=2)
fitted(reg) [1:4] #적합
residuals(reg) [1:4] #잔차
deviance(reg) #편차값
plot(reg, which=1) #잔차그래프; 수평일수록 이상적

#자기 상관성: 이전 값이 다음 값에 영향을 미치는 정도(시계열분석에서 중요).
#install.packages("lmtest")
library(lmtest) #2이하면 자기 상관성이 없다.
dwtest(reg)
#reg의 회귀모델
predict(reg, newdata=data.frame(speed=10)) #점추정
predict(reg, newdata=data.frame(speed=10), interval="confidence") #구간추정
predict(reg, newdata=data.frame(speed=10), intercal="prediction") #차량상태등을 random으로 고려해서 에측.
par(mfrow=c(2,2))
plot(reg)

#########
data <- iris
left <- "Sepal.Length" #어떻게 하면 가장 좋은 적합을 찾아내느냐?
upper = "~Sepal.Width + Petal.Length + Petal.Width"
plot(data)
m <- lm(formula(paste(left, upper)), data =data)
summary(m)
#step()는 변수선택법. AIC는 변수적합도를 결정하는 값인데 작을수록 좋다.
step(lm(formula(paste(left, "~1")), data=data), scope = list(lower=~1, uppwer=formula(upper)), 
     direction="forward") #전진선택법.
step(lm(formula(paste(left, upper)), data=data), scope = list(lower=~1, uppwer=formula(upper)), 
     direction="backward") #후진선택법
step(lm(formula(paste(left, "~1")), data=data), scope = list(lower=~1, uppwer=formula(upper)), 
     direction="both") #단계적 선택법

#정규화? 변수의 크기가 종속변수에 영향을 미치면 안됨. 동일한 기준으로 이펙트를 측정해야 한다.
library(MASS)
head(Boston,3)
boston_df <- as.data.frame(scale(Boston)) #scale정규화(관측치-평균)/표준편차
head(boston_df, 3)
#sampling
set.seed(123)
idx <- sample(1:nrow(boston_df), 300) ; str(idx) #범위속에서 선택된 인덱스가 들어간다.
trainDF <- boston_df[idx,] ; trainDF
testDF <- boston_df[-idx,] ; testDF #앞에 뽑은 애를 제외하고 표집해라.
dim(trainDF)
dim(testDF)

#model 생성 (팩터형 변수들 제외). 회귀분석에서는 팩터형 변수들이 유의미하지 않다.
form <- medv ~ crim+zn+indus+nox+rm+age+dis+rad+tax+ptratio+black+lstat
lm_model <- lm(formula = form, data= trainDF) ; lm_model #방정식을 구성하는 베티값과 상수값이 들어있다.
names(lm_model)
lm_model$fitted.values[1:5]
lm_model$fitted.residuals[1:5]
trainDF$modv[1:5]
summary(lm_model)
pred <- predict(lm_model, testDF) #회귀분석에서는 연속형 데이터를 쓴다.
length(pred) #연속형 변수의 예측값은 사실 타당성을 가지기 어렵다. 따라서,
cor(pred, testDF$medv) #상관분석을 통해 예측값의 타당성 검증.
#머신러닝도 이러한 형식을 따른다.

# 다중공선성 검사
# 독립변수들은 종속변수에만 영향을 미쳐야한다.
install.packages("car")
library(car)
sqrt(vif(lm_model)) > 2
cor(trainDF[c('nox','dis','rad','tax')])
form<-medv~crim+zn+indus+rm+age+dis+rad+ptratio+black+lstat
lm_model<- lm(formula=form, data=trainDF)
sqrt(vif(lm_model)) > 2


step(model_ins, direction="both")
model_ins<-lm(charges~age+bmi+children+smoker, data=training_ins)
model_ins
summary(model_ins)

# 로지스틱 회귀분석
# sigmoid함수를 이용해서 확률값으로 0~1사이의 값으로 분류
# 로지스틱 회귀분석은 정규분포가 아니라 이항분포를 따름
# 종속 변수가 2개 이상인 경우 dummy 변수화 함(one-hot encoding과 같은개념)
# glm(formula, family=familytype(link=linkfunction), data=)
# 
Dine<-matrix(c(0,17,0,19,0,20,0,20.5,0,21.5,0,22,1,24,1,25,0,25,0,27,1,28,1,30,0,31.5,1,32,1,34,1,35,1,36.5,1,39,1,40,1,44),nc=2, byrow=T)
colnames(Dine)<-c("Class", "Income")
Dine<-as.data.frame(Dine)
Logit.model<-glm(Class~Income, family="binomial", data=Dine)
summary(Logit.model)
Logit.model$coeff[2]
OR<-exp(Logit.model$coeff[2]) # 우도비
OR

weather=read.csv(file="C:\\Users\\acorn\\Downloads\\weather.csv", header=TRUE, stringsAsFactors=F)
dim(weather)
head(weather)
str(weather)

set.seed(100)

install.packages("ROCR")
library(ROCR)
pr<-prediction(pred, test$RainTomorrow)
prf<-performance(pr, measure="tpr", x.measure="fpr")
plot(prf)

# Smarket :이전 lag의 데이터로 부터 명일의 주식 방향을 예측하는 데이터
# logistic regression을 통해 예측 모델을 생성하시오
install.packages("ISLR")
library(ISLR)
str(Smarket)
set.seed(100)
idx<-sample(x=c("train", "test"), size=nrow(Smarket), prob=c(7,3))
train<-Smarket[idx=="train",]
test<-Smarket[idx=="test",]

with(Smarket, plot(jitter(as.numeric(Direction), 1, 0.03)~jitter(Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 1, 0.1), data=Smarket))

glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
glm.probs<-predict(glm.fit, type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred<-rep("Down", dim(Smarket)[1])
glm.pred[glm.probs>0.5]<-"Up"
res=table(glm.pred, Smarket$Direction)
confusionMatrix(as.factor(glm.pred), Smarket$Direction)
detach(res)




