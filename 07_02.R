library(ggplot2)
coin<-c(2,2,0,1,1,1,2,2,3,4,1,3,2,3,3,1,2,2,1,2,2)
coin.num=sort(unique(coin))
coin.freq=rep(0,4)
for(i in 1:length(coin.num)){
	coin.freq[i]<-length(coin[coin==i-1])
}
coin.rel=round(coin.freq/length(coin), 2)
coin.cum=cumsum(coin.rel)
coin.freq<-data.frame(coin.num, coin.freq)
names(coin.freq)[2]<-paste("val")
coin.freq$type=rep("freq", length(coin.num))
coin.freq
coin.rel=data.frame(coin.num, coin.rel)
names(coin.rel)[2]<-paste("val")
coin.rel$type=rep("rel", length(coin.num))
coin.rel
coin.cum=data.frame(coin.num, coin.cum)
names(coin.cum)[2]<-paste("val")
coin.cum$type = rep("cum", length(coin.num))
coin.cum
coin.graph=rbind(coin.freq, coin.rel, coin.cum)
ggplot(coin.graph, aes(coin.num, val, group=type, col=type))+geom_point()+geom_line()

score4<-c(3,3,6,7,7,10,10,10,11,13,30)
summary(score4)
total<-sum((score4-mean(score4))*(score4-mean(score4)))
total/length(score4)
var_val<-total/(length(score4)-1)
sqrt(var_val)
sd(score4)
range(score4)[2]-range(score4)[1]
diff(range(score4))
quantile(score4)
quantile(score4, 0.25, type=1)
quantile(score4, probs=c(1:100)/100)
fivenum(score4)
IQR(score4)
ret<-boxplot(score4, xlab="점수")
print(ret)


sapply(mtcars, mean, na.rm=TRUE)

library(MASS)
str(Cars93)
hist(Cars93$MPG.highway)
disc_1<-Cars93[,c("Model", "MPG.highway")]
head(disc_1)
disc_1<-within( disc_1,{
	MPG.highway_cd=character(0)
	MPG.highway_cd[MPG.highway>=20 & MPG.highway<25]="20~25"
	MPG.highway_cd[MPG.highway>=25 & MPG.highway<30]="25~30"
	MPG.highway_cd[MPG.highway>=30 & MPG.highway<35]="30~35"
	MPG.highway_cd[MPG.highway>=35 & MPG.highway<40]="35~40"
	MPG.highway_cd[MPG.highway>=40 & MPG.highway<45]="40~45"
	MPG.highway_cd[MPG.highway>=45 & MPG.highway<=50]="45~50"
	MPG.highway_cd=factor(MPG.highway_cd, level=c("20~25", "25~30","30~35","35~40","40~45","45~50"))
})
attributes(disc_1$MPG.highway_cd)
table(disc_1$MPG.highway_cd)
disc_1

tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sum)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, mean)
tapply(disc_1$MPG.highway, disc_1$MPG.highway_cd, sd)
ggplot(disc_1, aes(x=MPG.highway_cd, fill=MPG.highway_cd))+geom_dotplot()
#4분위수 범주화 
summary(disc_1$MPG.highway)
disc_1 <- within(disc_1, {
  MPG.highway_cd2 = character(0)
  MPG.highway_cd2 [MPG.highway < quantile(MPG.highway, 0.25)] = "1Q"
  MPG.highway_cd2 [MPG.highway >= quantile(MPG.highway, 0.25) &
                     MPG.highway < quantile(MPG.highway, 0.50)] = "2Q"
  MPG.highway_cd2 [MPG.highway >= quantile(MPG.highway, 0.50) &
                     MPG.highway < quantile(MPG.highway, 0.75)] = "3Q"
  MPG.highway_cd2 [MPG.highway >= quantile(MPG.highway, 0.75)] = "4Q"
  MPG.highway_cd2 = factor(MPG.highway_cd2, 
                           level = c("1Q","2Q","3Q","4Q"))
})
head(disc_1)
(table(disc_1$MPG.highway_cd2))
(disc_1 <- disc_1[order(disc_1$MPG.highway), ])

dim(disc_1)

# one hot encoding
# 표준편차의 문제점을 해결한 변동계수와 표준오차
# 평균이 증가할 때 분산도 증가하는 문제
# 데이터 표본수가 많아지면 표준편차가 점점 줄어든다.

data(trees)
dim(trees)
sd(trees$Volume)/mean(trees$Volume)
x=c(0,1,2,3,4,5,6,7,8,9,10,11)
sem<-sd(x)/sqrt(length(x))
c(mean(x)-2*sem, mean(x)+2*sem)
stderr<-function(x) sqrt(var(x, na.rm=TRUE)/length(na.omit(x)))
stderr(x)

x<-rnorm(1000,64.5,2.5)
x<-sort(x)
d<-dnorm(x, 64.5, 2.5)
pnorm(0)
pnorm(1)

# 다음 표준 점수를 구하라
# 평균이 100이고 분산이 10인 정규분포에서 50이 나올 확률은
pnorm(50, 100, sqrt(10))

# 평균이 90이고 분산이 12인 정규분포에서 98이 나올 확률은
pnorm(98, 90, sqrt(12))


# (1) 정규분포 그래프 (Normal distribution plot, X~N(0,1)) >  > 
x <- seq(-3, 3, length=200) 
plot(x, dnorm(x, mean=0, sd=1), type='l', main="Normal distribution, X~N(0,1)") 

# 특정한 브랜드의 백열전구의 수명이 1500시간의 평균값과 75시간의 표준편차로 정규분포로
# 이루어져있다. 백열전구가 1410시간보다 덜 오래갈 확률은 얼마인가
# 1410-1500/75
pnorm(-1.2)
# 백열전구가 1563과 1648시간 사이일 확률은 얼마인가
pnorm(1648, 1500, 75)-pnorm(1563, 1500, 75)

# 우리나라에서 사육하고 있는 생후 18개월 이상된 황소무게는 평균이 500kg이고 표준편차가
# 50kg인 정규분포라 한다. 이제 우량한우를 집중 육성 관리하기 위해 무게가 무거운 순서대로
# 5%에 해당하는 황소를 선발하고자 한다. 그렇다면 무게가 몇 kg이상인 황소를 선발해야하는가
qnorm(1-0.05, 500,50)

# X~(300, 50^2)인 정규분포에서 P(X>= 370)일 확률을 구하여라
1-pnorm(1.4)

x<-matrix(1:10, ncol=2)
centered.x<-scale(x,scale=FALSE)
scale(x)

#z값
height <- transform(height, z2.height_korean =(height_korean - mean(height_korean))/sd(height_korean),
                    z2.height_bushman = (height_bushman - mean(height_bushman)) / sd(height_bushman))
#scale은 백터로 들어온 데이터를 대상으로 mean을 구하고 sd를 구한 다음 하나의 데이터마다 z점수를 구해준다.
height <- transform(height, z.height_korean = scale(height_korean), z.height_bushman=scale(height_bushman))
hist(height$z.height_korean, freq=T, main="한국인 표준")
hist(height$z.height_bushman, freq=T, main="부시맨 표준")









