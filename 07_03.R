# 통계학 공부
#			범주형			연속형
#			
#범주형변수	분할표			t-test
#			카이제곱검정	ANOVA
#--------------------------------------------
# 카이제곱검정: 두 데이터의 관계의 독립성 검정
# t분포검정: 두 집단의 평균 비교

data<-textConnection(
"브랜드 종류 관측도수
	BrandA 18
	BrandB 23
	BrandC 19")
x<-read.table(data, header=TRUE)
chisq.test(x$관측도수)


ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
stat_function(fun=dnorm, colour="blue", size=1) +
stat_function(fun=dt, args=list(df=3), colour="red", size=2) +
stat_function(fun=dt, args=list(df=1), colour="yellow", size=3) +
annotate("segment", x=1.5, xend=2, y=0.4, yend=0.4, colour="blue", size=1) +
annotate("segment", x=1.5, xend=2, y=0.37, yend=0.37, colour="red", size=2) + 
annotate("segment", x=1.5, xend=2, y=0.34, yend=0.34, colour="yellow", size=3) + 
annotate("text", x=2.4, y=0.4, label="N(0,1)") +
annotate("text", x=2.4, y=0.37, label="t(3)") + 
annotate("text", x=2.4, y=0.34, label="t(1)") + 
ggtitle("Normal Distribution, t-distribution")

# shapiro wilk test
shapiro.test(rnorm(1000))
set.seed(450)
x<-runif(50, min=2, max=4)
shapiro.test(x)

# var.test(): 귀무가설 분산이 동일하다. 0.05보다 작으면 분산이 다르다
str(sleep2<-sleep[,-3])
tapply(sleep2$extra, sleep2$group, mean)
# formula ~를 이용하면 자동 그룹핑된다.
var.test(extra~group, sleep2)
t.test(extra~group, data=sleep2, paired=TRUE, var.equal=TRUE)

# iris의 sepal.width와 sepal.length의 등분산성 테스트를 하시오
# 정규분포가 아닐경우 wilcox.test

# MASS 패키지에 내장된 Cars93 데이터 프레임의 가격과 생산국가
# 데이터에서 생산국이 USA vs non-USA 2개의 group에 대하여 차 가격의
# 평균이 차이가 있는지를 검정해 보시오
# 유의수준은 0.95
# 다음과 같은 형식으로 보고서를 제출하시오
# 1)가설설정: 대립가설, 귀무가설
# 귀무가설: Origin이 USA, non-USA인 차량의 price의 평균은 같다.
# 대립가설: Origin이 USA, non-USA인 차량의 price의 평균은 다르다.
# 2)연구환경
# 3)유의수준
# 4)분석방법
# 5)검정통계량
# 6)유의확률
# 7)결과해석

var.test(Cars93[Cars93$Origin=="USA",5], Cars93[Cars93$Origin=="non-USA",5])
t.test(Cars93[Cars93$Origin=="USA",5], Cars93[Cars93$Origin=="non-USA",5], paired=FALSE, var.equal=TRUE, conf.level = 0.95)

# ANOVA: 분산분석
# ANCOVA: 공분산분석
# bartlett.test

# PlantGrowth 데이터의 group별로 weight의 평균차이가 있는지 검정통계량

# airquality에 대한 상관계수
# 숫자만 처리
# na는 제거
# 상관정도 확인
# corrplot 이용
# 가장 큰, 약한 상관관계
# x<-airquality %>% select_if(function(col) is.numeric(col))
x=airquality[c("Ozone","Solar.R","Wind","Temp")]
m<-cor(x, use="complete.obs")
corrplot(m, method="number", col=rainbow(length(m)))
heatmap(m, col=rainbow(length(m)), symm=TRUE)

# 상삼각행렬과 하삼각행렬

165