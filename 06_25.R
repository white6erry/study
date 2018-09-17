int <- 100
string <-'대한민국'
boolean <-TRUE
mode(int); mode(string);mode(boolean);
class(int);
is.numeric(int)
is.character(string)
is.logical(boolean);
score <- c(85,95,NA,75,65)
score
mean(score, na.rm=T)
sum(score, na.rm=T)

# 산술평균: 이상치 때문에 영향을 덜받는 median을 사용한다.
x<-c(1,2,3,'4')
mode(x)
#괄호 치는 이유는 출력하기 위해서
(xx<-as.numeric(x))

# factor (범주형: 정성적 데이터(명명식, 순서식))
gender <-c('M','F','M','F','M')
gender
#plot(gender)
fgender<- as.factor(gender)
fgender
fgender[2]
mode(fgender);
class(fgender) # class 자료형
plot(fgender)

Sys.getenv("PATH")
Sys.Date()
Sys.time()

(today<-'2017-03-11 10:15:24')
mode(today)
class(today)

(today2<-as.Date(today))
mode(today2)
class(today2)

help(mean)
args(sum)
example(sum)

data()
Nile
plot(Nile)

# working directory
getwd()
# 역슬래시 두개
setwd("C:\\work\\R")

# 메모리의 변수
ls()
rm(list=ls())

# []: indexing, {}: block, ():function
a = c(1,2,3)
a[2]
b=1:10
b[c(2,3,4)]

m1=matrix(c(1,2,3,4), nrow=2, byrow=TRUE)
m2=matrix(c(1,2,"three", 4), nrow=2)
help(matrix)

# 데이터 프레임은 벡터를 결합
# 데이터 프레임은 열중심
df1=data.frame(v1=c(1,2), v2=c("A","B"))

# vector
# 1차원 데이터
# 벡터의 연산(+,-,/,*) 같은 위치에 있는 속성마다 연
#
(x=vector("numeric",10))
(x=numeric(10))

seq(1,3,by=0.2)
seq(1,5,length.out=4)
(c<-seq(1,9,by=2))

(b<-rep(1:4,2))
(d<-rep(1:3,each=3))

x<-c(1,2,3,4,5,6,7,8,9)
x[-1] # 제외한다
x[c(TRUE, FALSE, FALSE, TRUE)]
x[x<3] 

#key: 데이터 형식으로 입력가능 키는 열이름
x<-c("first"=3,"second"=0)
x["second"]

# vector 연산
x=c(3,6,8)
y=c(2,9,0)
x+y
x+1
x+c(1,4)
# 연산자 정의 %%사이를 이용해 정의한다.
x%*%y
"a" %in% c("a", "b", "c")
c(1,2,3)==c(1,2,100)

all(x>2)
any(x>2)

x<-c(3,2,5,4,8,1)
sort(x)
sort(x, decreasing=TRUE)
order(x)
order(x, decreasing=TRUE)
x[order(x)]

vectorA<-c(1,2,3)
names(vectorA)<-c("aaa", "bbb", "ccc")
vectorA["aaa"]

# 변수에 값을 추가하지 않고 리턴함
vectorB = append(vectorA, c(3,4,5))

# 집합 연산
union(vectorA, vectorB)
intersect(vectorA, vectorB)
setdiff(vectorA, vectorB)
setequal(vectorA, vectorB)

par(mfrow=c(1,2))
df2=read.csv(file.choose())
plot(df2)
help(plot)

df3=read.csv(file.choose())
plot(df3, main="2017", type="s")
points(col="dark red")

(x <- c(3,2,6,4,5,8,1))
subset(x,x>3)
# 위치값 반환
which(x*x > 8)
nums<-c(5,8,10,NA,3,11)
which.min(nums)
nums[which.min(nums)]

# vector
# vector1 변수를 만들고 "R"문자가 5회 반복되도록 하시오
(vector1 <- rep("R",5))
# vector2 변수에 1~20까지 3간격으로 연속된 정수를 만드시오
(vector2 <- seq(1,20, by = 3))
# vector3 1~10까지 3간격으로 연속된 정수가 3회 반복되도록 만드시오
(vector3 <- rep(seq(1, 10, by = 3), each = 3))
# vector4 vector2,3 모두 포함되는 벡터
(vector4 <- rep(seq(1, 20, by = 3), each= 3))
# 25~-15까지 5간격으로벡터생성 -seq()
(vector5 <- seq(25,-15, by = -5))
# Vec4 홀수 값만 선택하여 Vec5에할당
(vector5 <- vector4[seq(1, length(vector4), 2)])
# Vec5의 데이터를 문자로 변경
(vector6 <- as.character(vector5))
# Vec5와 vec6을 더하시오
(vector7 <- vector5 + as.numeric(vector6))

# matrix
# 변환과 차원 mapping (고차원에서 저차원으로 차원축소, 특징추출)
# vector와 matrix곱은 변환

# 열이 먼저
# dimnames에 list 사용 이유: 행과 열의 크기가 다르기 때문에
(a=matrix(1:9, nrow=3, ncol=3, dimnames=list(c("X","Y","Z"), c("A","B","C"))))
class(a)
attributes(a) 
dim(a)

x=matrix(1:9, nrow=3, byrow=TRUE)
colnames(x)
colnames(x)<-c("C1","C2","C3")

(x=matrix(1:9, nrow=3, byrow=TRUE))
x[-1,] # 1행 제외, 모든 열
a = x[1,]
class(a)
mode(a)

# 데이터는 열 방향으로 저장됨
x[1:4] # 1차원으로 간주함
x[c(3,5,7)]

# matrix index 세로, 가로
x[c(TRUE,FALSE,TRUE), c(TRUE,TRUE,FALSE)]

mode(x[x%%2==0])

x <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
print(x)
x[x<5]<-0
t(x)
cbind(x, c(1,2,3))
rbind(x, c(1,2,3))
x <- x[1:2, ]

# 직교행렬
mdat <- matrix(seq(20,4,-2), nrow = 3, ncol = 3, byrow = TRUE, dimnames = list(c("a", "b", "c"), c("x", "y", "z")))
nrow(mdat)
rowMeans(mdat)
rowSums(mdat)
colMeans(mdat)
colSums(mdat)

# 대각 성분
diag(mdat)
diag(20,12,4)

# 연립방정식의 해 구하기
# 2X + 3Y = 5,
# 3X + 5Y = 6
# 

(c = c(5,6))
mdat <- matrix(c(2,3,3,5), nrow = 2, ncol = 2, byrow=TRUE)
solve(mdat, c)

# data.frame
# 열로 접근할 때는 $와 열이름으로 접근
# 실시간으로 열 추가
# head, tail
# 데이터 입출력
# 문자형이면 factor형으로 전환해서 읽어들임

x<-c(10,20,30,40)
y<-c(6,7,8,9)
data<-data.frame(길이=x, 높이=y)
str(data)
data$길이
head(data[c("길이","높이")], 2)

d.f <-data.frame()
d.f<-edit(d.f)

L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
(d <- data.frame(x = 1, y = 1:10, fac = fac))

# 데이터 프레임의 인덱싱 결과는 데이터 프레임
# [[]]은 contents이기 때문에 문자열

# 다음을 데이터 프레임에 입력하시오
#     영어점수 등급
# 퀴즈 67 'C'
# 중간 92 'A'
# 기말 89 'B'
df<-data.frame("영어점수" = c(67,92,89), "등급"=c('C','A','B'))
rnames<- c("퀴즈", "중간", "기말")
rownames(df)<-rnames

# 수학점수를 50, 100, 80점으로 입력하시오
수학점수<-c(50,100,80)
df1<-cbind(df, 수학점수)

# 보충이라는 이름으로 dataframe을 만들어 rbind시키시오
# 영어점수 90, 등급 'A', 수학점수 100
보충<-data.frame("영어점수"=90, "등급"="A", "수학점수"=100)
df2<-rbind(df1, 보충)
rownames(df2)[4]<-c("보충")

# 열별 합계를 내시오
합계<-data.frame("영어점수"=sum(df2[1]),"등급"=NA, "수학점수"=sum(df2[3]))
df3<-rbind(df2, 합계)
rownames(df3)[5]<-c("합계")

# 영어 점수 기말을 88점으로 수정하시오
(df3[3,1]<-88)