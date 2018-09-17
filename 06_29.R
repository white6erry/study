# 문자열 처리함수: base, stringr, stringi(광범위)
# Meta Character
# .: 한문자
# *: 0 or more
# +: 1 or more
# ?: 0 or 1
# |: or
# \\d: 숫자, \\D: 숫자이외, \\w: 문자, \\W: 문자 이외
# 

empty_str=""
class(empty_str)
length(empty_str)

example=character(0)
example
length(example)
example[1]="first"
example[2]="second"
example[1]
example[2]

(PI=paste("pi의 값은 = ",pi))
c(1:5, TRUE, pi, "text", FALSE)
rbind(1:5, letters[1:5])
head(USArrests)
(states= rownames(USArrests))
substr(x=states, start=1,stop=4)

# 축약
(states2 = abbreviate(states))
names(states)=NULL
states2
abbreviate(states, minlength=5)
state_chars=nchar(states)
states[which(state_chars==max(state_chars))]

# ""제거 후 출력
noquote()

# 문자열 사이에 문자 넣기
cat(1:10, sep = "-")
format(1234567890, big.mark = ",")
sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%e", pi)
sprintf("%-10f",pi)

# ()들을 문자열로 만듬
toString()

# nchar 문자의 갯수
nchar("How many characters") #19

# length 문자열 갯수 
length("How many characters") #1

# 문자열에서 소문자 a를 대문자 A로 변경
chartr("a","A", "This is a boaring string")

# 
x = c("may","tree","force")
substr(x,2,2) <- "#"
x

# 집합 연산
union #합
intersect # 교

# 집합 비교
setequal #순서가 달라도 원소값이 같으면 TRUE
identical #순서가 다르면 FALSE

# 문자열 패턴 검색
grep(pattern="k", x=states)
class(grep(pattern="k", x=states))

# 문자열 대체 
# sub: 처음 한개만 변환
# gsub: 문자열 전체 변환
sub("\\.", "", "Peace.Love")
gsub("\\d", "_", "the dandelion war 2010")

# 횟수 조절
people = c("rori", "emilia", "matteo", "mehmet")
grep(pattern="m?", people,value=TRUE, perl=FALSE)

# 소수점을 제거하시오
data<-c("12.578234556", "12.34534645", "12.56756756")
gsub("\\.","",data)

# str_sub: 위치로 
# str_extract, str_extract_all: 표현식으로
lorem="Lorem lpsum"
str_sub(lorem, start=1,end=5)
str_dup(lorem, 1:3)
str_dup(lorem, 3)

# 해당하는 문자열을 가지고 있는지 확인한뒤 TRUE FALSE
fruit <- c("apple", "banana", "pear", "pineapple")
str_detect(fruit, "a")

# 알파벳만 추출하시오
chr5<-"123ab456"
str_extract_all(chr5, "[a-z]")

# 정규표현식을 이용하여 전화번호를 추출하시오
chr6<-"abdbsdsa 010-1234-5678 dasdasd"
str_extract(chr6, "[0-9]{3}-[:digit:]{4}-\\d{4}")

# 모음부분을 -로 바꾸시오
fruits<-c("one apple", "two pears", "three bananas")
gsub("[aeiou]","-",fruits)
# str_replace_all함수 이용해도 됨

# 문자열 나누기
fruits<-c("apples and oranges and pears and bananas", "pineapples and mangos and guavas")
str_split(fruits, " and ")

# 일자별 수입중에서 "3000원", "4500원", "2500원" 같은 형태로 출력
shoppinglist<-c("2017-10-19 수입3000원", "2017-10-20 수입4500원", "2017-10-21 수입2500원")
list2<- str_extract(shoppinglist, "[:digit:]*원")

# 수치를 모두 제거하여 "--수입원" 같이 출력하시오
list3<-gsub("수입\\d*원", "--수입원", list2)

# -를 모두 /로 치환하시오
list4<-str_replace_all(list3, "-", "/")

# 날짜만 모두 추출하시오
list5<- str_extract(shoppinglist, "[:digit:]{4}-\\d{2}-[0-9]{2}")

# 데이터를 "수입3000원" "수입4500원"형식으로 출력하시오
list6<- str_extract(shoppinglist, "수입[:digit:]*원")

library(dplyr)
dataset<-tbl_df(read.csv(file.choose(), stringsAsFactors=F, header=TRUE))
str(dataset)
dim(dataset)
names(dataset)
x<-dataset$gender
y<-dataset$price
plot(x,y)
dataset["gender"]
# NA값이 존재하면 덧셈하지 못하기 때문에 NA값 제거
sum(dataset$price, na.rm=T)
# NA값 제거
na.omit(dataset$price)
x<-dataset$price
# NA값이 있어도 0으로 계산하도록
dataset$price2=ifelse(!is.na(x), x, 0)
dataset$price3=ifelse(!is.na(x), x, round(mean(x, na.rm=TRUE), 2))
head(dataset[c('price', 'price2','price3')],10)

gender<-dataset$gender
dataset<-subset(dataset, gender==1|gender==2)
table(dataset$gender)

dataset2<- subset(dataset, price>=2&price<=10)
stem(dataset2$price)

dataset2<-subset(dataset2, age>=20&age<=69)
boxplot(dataset2$age)

dataset2$resident2[dataset2$resident == 1] <-'1.서울특별시'
dataset2$resident2[dataset2$resident == 2] <-'2.인천광역시'
dataset2$resident2[dataset2$resident == 3] <-'3.대전광역시'
dataset2$resident2[dataset2$resident == 4] <-'4.대구광역시'
dataset2$resident2[dataset2$resident == 5] <-'5.시구군'
dataset2[c("resident","resident2")]

dataset2$job2[dataset2$job==1]<-'공무원'
dataset2$job2[dataset2$job==2]<-'회사원'
dataset2$job2[dataset2$job==3]<-'개인사업'

dataset2$survey[dataset2$survey==1]<-'매우만족'
dataset2$survey[dataset2$survey==2]<-'만족'
dataset2$survey[dataset2$survey==3]<-'보통'
dataset2$survey[dataset2$survey==4]<-'불만족'
dataset2$survey[dataset2$survey==5]<-'매우 불만족'

resident_gender<-table(dataset2$resident2, dataset2$gender)
gender_resident<-table(dataset2$gender, dataset2$resident2)

head(dataset2)

# weatherAUS를 로딩하여 다음과정을 처리하시오
dataset<-tbl_df(read.csv(file.choose(), stringsAsFactors=F, header=TRUE))

# 1. NA수를 세고 처리하시오
sum(is.na(dataset))
dataset<-na.omit(dataset)

# 2. 이상치를 확인하고 처리하시오
#  1-숫자와 문자형 데이터로 분리
#  2-키를 주고 분리
#  3-이상치 처리
#  4-pairs
#  5-숫자와 문자데이터 결합
dataset$key=1:nrow(dataset)
names(dataset[25])
data_int_field<-names(select_if(dataset, is.numeric))
ndata=dataset[,c(3,4,5,6,7,9,12,13,14,15,16,17,18,19,20,21,23)]
sdata=dataset[,c(1, 2, 8, 10, 11, 22,24)]


par(mfrow=c(4,4))
boxplot(x=dataset$MinTemp)$stats
boxplot(x=dataset$MaxTemp)$stats
boxplot(x=dataset$Rainfall)$stats
boxplot(x=dataset$Evaporation)$stats
boxplot(x=dataset$Rainfall)$stats
boxplot(x=dataset$WindGustSpeed)$stats
boxplot(x=dataset$Sunshine)$stats
boxplot(x=dataset$WindSpeed9am)$stats
boxplot(x=dataset$WindSpeed3pm)$stats
boxplot(x=dataset$Humidity9am)$stats
boxplot(x=dataset$Humidity3pm)$stats
boxplot(x=dataset$Pressure9am)$stats
boxplot(x=dataset$Pressure3pm)$stats
boxplot(x=dataset$Cloud9am)$stats
boxplot(x=dataset$Cloud3pm)$stats
boxplot(x=dataset$Temp9am)$stats
boxplot(x=dataset$Temp3pm)$stats
boxplot(x=dataset$RISK_MM)$stats

# 3. 데이터를 7:3으로 샘플링하시오


# 4. 위의 데이터에서 각자 3가지 정보를 찾아내고 시각화하시오



library(lattice)
library(dplyr)
library(ggplot2)
weather = read.csv(file.choose())
length(weather)
nrow(weather)
str(weather)
head(weather)
summary(weather)

sum(is.na(weather))
weather <- na.omit(weather)
sum(is.na(weather))

weather$key=1: nrow(weather) #id : primary key 
names(weather[25])
(data_int_field<- names(select_if(weather, is.numeric)))

is.numeric(weather[1,2])
outdata <- weather[,c(-1,-2,-8,-10,-11,-22,-24)] #숫자인 데이터 : key를 다 양쪽에 저장
str(weather)
nonnumericdata<-weather[,c(1,2,8,11,12,22,24,25)]
str(nonnumericdata)
str(outdata)

for(i in 1:(ncol(outdata)-1)){ #열별로 이상치 제거
  uppercut = fivenum(outdata[,i])[4]+1.5*IQR(outdata[,i])
  lowercut = fivenum(outdata[,i])[2]-1.5*IQR(outdata[,i])
  out <- filter(outdata, outdata[,i]<= uppercut, outdata[,i]>=lowercut) #14228
}

str(out) #이상치가 제거된 값
pairs(out, main="weatherAUS data") #숫자간의 상관관계를 확인 _ 오래걸림
weather_clean <- merge(nonnumericdata,out, by="key")
set.seed(1) #오래걸림
          #(1~30000)
idx= sample(1:nrow(out),0.7*nrow(out)) #machine learning 7:3 7은 training, 3:test 
  #과적합
training = weather_clean[idx,] #열은 같고 행만 7:3으로 분리되어 저
testing = weather_clean[-idx,]
str(training)
str(testing)
library(lattice)
densityplot(~MinTemp | training, data=training, plot.points=T, auto.key = T)

plot(x=training$Temp9am, y=training$MinTemp)
plot(x=training$, y=training$)
plot(x=training$, y=training$)