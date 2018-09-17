# PCA, Factor analysis, MDS

a<-c(1,3,2)
t(a)
7*a
b<-(2,8,9)
a+b

# vector 연산
v1<-c(1,1)
v2<-c(-1, 1)
(v1*v2) # 요소별 곱
sum(v1*v2)
v1%*%v2 # 내적

# matrix 연산

# dist
x<-matrix(rnorm(100), nrow=5)
dist(x)# 벡터들의 거리
dist(x, diag=TRUE)
dist(x, upper=TRUE)

# PCA
# scree plot: 주성분 수를 확인
# loadings: 주성분에 기여하는 정도를 확인하는 것
# biplot: 주성분에 기여하는 정도를 그래프로 확인

a<-c(4.0, 4.2, 3.9, 4.3, 4.1)
b<-c(2.0, 2.1, 2.0, 2.1, 2.2)
c<-c(0.60, 0.59, 0.58, 0.62, 0.63)
mat<-matrix(c(a,b,c), nrow=5, byrow=F)
avr<-colMeans(mat)
acha<-a-avr[1]
bcha<-b-avr[2]
ccha<-c-avr[3]
aa_var<-sum(acha*acha)/(length(a)-1)
ab_var<-sum(acha*bcha)/(length(a)-1)
cov(mat)

# eigen: 고유값 분해 함수
eigvector<-eigen(cor(mat))
names(eigvector)
eigvector$values
eigvector$vectors

# 
library(dplyr)
weather=read.csv("C:\\Users\\acorn\\Downloads\\weather.csv")
weather<-na.omit(weather)
weather$key=1:nrow(weather)
outdata=select_if(weather, is.numeric)
str(weather)
str(outdata)

for (i in 1:(ncol(outdata)-1)){
	uppercut=fivenum(outdata[,i])[4]+1.5*IQR(outdata[,i])
	lowercut=fivenum(outdata[,i])[2]+1.5*IQR(outdata[,i])
	out<-filter(outdata, outdata[,i]<=uppercut, outdata[,i]>=lowercut)
}

res_cor<-cor(out)
res_eig<-eigen(res_cor)
res_eig_t=t(res_eig$vectors)
res_eig$vectors %*% res_eig_t

mat.pca<-princomp(mat, cor=T, scores=T)
mat.pca
summary(mat.pca)
eigval=mat.pca$sdev^2
par(mfrow=c(1,2))
screeplot(mat.pca)
screeplot(mat.pca, type="line", pch=19, main="Scree Plot")

install.packages("factoextra")
library(factoextra)
par(mfrow=c(1,2))
fviz_eig(mat.pca)
fviz_pca_ind(mat.pca, col.ind="cos2", gradient.cols=rainbow(3), repel=TRUE)

iris.cntn<-iris[,-5]
iris.cat<-iris[,5]
library(caret)

######## iris를 이용한 주성분 분석

pcaCharts <- function(x){
  x.var <- x$sdev^2
  x.pvar <- x.var/sum(x.var)
  print("분산 점유율:")
  print(x.pvar)
  
  par(mfrow = c(2,2))
  plot(x.pvar, xlab = "주성분", ylab = "분산 점유율", ylim = c(0,1), type='b')
  plot(cumsum(x.par), xlab = "주성분", ylab = "분산 점유율", ylim = c(0,1), type='b')
  screeplot(x)
  screeplot(x,type="l")
  par(mfrow=c(1,1))
}

head(iris)
str(iris)
iris.cntn <- iris[,-5]
iris.cat <- iris[,5]

install.packages("e1071")
library(e1071)
library(caret) # BoxCox : 비정규분포를 정규분포로 변환하는 함수
# 변환모델
iris.trans <- preProcess(x= iris.cntn, method = c( "BoxCox" ,"center","scale"))

iris.trans
iris.preproc <- predict(iris.trans, newdata = iris.cntn)
iris.pca <- prcomp(iris.preproc, center = FALSE)
iris.pca

summary(iris.pca)
pcaCharts(iris.pca)

# 문제 csv 데이터를 이용하여 주성분 분석
# 제1, 2 주성분에 대한 설명
# V1: 총 자본순이익율
# V2: 자기자본순이익율
# V3: 자기자본비율
# V4: 부채비율
# V5: 자기자본회전율

secu_com_finance_2007<-read.csv("C:\\Users\\acorn\\Downloads\\secu_com_finance_2007.csv", header=TRUE, stringsAsFactors=FALSE)
secu_com_finance_2007<-transform(secu_com_finance_2007, V1_s=scale(V1), V2_s=scale(V2), V3_s=scale(V3), V4_s=scale(V4), V5_s=scale(V5))
secu_com_finance_2007<-transform(secu_com_finance_2007, V4_s2=max(V4_s)-V4_s)
secu_com_finance_2007_2<-secu_com_finance_2007[, c("company", "V1_s", "V2_s", "V3_s", "V4_s","V5_s")]
secu_procomp<-prcomp(secu_com_finance_2007_2[,c(2:6)], scale=FALSE)
biplot(secu_procomp)

# 제 1주성분
# 자기 자본회전율과 자기자본 순이익율이 커지면 작아지고 자기자본비율과 부채비율이 커지면
# 커지는 특징

# 제 2주성분
# 총자본 순이율과 자기자본 순이익율이 커지면 작아지는 특징
# 왼쪽 아래기업이 양호한 기업이다.

# Factor 분석
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4) 
m1<-cbind(v1, v2, v3, v4, v5, v6)
cor(m1)
factanal(m1, factors=3)
factanal(m1, factors=3, rotation="promax")
factanal(~v1+v2+v3+v4+v5+v6, factors=3, scores="Bartlett")$scores

s1 <- c(1, 2, 1, 2, 3, 4, 2, 3, 4, 5)
s2 <- c(1, 3, 1, 2, 3, 4, 2, 4, 3, 4)
s3 <- c(2, 3, 2, 3, 2, 3, 5, 3, 4, 2)
s4 <- c(2, 4, 2, 3, 2, 3, 5, 3, 4, 1)
s5 <- c(4, 5, 4, 5, 2, 1, 5, 2, 4, 3)
s6 <- c(4, 3, 4, 4, 2, 1, 5, 2, 4, 2) 
name<-1:10
subject<-data.frame(s1, s2, s3, s4, s5, s6)
summary(subject)
pc<-prcomp(subject)
plot(pc)
en<-eigen(cor(subject))
names(en)
en$values
plot(en$values, type="o")
cor(subject)

result2<-factanal(subject, factors=3, rotation="varimax", scores="regression")
result2$loadings
print(result2, digits=2, cutoff=0.5)
plot(result2$scores[,c(1:2)], main="Factor1과 Factor2의 요인점수 행렬")
plot(result2$scores[,c(1:2)], main="Factor1과 Factor2의 요인점수 행렬")
text(result2$scores[,1], result2$scores[,2], labels=name, cex=0.7, pos=3, col="blue")
points(result2$loadings[,c(1:2)], pch=19, col="red")
text(result2$loadings[,1], result2$loadings[,2], labels=rownames(result2$loadings), cex=0.8, pos=3, col="red")

# FA 실시
library(dplyr)
attach(secu_com_finance_2007)
finance<-transform(secu_com_finance_2007, V1_s=scale(V1), V2_s=scale(V2), V3_s=scale(V3), V4_s=scale(V4), V5_s=scale(V5))
detach(secu_com_finance_2007)
finance<-transform(finance, V4_s=max(V4_s)-V4_s)
finance2<-finance[,c("company", "V1_s", "V2_s", "V3_s", "V4_s","V5_s")]
round(cor(finance2[,-1]),digits=3)
plot(finance2[,-1])
pcaloading<-prcomp(cor(finance2[,c(2:6)]))
plot(prcomp(finance2[,c(2:6)], type="l", sub="Scree Plot")
secu_fact<-factanal(finance2[2:6], factors=2, rotation="varimax", scores="regression")
print(secu_fact)
print(secu_fact$loadings, cutoff=0)
# scores: 주요 소축으로 재해석된 좌표값
plot(secu_fact$scores, main="요인분석 2요인 Biplot")
text(secu_fact$scores[,1], secu_fact$scores[,2], labels=finance$company, cex=0.7, pos=3, col="blue")
points(secu_fact$loadings, pch=19, col="red")
text(secu_fact$loadings[,1], secu_fact$loadings[,2], labels=rownames(secu_fact$loadings), cex=0.8, pos=3, col="red")

segments(0,0,secu_fact$loadings[1,1],secu_fact$loadings[1,2])
segments(0,0,secu_fact$loadings[2,1],secu_fact$loadings[2,2])
segments(0,0,secu_fact$loadings[3,1],secu_fact$loadings[3,2])
segments(0,0,secu_fact$loadings[4,1],secu_fact$loadings[4,2])
segments(0,0,secu_fact$loadings[5,1],secu_fact$loadings[5,2])

# MDS
mydata<-select(secu_com_finance_2007, V1,V2,V3,V4,V5)
d<-dist(mydata)
fit<-cmdscale(d,eig=TRUE,k=2)
x<-fit$points[,1]
y<-fit$points[,2]
plot(x,y,xlab="1",ylab="2", type="n", col="red")
text(x,y,labels=row.names(mydata),cex=.9)
text(x,y,labels=secu_com_finance_2007$company,cex=.9)

# isometric
install.packages("HSAUR2")
library("MASS")
library("HSAUR2")
data("voting", package="HSAUR2")
voting

voting_mds<-isoMDS(voting)
voting_mds$points
x<-voting_mds$points[,1]
y<-voting_mds$points[,2]
plot(x,y,xlim=range(voting_mds$points[,1])*1.2, type="n")
text(x,y,labels=colnames(voting),cex=0.6)

data("swiss")
library(magrittr)
library(dplyr)
library(ggpubr)
mds<-swiss%>%
dist()%>%
cmdscale()%>%
as_tibble()
colnames(mds)<- c("Dim.1", "Dim.2")
ggscatter(mds, x="Dim.1", y="Dim.2", label=rownames(swiss), size=1, repel=TRUE)
clust<-kmeans(mds,3)$cluster%>%
as.factor()
mds<-mds%>%
mutate(groups=clust)
ggscatter(mds, x="Dim.1", y="Dim.2", label=rownames(swiss), color="groups", palette="jco", size=1, ellipse=TRUE, ellipse.type="convex", repel=TRUE)

install.packages("leaflet")
library(leaflet)

m<-leaflet()%>%
addTiles(m, "서울시 지")%>%
addMarkers(m, lng=126.9779451, lat=37.5662952, popup="서울시청")%>%%>%
addMarkers(m, lng=127.000818, lat=37.557775, popup="동국대학교")
m<-leaflet()
m<-addTiles(m)
m<-addMarkers(m, lng=127.000818, lat=37.557775, popup="동국대학교")
m

library(leaflet)
library(shiny)
r_colors<-rgb(t(col2rgb(colors())/255))
names(r_colors)<-colors()
ui<-fluidPage(leafletOutput("mymap"),p(),actionButton("recalc","New points"))
server <- function(input, output, session){
  points <- eventReactive(input$recalc,{
    cbind(rnorm(5) + 128, rnorm(2)+36)
  }, ignoreNULL = F)
  output$mymap <-renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Watercolor,
                       options = providerTileOptions(noWrap = TRUE)
                       ) %>%
      addMarkers(data=points())
  })
}
shinyApp(ui,server)




