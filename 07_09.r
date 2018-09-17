# K-means

#install.packages("proxy")
library(proxy)
x <- matrix(1:16, nrow=4) ; x
(dist <- dist(x, method="euclidean"))
(dist <- dist(x, method="minkowski", p=2)) #지수가 변한다. 2일때는 유클리디안, 1일때는 맨하탄.
(dist <- dist(x, method="binary")) #다른 개수가 몇개냐.
(dist <- dist(x, method="manhattan")) 
(simil <- simil(x, method="manhattan")) #유사도

#Hierarchical Clustering(탐색적 군집분석)
idx = sample(1:dim(iris)[1], 40)
irisSample = iris[idx,]
species <- irisSample$Species
irisSample$Species = NULL
hc = hclust(dist(irisSample), method="ave") #dist를 구하고 난 다음 hclust를 시행. #ave는 평균법.
plot(hc, hang = -1, labels=iris$Species[idx])

rect.hclust(hc, k=3)
(groups = cutree(hc, k=3)) #출력된 데이터에 대한 군집에 번호 labeling. 
table(groups, species) #두개

####
set.seed(1)
iris2 <- iris ; head(iris2)
(kmeans.result <- kmeans(iris2, 3, trace=T))
table(iris$Species, kmeans.result$cluster)
plot(iris2[c("Sepal.Length","Sepal.Width")], col=kmeans.result$cluster)
#완료후 중심점 보유
points(kmeans.result$centers[, c("Sepal.Length","Sepal.Width")], col = 1:3, pch=8, cex=2)
print(kmeans.result$totss) #total sum of square(거리값의 합)
print(kmeans.result$withinss) #응집도
print(kmeans.result$betweenss) #군집간 거리의 합계 

library(caret)
set.seed(123)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
class(inTrain)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
#z점수 정규화, (z점수 = 관측치 - 평균값/ 표준편차)
training.data <- scale(training[-5]) #변수의 크기값을 고려하기 위해 정규화
summary(training.data)
iris.kmeans <- kmeans(training.data[,-5], centers = 3, iter.max=10000)
training$cluster <- as.factor(iris.kmeans$cluster)
class(iris.kmeans$cluster)
length(iris.kmeans$cluster)
iris.kmeans$cluster ==3
par(mfrow=c(2,2))
qplot(Petal.Width, Petal.Length, colour = cluster, data= training)
qplot(Sepal.Width, Petal.Length, colour = cluster, data= training)
qplot(Sepal.Width, Sepal.Length, colour = cluster, data= training)
qplot(Petal.Length, Sepal.Width, colour = cluster, data= training)
table(training$Species, training$cluster)

#군집수를 알려주는 함수
#install.packages("NbClust")
library(NbClust)
nc <- NbClust(training.data, min.nc=2, max.nc = 15, method="kmeans") #최대 최소 클러스터수 지정
#distance method와 clustering method에 따른 최적 클러스터를 보여줌.
nc$Best.n[1,]
nc$Best.n[2,]
table(nc$Best.n[1,])
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Cluster의 수", ylab="Criteria 수",
        main="클러스터 갯수 결정 그래프")

# sum of square가 중심점으로부터 거리의 제곱값.
wssplot <- function(data, nc=15, seed=1234){ #최대 클러스터수를 15개까지.
  wss <- (nrow(data-1))*sum(apply(data,2,var))
          for(i in 2:nc) {
            set.seed(seed)
            wss[i] <- sum(kmeans(data, centers=i)$withinss)}
          plot(1:nc, wss, type="b", xlab="Cluster 수", ylab="그룹 내 sum of squares")}
wssplot(training.data)  

#
teens <- read.csv("C:\\Users\\acorn\\Downloads\\snsdata.csv")
set.seed(100)
str(teens)
table(teens$gender)
table(teens$gender, useNA="ifany")
summary(teens$age)

teens$age<-ifelse(teens$age >=13 & teens$age < 20, teens$age, NA)
summary(teens$age)

table(teens$gender, useNA = "ifany")
mean(teens$age, na.rm=T)
(avg <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm=T)))
teens$age <- ifelse(is.na(teens$age), avg, teens$age)
teens$age[1:6]
summary(teens$age)

interests <- teens[5:40]
summary(interests)
interests_n <- data.frame(lapply(interests, scale))
summary(interests_n)
# 청소년 성향을 5개 그룹으로 분류
teen_clusters<-kmeans(interests_n, 5)
names(teen_clusters)
teen_clusters$cluster[1:5]

teen_clusters$size
table(teen_clusters$cluster)
# 군집별 특성에 따라 군집 이름을 결정해 보시오
# 성장물 코디디에서 고등학생을 5가지 그룹으로 분류
# 범죄성향, 운동성향, 외모지향, 무기력, 브레인 5가지 그룹으로 cluster명을 결정
teen_clusters$centers

teens$cluster <- teen_clusters$cluster
head(teens)
class(teens)
head(teens[, c("cluster", "gender", "age", "friends", "cute")])
# 클러스터별 나이의 합계
aggregate(data = teens, age ~ cluster, mean)
# 군집별 여성비율
aggregate(data = teens, gender == 'F' ~ cluster, mean)

aggregate(data = teens, softball+volleyball+hair+dress~gender=='F', mean)
aggregate(data = teens, softball~gender == 'F', mean)
aggregate(data = teens, hair ~ gender == 'M', mean)
aggregate(data = teens, sex+die+drunk~bible > 0, mean)

# diamonds 데이터의 price, carat, depth, table 변수만을 대상으로 탐색적 및 확인적
# 근접분석을 실시하시오

library(ggplot)
t<-sample(1:nrow(diamonds), 1000)
test<-diamonds[t,]
mydia<-test[c("price", "carat", "depth", "table")]
result<-hclust(dist(mydia), method="average")
plot(result, hang=-1)
result2<-kmeans(mydia, 3)
mydia$cluster<-result2$cluster
head(mydia)
plot(mydia[,-5])
cor(mydia[,-5], method="pearson")
library(corrgram)
corrgram(mydia[,-5])
corrgram(mydia[,-5], upper.panel=panel.conf)

plot(mydia$carat, mydia$price)
plot(mydia$carat, mydia$price, col=mydia$cluster)
points(result2$centers[,c("carat", "price")], col=c(3,1,2), pch=8, cex=5)

hc<-hclust(dist(mtcars))
plot(hc)

op = par(mfrow=c(2,1))
hcd<-as.dendrogram(hc)
plot(cut(hcd, h = 75)$upper)
plot(cut(hcd, h=75)$lower[[2]])
par(op)

#
install.packages("ape")
library(ape)
plot(as.phylo(hc), cex=0.9, label.offset=1)
plot(as.phylo(hc), type="fan")
install.packages("ggdendro")
library(ggdendro)
ggdendrogram(hc)
ggdendrogram(hc, rotate=TRUE, size=4, theme_dendro=FALSE, color= "tomato")

#
seed=read.table("C:\\Users\\acorn\\Downloads\\seeds_dataset.txt")
str(seed)
colnames(seed) = c("area", "perimeter", "compactness", "length", "width", "asymmetry", "groovelength")
seed=seed[,1:7]
seed_sc <-scale(seed)
seed.dist=dist(seed)
seed.hclust=hclust(seed.dist, method="ward")
plot(seed.hclust)
seed.3clust = cutree(seed.hclust, k=3)
seed[seed.3clust==3, ]

#
library(fpc)
plotcluster(seed, seed.3clust)

nc <-NbClust(seed_sc, min.nc=2, max.nc=9, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]))
wss<-(nrow(seed)-1)*sum(apply(seed, 2, var))
for( i in 2:12) wss[i]<-sum(kmeans(seed_sc, centers=i)$withinss)
plot(1:12, wss, type="b")
fit<-kmeans(seed_sc, 3)
table(fit$cluster)

seed$cluster<-fit$cluster
aggregate(data=seed, area~cluster, mean)
aggregate(data=seed, asymmetry~cluster, mean)
seed[fit$cluster==1]
aggregate(seed, by=list(fit$cluster), FUN=mean)
fit$centers
MASS::parcoord(seed, col=seed$cluster, var.label=TRUE, lwd=2)

#
library(ggmap)
library(ggplot2)
gc<-geocode("seoul city hall, korea", source="google")
center<-as.numeric(gc)
center
lat<-center[2]
lon<-center[1]
lat; lon

univ <- read.csv("C:\\Users\\acorn\\Downloads\\university.csv")
str(univ)
distance<-sqrt((lat-univ$LAT)^2 + (lon-univ$LON)^2)
min(distance)
close<-which(distance == min(distance))
univ[close, ]
seoul<-list("seoulcity", lat, lon)
univ<-rbind(univ, seoul)

kor<-get_map("seoul", zoom=11, maptype="watercolor")
ggmap(kor)+geom_point(data=univ, aes(x=LON, y=LAT, color=factor(학교명)),size=3)

# dbscan
# eps
# 

iris2 = iris[-5]
ds = dbscan(iris2, eps=0.42, MinPts=5)

table(ds$cluster, iris$Species)
plot(ds, iris2)
plot(ds, iris2[c(1,4)])
plotcluster(iris2, ds$cluster)
set.seed(435)
idx = sample(1:nrow(iris), 10)
newData = iris[idx,-5]
newData = newData+matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)
myPred=predict(ds, iris2, newData)
plot(iris2[c(1,4)], col=1+ds$cluster)
points(newData[c(1,4)], pch="*", col = 1+myPred, cex=3)

# KNN
library(class)
set.seed(100)
normalize<-function(x){
	return ((x-min(x))/(max(x)-min(x)))
}
iris_normal<-apply(iris[1:4],2,normalize)
idx=sample(1:nrow(iris_normal), 0.7*nrow(iris_normal))
training=iris[idx,]
test=iris[-idx,]
k<-5:20
acc<-numeric()
cnt<-1
for(i in k){
	pred<-knn(training[,-5], testing[,-5], training$Species, k=i, prob=TRUE)
	t<-table(pred, testing$Species)
	acc[i-4] <- (t[1,1]+t[2,2]+t[3,3])/sum(t)
}
dacc<-data.frame(no=k, acc=acc)
optk<-with(dacc, no[which.max(acc)])
model.knn=knn(training[,-5], testing[,-5], training$Species, k=optk, prob=TRUE)
model.knn
summary(model.knn)
tb<-table(model.knn, testing$Species)
paste(round(sum(diag(tb))/sum(tb)*100), "%")

# knn분석
wdbc<-read.csv("C:\\Users\\acorn\\Downloads\\wdbc_data.csv")
wdbc <- wdbc[-1]
idx=sample(1:nrow(wdbc), 0.7*nrow(wdbc))
train<-wdbc[idx,]
test<-wdbc[-idx,]

wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B", "M"))
prop.table(table(wdbc$diagnosis)) * 100
normalize <- function(x){
                return ((x - min(x)) / (max(x) - min(x)))
}
wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize))
set.seed(415) # 시드값 적용 - 동일한 랜덤값 제공 
idx = sample(1:nrow(wdbc_x), 0.7*nrow(wdbc_x))
wdbc_train = wdbc_x[idx, ] # 훈련 데이터 
wdbc_test = wdbc_x[-idx, ] # 검정 데이터 
dim(wdbc_train) # [1] 398  30
wdbc_train_y <- wdbc[idx, 1] # 훈련 데이터의 diagnosis 칼럼 
wdbc_test_y <- wdbc[-idx, 1] # 검정 데이터의 diagnosis 칼럼 
k = sqrt(398)
wdbc_pred <- knn(wdbc_train, wdbc_test, wdbc_train_y, k=19)
wdbc_pred # 분류예측모델 
result <- numeric()
k = 5:25
for(i in k ){
    wdbc_pred <- knn(wdbc_train, wdbc_test, wdbc_train_y, k=i)
    t <- table(wdbc_pred, wdbc_test_y)
    result[i-4] <- (t[1,1]+t[2,2])/sum(t)
}
result
sort(result, decreasing = T)
which(result==max(result))
