# 연관 분석: 군집분석 이후 그룹의 특성 분석하기 위한 비지도 학습
# 연관분석의 척도: 신뢰도, 지지도, 향상도
# 지지도(support) = P(A∩B)
# 신뢰도(confidence) = P(B|A)
# 향상도(lift) =𝑷P(B|A)/P(B)
# 	lift = 1 관련없음
# 	lift > 1 클수록 더 강한 연관
# 	lift < 1 연관 작아짐

library(arules)
data(Adult)
str(Adult)
attributes(Adult)
adult<-as(Adult, "data.frame")
str(adult)
summary(adult)

ar<-apriori(Adult, parameter=list(supp=0.1, conf=0.8))
ar1<-apriori(Adult, parameter=list(supp=0.2))
ar2<-apriori(Adult, parameter=list(supp=0.2, conf=0.95))
ar3<-apriori(Adult, parameter=list(supp=0.3, conf=0.95))
ar4<-apriori(Adult, parameter=list(supp=0.35, conf=0.95))
ar5<-apriori(Adult, parameter=list(supp=0.4, conf=0.95))
inspect(head(ar5))
inspect(head(sort(ar5, decreasing=T, by="confidence")))
install.packages("arulesViz")
library(arulesViz)
plot(ar1)

data(Groceries)
Groceries.df<-as(Groceries, "data.frame")
itemFrequencyPlot(Groceries, topN=20, type="absolute")
rules<-apriori(Groceries, parameter=list(supp=0.001, conf=0.8, maxlen=3))
inspect(rules)
plot(rules, method="grouped")
inspect(rules)
rules<-sort(rules, decreasing=T, by="confidence")
inspect(rules)

wmilk<-subset(rules, rhs %in% 'whole milk')
inspect(wmilk)
plot(wmilk, method="graph")

oveg<-subset(rules, rhs %in% 'other vegetables')
inspect(oveg)
plot(oveg, method="graph")
rules<-apriori(Groceries, parameter=list(support=0.005, confidence=0.5))
p<-inspectDT(rules)
htmlwidgets::saveWidget(p, "arules.html", selfcontained=FALSE)
browseURL("arules.html")
p<-plotly_arules(rules)
htmlwidgets::saveWidget(p, "arules.html", selfcontained=FALSE)
browseURL("arules.html")

# 
data("AdultUCI")
#데이터 범주형 변수로 변환
AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100데이터 프레임 factor형 변환)),
  labels = c("young", "middle", "senior", "old"))

AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]],
  c(0,25,40,60,168)),
  labels = c("part-time", "full-time", "over-time", "workaholic"))

AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]],
  c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]),
  Inf)), labels = c("None", "Low", "High"))

AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]],
  c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]),
  Inf)), labels = c("None", "Low", "High"))

AdultUCI[["fnlwgt"]]<-NULL
AdultUCI[["education-num"]]<-NULL
str(AdultUCI)
Adult_new<-as(AdultUCI, "transactions")
basket_rules<-apriori(Adult_new, parameter=list(sup=0.08, conf=0.8, target="rules"))
inspect(basket_rules[1:10])
p<-inspectDT(basket_rules)
htmlwidgets::saveWidget(p, "arules_2.html", selfcontained=FALSE)
browseURL("arules_2.html")

small_income<-subset(basket_rules, subset=lhs %in% "income=large" & lift>1.1)
length(small_income)

# 가족관계 및 교육수준의 소득과의 연관성을 확인하시오
# 주당 일하는 시간과 소득과의 관계를 확인해 보시오
levels(AdultUCI$education)
p<-inspectDT(small_income)
htmlwidgets::saveWidget(p, "small_income_2.html", selfcontained=FALSE)
browseURL("small_income_2.html")
small_income<-subset(basket_rules, subset=lhs %in% "income=small" & lift>1.1)
large_income<-subset(basket_rules, subset=lhs %in% "income=large" & lift>1.2 & rhs %in% "education=Doctorate")
large_income<-subset(basket_rules, subset=lhs %in% "income=large" & lift>1.2 & rhs %in% "education=Bachelors")
income_rules<-apriori(Adult_new ,parameter=list(support=0.04, confidence=0.4, minlen=2, maxlen=14),appearance=list(rhs=c("income=large", "income=small"),default='lhs')) 

# titani3.csv 데이터를 이용하여 사망에 대하여 분석
titan<-read.csv("C:\\Users\\acorn\\Downloads\\데이터\\titanic3.csv")
str(titan)
titan

library(KoNLP)
library(arules)
library(igraph)
library(combinat)
 
#
f <- file("C:\\Users\\acorn\\Downloads\\데이터\\tax.txt", encoding="UTF-8")
fl <- readLines(f)
close(f)

tran <- Map(extractNoun, fl)
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y) {nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)},x)} )
tran <- Filter(function(x){length(x) >= 2}, tran)
names(tran) <- paste("Tr", 1:length(tran), sep="")
wordtran <- as(tran, "transactions")
 
#co-occurance table 
wordtab <- crossTable(wordtran)
 
 
ares <- apriori(wordtran, parameter=list(supp=0.05, conf=0.05))
inspect(ares)
rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ",  USE.NAMES=F)
rulemat <- do.call("rbind", rules)
 
# ares <- apriori(wordtran, parameter=list(supp=0.05, conf=0.05))
# inspect(ares)
# rules <- labels(ares, ruleSep="/", setStart="", setEnd="")
# rules <- sapply(rules, strsplit, "/",  USE.NAMES=F)
# rules <- Filter(function(x){!any(x == "")},rules)
# rulemat <- do.call("rbind", rules)
# rulequality <- quality(ares)
# ruleg <- graph.edgelist(rulemat,directed=F)
 
#plot for important pairs 
ruleg <- graph.edgelist(rulemat[-c(1:16),],directed=F)
plot.igraph(ruleg, vertex.label=V(ruleg)$name, vertex.label.cex=0.5, vertex.size=20, layout=layout.fruchterman.reingold.grid)
 
 
#plot for all pairs
# tranpairs <- sapply(tran, function(x){t(combn(x,2))})
# sapply(tran,function(x){x })
# edgelist  <- do.call("rbind", tranpairs)
# edgelist <- unique(edgelist)
# g <- graph.edgelist(edgelist, directed=F)
# plot(g)

# KoNLP + arules + igraph 를 결합한 관계망 출력
# 결합하여 텍스트 마이닝 후 사회 관계망으로 시각화 
library(KoNLP)
library(arules)
library(igraph)
library(combinat)

f<- file("tax.txt", encoding="UTF-8")
fl <- readLines(f)
close(f)
head(fl, 10)

tran <- Map(extractNoun, fl)
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x) {Filter(function(y)+ {nchar(y) <=4 && nchar(y)>1 && is.hangul(y)}, x)})
tran <- Filter(function(x){length(x)>=2}, tran)

names(tran) <- paste("Tr", 1: length(tran), sep="")
names(tran)
wordtran<-as(tran, "transactions")
wordtab <- crossTable(wordtran)
wordtab
ares <- apriori(wordtran, parameter = list(supp=0.07, conf =0.05))
inspect(ares)
rules <- labels(ares, ruleSep=" ")
rules <- sapply(rules, strsplit, " ", USE.NAMES=F)
rulemat<- do.call("rbind", rules)
ruleg <- graph.edgelist(rulemat[-c(1:11, 28:30),], directed=F)
plot.igraph(ruleg, vertex.label=V(ruleg)$name, # 사회관계망 분석
            vertex.labe.cex=0.5, vertex.size=20)

# 추천 시스템
# sparce matrix
# 특이행렬 분해, 고윳값 분해
# ratingMatrix(가상함수)
# SVD - 특이행렬분해 필요

library(lsa)
library(recommenderlab)

vec1=c(1:12)
vec2=c(12:1)
cosine(vec1, vec2)
rep(.3/6, 6)
m<-matrix(sample(c(NA, 0:5), 100, replace=TRUE, prob=c(.7, rep(.3/6,6))),nrow=10, ncol=10, dimnames=list(user=paste('u', 1:10, sep=''),item=paste('i', 1:10,sep='')))
m
r<-as(m, "realRatingMatrix")
r
hist(getRatings(r), breaks="FD")
image(r[1:5,1:5])
as(r, "matrix")
b<-binarize(r, minRating=4)
b
as(b, "matrix")

data(MSWeb)
as(MSWeb, "matrix")
dissimilarity(MSWeb[1:5], method="jaccard")
similarity(MSWeb[1:3], method="jaccard", which="items")
MSWeb10<-sample(MSWeb[rowCounts(MSWeb)>10, ],100)
es<-evaluationScheme(MSWeb10, method="cross-validation", k=10, given=3)
ev<-evaluate(es, "POPULAR", n=c(1,3,5,10))
avg(ev)
plot(ev, annotate=TRUE)

data(MovieLense)
MovieLense100<-MovieLense[rowCounts(MovieLense)>100,]
train<-MovieLense100[1:100]
test<-MovieLense100[101:103]
recom<-HybridRecommender(
	Recommender(train, method="POPULAR"),
	Recommender(train, method="RANDOM"),
	Recommender(train, method="RERECOMMEND"),
	weights=c(.6, .1, .3))
	
# 평가





































