# 텍스트 마이닝
library(stringr)
str_extract("abcd12aaa33", "[0-9]{2}")
str_extract_all("abcd12aaa33", "[0-9]{2}")
str<-'hongkildong35lee45kang55안창호25'
result<-str_extract_all(str, '[a-z]{3}')
(result<- str_extract_all(str, '[0-9]{2}'))
(result<- str_extract_all(str, '[가-히]{3}'))
x <- "R totorial"
gsub("ot","ut",x)
gsub("tot","ut",x,ignore.case = T)
(y <- gsub("[[:lower:]]","-",x))

library(tm)
data("crude")
tdm <- TermDocumentMatrix(crude)[1:10, 1:20]
Docs(tdm)
nDocs(tdm)
nTerms(tdm)
Terms(tdm)
findAssocs(x, terms, corlimit)
tdm <- TermDocumentMatrix(crude)
findAssocs(tdm, c("oil", "opec", "xyz"), c(0.7, 0.75, 0.1))

#구두점, 숫자, 불용어. 2자 이상 등의 제한을 가하여 만들어진 tdm(termdocumentmatrix)
tdm <- TermDocumentMatrix(crude,
                          control = list(removePunctuation=T,
                                         removeNumbers= T,
                                         stopwords=T, worlLengths=c(2,Inf)))
#메타데이터의 확인과 메타데이터 등록
meta(crude[[1]])
meta(crude[[1]], tag="topics")
meta(crude[[1]], tag="comments") <- "A short comment."

data(crude)
inspect(crude)
crude <- tm_map(crude, removePunctuation)
inspect(crude)
moby <- tm_map(moby, removeWords, stopwords('english'))
crude <- tm_map(crude, removeWords, stopwords('english'))
inspect(crude)
tdm <- TermDocumentMatrix(crude)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word,d$freq, colors=pal2)

(m3<-t(m2))
k<-4
kmres<-kmeans(m3,k)
round(kmres$centers, digit=3)
for(i in 1:k){
	cat(paste("cluster", i, " : ", sep=""))
	s<-sort(kmres$centers[i,], decreasing=T)
	cat(names(s)[1:3], "\n")
}

library(testthat) 
library(KoNLP) 
options(encoding="UTF-8") 
useSejongDic()

extractNoun("더불어민주당 문재인 자유한국당 홍준표 국민의당 안철수
            바른정당 유승민 정의당 심상정 후보는 이날 저마다의 스타일에 맞춰 각양각색의 방법으로 TV토론 준비에 나섰다.")
#9개의 메인태그만 사용
SimplePos09("더불어민주당 문재인 자유한국당 홍준표 국민의당 안철수
            바른정당 유승민 정의당 심상정 후보는 이날 저마다의 스타일에 맞춰 각양각색의 방법으로 TV토론 준비에 나섰다.")
txt <- '미국 싱크탱크 전략국제문제연구소(CSIS)의 빅터 차 한국석좌는 9일(현지시간) 미국의 제45대 대통령으로 당선된 
도널드 트럼프가 전시작전통제권(전작권)을 한국에 조기에 넘길 가능성이 있다고 전망했다.'
extractNoun(txt)
# 사전에 추가 
buildDictionary(ext_dic = c('sejong', 'woorimalsam'),
                user_dic = data.frame(term="전작권", tag='ncn'), # 명사형 
                category_dic_nms=c('political'))
txt <- '미국 싱크탱크 전략국제문제연구소(CSIS)의 빅터 차 한국석좌는 9일(현지시간) 미국의 제45대 대통령으로 당선된 
도널드 트럼프가 전시작전통제권(전작권)을 한국에 조기에 넘길 가능성이 있다고 전망했다.'
extractNoun(txt)

# 감정분석
library(tm)
library(wordcloud)
install.packages("topicmodels")
library(topicmodels)
reviews=read.csv("C:\\Users\\acorn\\Downloads\\데이터\\movie_reviews.csv", stringsAsFactors=F, row.names=1)
str(reviews)
reviews_corpus=Corpus(VectorSource(reviews$content))
tm_corpus<-tm_map(reviews_corpus, content_transformer(tolower))
tm_corpus<-tm_map(tm_corpus, removeNumbers)
tm_corpus<-tm_map(tm_corpus, removePunctuation)
tm_corpus<-tm_map(tm_corpus, removeWords, c("the", "and", stopwords("english")))
tm_corpus<-tm_map(tm_corpus, stripWhitespace)
tm_corpus
tm_corpus<-tm_map(tm_corpus, stemDocument)
inspect(tm_corpus[1])

review_dtm<-DocumentTermMatrix(tm_corpus)
review_dtm
inspect(review_dtm[1,])
review_dtm=removeSparseTerms(review_dtm, 0.95)
inspect(review_dtm[1,1:20])
findFreqTerms(review_dtm,1000)
freq=data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1],max.words=50, colors=brewer.pal(1,"Dark2"))

review_dtm_tfidf<-DocumentTermMatrix(tm_corpus, control, list(weighting=weightTfldf))
review_dtm_tfidf=removeSparseTerms(review_dtm_tfidf,0.95)
review_dtm_tfidf
freq=data.frame(sort(colSums(as.matrix(review_dtm_tfidf)),decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=100, random.order=F, scale=c(5,0.5),colors=brewer.pal(1,"Dark2"))

library(plyr)
library(stringr)
neg_words<-read.table("C:\\Users\\acorn\\Downloads\\데이터\\negative-words.txt", header=F, stringsAsFactors=F)[,1]
pos_words<-read.table("C:\\Users\\acorn\\Downloads\\데이터\\positive-words.txt", header=F, stringsAsFactors=F)[,1]
length(pos_words)

sent<-function(corpus, pos, neg){
	scores = lapply(corpus, function(corpus, neg, pos){
		word.list = str_split(corpus, "\\s+")
		words = unlist(word.list)
		pos.matches = match(words, pos)
		neg.matches = match(words, neg)
		pos.matches = !is.na(pos.matches)
		neg.matches = !is.na(neg.matches)
		score = sum(pos.matches) - sum(neg.matches)
		return (score)
	}, pos, neg)
}
comp<-sent(tm_corpus, pos_words, neg_words)
length(comp)
ll<-unlist(comp)

#영화평을 분석한 결과를 근부정 단어가 포함된 개수에 따라서 긍정 부정으로 구
reviewsent <- data.frame(content = reviews$content, score =ll)
reviewsent$content <- as.character(reviewsent$content)
reviewsent$remark[reviewsent$score >=1] = "긍정"
reviewsent$remark[reviewsent$score ==0] = "중립"
reviewsent$remark[reviewsent$score < 0] = "부정"
(result <- table(reviewsent$remark))

#감정분석 결과 시각화
library(ggplot2)
plot(reviewsent$score, xlab="document",ylab="sent score", main="리뷰별 스코어")
pie(result, main="감정분석 결과", col=c("blue","red", "green"), radius=0.8)
ggplot(reviewsent, aes(reviewsent$score, fill=reviewsent$remark)) + geom_dotplot()
ggplot(reviewsent, aes(reviewsent$score, fill=reviewsent$remark)) + geom_histogram()

# tax.txt
tax<-stri_read_lines("C:\\Users\\acorn\\Downloads\\데이터\\tax.txt")
noun<-sapply(tax, extractNoun, USE.NAMES=F)
noun2<-unlist(noun)
word_count<-table(noun2)
wordcloud(names(word_count), freq = word_count, random.order=F, rot.per=0.1, colors=brewer.pal(8, "Dark2"))

########## 선생님 정답 # 문제 : tax.txt 데이터를 이용하여 wordcloud를 출력하시오
# 원하는 단어 빠져있는 것 확인 => dictionary에 등
# list ,unlist = > vector

library(KoNLP)
library(RColorBrewer)
useSejongDic()
buildDictionary(ext_dic = "sejong", user_dic=data.frame("세액 공제", "ncn"),
                replace_usr_dic= T)

buildDictionary(ext_dic = "sejong", user_dic=data.frame("환급금", "ncn"),
                replace_usr_dic= T)

buildDictionary(ext_dic = "sejong", user_dic=data.frame("자기검증", "ncn"),
                replace_usr_dic= T)

mytext <- file("C:\\Users\\acorn\\Downloads\\데이터\\tax.txt", encoding="UTF-8")
myline <- readLines(mytext)
close(mytext) ; head(myline, 5); tail(myline, 5)
(myword<-sapply(myline, extractNoun, USE.NAMES=F))
result <- unlist(myword) ; head(result, 20)
(result2 <- Filter(function(x){nchar(x)>=2}, result))
(result3 <- Filter(function(x){nchar(x) ==3}, result))
(result2 <- Filter(function(x){nchar(x)>=2 & nchar(x) <=4}, result))

result2 <- gsub("것", "", result2) ; result2 <- gsub("저", "", result2)
result2 <- gsub("", "", result2) ; result2 <- gsub("\\n", "", result2)
result2 <- gsub("\\d+", "", result2) ; result2 <- gsub("\\.", "", result2)
head(result2, 20)

wordcount <- table(result2)
head(sort(wordcount, decreasing = T),20)
palete <- brewer.pal(0, "Set1")

wordcloud(names(wordcount), freq=wordcount,
          scale = c(5,1), rot.per=0.5, min.freq = 4, random.order = F, random.color = T,
          colors = palete)
		  
# 주제어 분석
library(topicmodels)
data("AssociatedPress")
str(AssociatedPress)
ap_lda<-LDA(AssociatedPress, k=2, control=list(seed=1234))
library(tidytext)
ap_topics<-tidy(ap_lda, matrix="beta")
str(ap_topics)
library(dplyr)
library(ggplot2)
	ap_top_terms<-ap_topics %>%
	group_by(topic) %>%
	top_n(10,beta) %>%
	ungroup() %>%
	arrange(topic, -beta)
ap_top_terms %>%
	mutate(term = reorder(term, beta)) %>%
	ggplot(aes(term, beta, fill=factor(topic))) +
	geom_col(show.legend=FALSE) +
	facet_wrap(~ topic, scales="free") +
	coord_flip()

library(tidytext)
sentiments
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

library("janeaustenr")
library(dplyr)
library(stringr)
tidy_books <- austen_books() %>%
	group_by(book) %>%
	mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
		 ignore_case = TRUE)))) %>%
	ungroup() %>%
	unnest_tokens(word, text)
	
nrc_joy <- get_sentiments("nrc") %>% 
	filter(sentiment == "joy")

tidy_books %>%
	filter(book == "Emma") %>%
	inner_join(nrc_joy) %>%
	count(word, sort = TRUE)
  
library(tidyr)
jane_austen_sentiment<-tidy_books %>%
	inner_join(get_sentiments("bing"))%>%
	count(book, index=linenumber %/% 80, sentiment)%>%
	spread(sentiment, n, fill=0) %>%
	mutate(sentiment = positive - negative)
	
library(ggplot2)
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

# 스팸 분류
library(tidyr)
library(stringr)
library(tm)
library(caret)
library(wordcloud)
library(dplyr)
sms_raw <- read.csv("C:\\Users\\acorn\\Downloads\\데이터\\sms_pam_spam.csv")
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
sms_corpus_clean <- sms_corpus %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords()) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

train_index <- createDataPartition(sms_raw$type, p=0.75, list=FALSE)

# 훈련/검증 데이터
sms_raw_train <- sms_raw[train_index,]
sms_raw_test <- sms_raw[-train_index,]

# 훈련/검증 텍스트 코퍼스
sms_corpus_clean_train <- sms_corpus_clean[train_index]
sms_corpus_clean_test <- sms_corpus_clean[-train_index]

# 훈련/검증 문서단어행렬
sms_dtm_train <- sms_dtm[train_index,]
sms_dtm_test <- sms_dtm[-train_index,]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

wordcloud(sms_corpus_clean_train, min.freq=30, random.order=FALSE)
spam<-subset(sms_raw_train, type == "spam")
ham<-subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words=40, scale=c(3,0.5))
wordcloud(ham$text, max.words=40, scale=c(3,0.5))

## 3.2. 모형설계행렬 -------------------------------
sms_dict <- findFreqTerms(sms_dtm_train, lowfreq=5)
sms_train <- DocumentTermMatrix(sms_corpus_clean_train, list(dictionary=sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_clean_test, list(dictionary=sms_dict))

convert_counts <- function(x) {
    x <- ifelse(x > 0, 1, 0)
    x <- factor(x, levels = c(0, 1), labels = c("Absent", "Present"))
}

sms_train <- sms_train %>% apply(MARGIN=2, FUN=convert_counts)
sms_test <- sms_test %>% apply(MARGIN=2, FUN=convert_counts)

sms_train %>% tbl_df %>% 
    sample_n(100) %>% 
    dplyr::select(1:10) %>% 
    DT::datatable()
	
library(e1071)
sms_classifier<-naiveBayes(sms_train, sms_raw_train$type)
sms_test_pred<-predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type, prop.chisq=FALSE, prop.t=FALSE,
prop.r=FALSE, dnn=c('predicted','actual'))
sms_classifier2<-naiveBayes(sms_train, sms_raw_train$type, laplace=1)
sms_test_pred2<-predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type, prop.chisq=FALSE, 
prop.t=FALSE, prop.r=FALSE,dnn=c('predict', 'actual'))

# 웹페이지 기사에서 사전만들고 중요단어 이용해서 비슷한 성향의 문서 확인
# 명사, 형용사, 부사
# 사전 제작
# 문서 분류
(article <- readLines("C:\\Users\\acorn\\Downloads\\데이터\\a1.txt", encoding="UTF-8"))
noun<-sapply(article, extractNoun, USE.NAMES=F)
noun
noun2<-unlist(noun)
wordcount<-table(noun2)
head(sort(wordcount, decreasing=T))
SimplePos22(article)
buildDictionary(ext_dic = c('sejong', 'woorimalsam'), category_dic_nms=c('political'))
buildDictionary(ext_dic = c('sejong', 'woorimalsam'),
                user_dic = data.frame(term="전작권", tag='ncn'), # 명사형 
                category_dic_nms=c('political'))

