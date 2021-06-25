#라이브러리 패키지 설치 및 로딩
install.pakages("KoNLP")
install.pakages("tm")
install.pakages("wordcloud2")
install.pakages("dplyr")
install.pakages("tidytext")
install.pakages("stringr")
nstall.packages("arules")
library(stringr)
library(KoNLP)
library(tm)
library(dplyr)
library(tidytext)
library(wordcloud2)
library(arules)
library(httr)
library(XML)

#형태소 사전 활성화
useNIADic()

#1 연설문 토픽 분석
Dr.King <- readLines("DrKing.txt", encoding = 'UTF-8')#한글문서이므로 UTF8 적용
#불용어와 불필요 공백을 삭제하고 tibble화 한 다음, 명사(단어)를 기준으로 토큰화함.
#토큰화 이후 2글자 이상만 등장 빈도에 따라 정렬함.

Dr.KingWord <- Dr.King %>% 
  str_replace_all("[^가-힣]", " ") %>%  #우선 한글을 제외한 불용어 삭제함
  str_squish() %>%  #2칸이상 띄어쓰기 삭제
  as_tibble() %>% #토큰화하기 쉽게 tibble화 우선 실시
  unnest_tokens(input = value,
                output = word,
                token = extractNoun) %>% #명사(단어) 기준으로 토큰화함
  count(word,sort = T) %>% #단어 빈도를 구해 내림차순으로 정렬함
  filter(str_count(word) > 1) #2글자 이상만 추출
Dr.KingWord #데이터 정제 결과 다양한 단어가 확인되는데, 단어가 300행 이상으로 너무 많으므로 상위 50개만 추려낼 것
Top50 <- head(Dr.KingWord, 50)
wordcloud2(Top50, color = 'random-light', backgroundColor = 'black', fontFamily = '나눔바른고딕', shape = 'triangle-forward')
#단어구름 생성결과와 데이터 정제결과 확인을 통해 해당 연설문에서 가장 중요했던 단어는 '우리(맥락상 흑인과 동의어)'(33회),
#'자유'(27회),'흑인'(19회) 등 임을 확인할 수 있음.

#2 연설문 토픽 분석결과를 토대로 연관분석

lword <- Map(extractNoun, Dr.King) #기본제공 맵함수로 명사추출 시행.
length(lword)
lword <- unique(lword) #중복단어 제거하기
filter1 <- function(x){
  nchar(x) >= 2 && is.hangul(x) #글자수 2글자 이상만 필터링하는데 사용할 함수
}
filter2 <- function(x){Filter(filter1, x)} #위에서 작성한 함수로 연설문을 필터링하는데 쓸 함수
lword <- sapply(lword, filter2)
lword #필터링 결과 확인
lword <- sapply(lword, unique) #연관분석을 위해 이제 트랜잭션으로 바꿔야 함.
wordTran <- as(lword,"transactions")
wordTran #19트랜잭션과 312칼럼.
inspect(wordTran)
rules1 <- apriori(wordTran,
                     parameter = list(sup = 0.1, conf = 0.8)) #우선 기본값인 지지도 0.1 신뢰도 0.8 최대길이 10의 형태로 확인
#2696개의 규칙 확인 가능. 너무 많기때문에 지지도와 신뢰도를 수정해서 다시 실행
rules2 <- apriori(wordTran,
                  parameter = list(supp = 0.25, conf = 0.05)) #29개로 줄어듬

inspect(rules2)

#엣지로 시각화단계
rules <- labels(rules2, ruleSep = " ") #연관규칙 레이블을""로 분리하고 자료구조 변경
class(rules)
rules <- sapply(rules, strsplit, " ",USE.NAMES = F)
rulemat <- do.call("rbind", rules)
class(rulemat) #매트릭스 구조로 행단위로 묶어서 생성했음.
install.packages("igraph")
library(igraph)
ruleg <- graph.edgelist(rulemat[c(9:29),], directed=F) #T값으로줄경우 {제시}->{마케팅}으로 표시된다.  
plot.igraph(ruleg, vertex.label = V(ruleg)$name,
            vertex.label.ces = 1.2, vertex.label.color = 'black',
            vertex.size = 20, vertex.color = 'lightblue',
            vertex.frame.color = 'blue')

#3 실시간 뉴스데이터 토픽 분석(2021.04.07 21:00)
url <- "http://media.daum.net"
web <- GET(url)
web #웹문서 요청하기

#파싱을 통해 웹문서 다듬기
html <- htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = 'UTF-8')
rootNode <- xmlRoot(html)
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']",xmlValue)

#자료 전처리 단계
news_pre <- gsub("[\r\n\t]",' ',news) #이스케이프 제거
news_pre <- gsub('[[:punct:]]',' ',news_pre) #문장부호 제거
news_pre <- gsub('[[:cntrl:]]',' ',news_pre) #특수문자 제거
news_pre <- gsub('[a-z]+',' ',news_pre)
news_pre <- gsub('[A-Z]+',' ',news_pre)
news_pre <- gsub('\\s+',' ',news_pre)

news_pre #전처리결과 확인 후 랭킹뉴스등 불용한 부분 잘라내기
news_data <- news_pre[1:24]

exNouns <- function(x){paste(extractNoun(x),collapse = " ")} #단어 추출용 함수 제작

#시각화단계
news_nouns <- sapply(news_data, exNouns) #명사들만 추출
newsCorpus <- Corpus(VectorSource(news_nouns)) #말뭉치(Corpus)생성
inspect(newsCorpus[1:5])
tdm <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4,16))) #2단어 이상만 필터링
news.df <- as.data.frame(as.matrix(tdm)) #행렬 데이터프레임화 시켜서 시각화 준비 완료
dim(news.df) #요소 확인.
wordResult <- sort(rowSums(news.df), decreasing = T)
newsname <- names(wordResult)
worldnews.df <- data.frame(word = newsname, freq = wordResult)
wordcloud2(worldnews.df, color = 'random-light',backgroundColor = "white", fontFamily = '나눔바른고딕')