library(KoNLP)
library(rvest)
library(httr)
library(XML)
warning()


#<div class = 'main content'~~~>
url1 = 'https://news.joins.com/Search/JoongangNews?page='
url2 = '&Keyword=%EB%8C%80%ED%95%9C%ED%95%AD%EA%B3%B5&PeriodType=OneMonth&SortType=New&SearchCategoryType=JoongangNews'
news_page = list()

for(page in 1:24){
  url_joongang = paste0(url1,page,url2)
  web = GET(url_joongang)
  html = htmlTreeParse(web, useInternalNodes = T, trim = T, encoding = 'utf-8')
  rootNode = xmlRoot(html)
  title = c()
  for(i in 1:10){
    xpath1 = '//*[@id="content"]/div[2]/div[2]/ul/li['
    xpath2 = ']/div/h2/a'
    xpath = paste0(xpath1, i, xpath2)
    news = xpathSApply(rootNode, xpath, xmlValue)
    title[i] = news
  }
  news_page[[page]] = title
}
news_page
news_page1<-unlist(news_page)
news_page1

#전처리
news_pre<-gsub("[\r\n\t]",'',news_page1)
news_pre
news_pre1<-gsub("[[:punct:]]",'',news_pre)
news_data = news_pre1
str(news_data)
news_data.df<-as.data.frame(news_data)
str(news_data.df)
news_data.df

#파일 저장과 읽기
setwd("C:/Users/user/Desktop/2020 - 2/전산통계1/기말프로젝트")
write.csv(news_data,"news_data_JA.csv",quote = F)
news_data<-read.csv("news_data_JA.csv",header = T,stringsAsFactors = F, encoding = 'utf-8')
str(news_data)
names(news_data)<-c("no","news_text")
head(news_data)
dim(news_data)
#news 텍스트 벡터 생성
news_text<-news_data$news_text
news_text

#토픽 분석
library(tm)
library(wordcloud)
#exNouns<-function(x){paste(extractNoun(x),collapse = "")}
#exNouns
#news_nouns<-sapply(news_text,exNouns)
#news_nouns
#str(news_nouns)
#news_nouns<-unlist(news_text)
#news_nouns
#dim(news_nouns)

#말뭉치 생성
newsCorpus<-Corpus(VectorSource(news_data))
newsCorpus
inspect(newsCorpus)
TDM<-TermDocumentMatrix(newsCorpus,control=list(wordLengths=c(4,16)))
Encoding(TDM$dimnames$Terms) = 'utf-8'
TDM
news.df<-as.data.frame(as.matrix(TDM))
str(news.df)

#단어 출현 빈도수 구하기
wordResult<-sort(rowSums(news.df),decreasing = T)
wordResult[1:10]
#단어구름 생성
myNames<-names(wordResult)
myNames
#단어 빈도수 구하기
df<-data.frame(word=myNames,freq=wordResult)
head(df)
df
#단어구름 생성
pal<-brewer.pal(12,"Paired")
wordcloud(df$word,df$freq,min.freq = 2,
          random.order =F,scale=c(4,0.7),
          rot.per = .1,colors = pal,family="malgun")

