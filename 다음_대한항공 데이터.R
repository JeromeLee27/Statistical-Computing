library(KoNLP)
library(rvest)
library(httr)
library(XML)

#1~5페이지 타이틀 추출
url1="https://search.daum.net/search?w=news&sort=recency&q=%EB%8C%80%ED%95%9C%ED%95%AD%EA%B3%B5&cluster=n&DA=PGD&dc=STC&pg=1&r=1&p="
url2="&rc=1&at=more&sd=20201112004450&ed=20201212004450&period=m"
news_page_daum <- list()
for(page in 1:10){
  url = paste0(url1, page, url2)
  web <-GET(url)
  html <-htmlTreeParse(web,useInternalNodes = T,trim = T,
                       encoding = "utf-8")
  rootNode<-xmlRoot(html)
  title=c()
  for(i in 1:10){
    xpath1='//*[@id="newsResultUL"]/li['
    xpath2=']/div/div/div'
    xpath=paste0(xpath1,i,xpath2)
    x<-xpathSApply(rootNode,xpath,xmlValue)
    title[i]<-x
  }
  news_page_daum[[page]]<-title
}
news_page_daum
news_page1<-unlist(news_page_daum)
news_page1

#전처리
news_pre<-gsub("[\r\n\t]",'',news_page1)
news_pre
news_pre1<-gsub("[[:punct:]]",'',news_pre)
news_pre1
str(news_pre1)
news_data.df<-as.data.frame(news_pre1)
str(news_data.df)
news_data.df

#파일 저장과 읽기
setwd()
write.csv(news_pre1,"daum.csv",quote = F)
news_data<-read.csv("daum.csv",header = T,stringsAsFactors = F)
str(news_data)
names(news_data)<-c("no","news_text")
head(news_data)

news_data<-Corpus(VectorSource(news_data))
news_data
inspect(news_data)
TDM_d<-TermDocumentMatrix(news_data,control=list(wordLengths=c(4,16)))
Encoding(TDM_d$dimnames$Terms) = 'utf-8'
TDM_d
news_d.df<-as.data.frame(as.matrix(TDM_d))
str(news_d.df)

#단어 출현 빈도수 구하기
wordResult_d<-sort(rowSums(news_d.df),decreasing = T)
wordResult_d[1:10]
#단어구름 생성
myNames_d<-names(wordResult_d)
myNames_d
#단어 빈도수 구하기
df_d<-data.frame(word=myNames_d,freq=wordResult_d)
head(df_d)
df_d
#단어구름 생성
pal<-brewer.pal(12,"Paired")
wordcloud(df_d$word,df_d$freq,min.freq = 2,
          random.order =F,scale=c(4,0.7),
          rot.per = .1,colors = pal,family="malgun")
