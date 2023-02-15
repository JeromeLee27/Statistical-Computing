library(KoNLP)
library(rvest)
library(httr)
library(XML)
warnings()

url1 = 'http://search.khan.co.kr/search.html?stb=khan&dm=3&q=%EB%8C%80%ED%95%9C%ED%95%AD%EA%B3%B5&pg='
url2 = '&sort=1&d1=20201112~20201212'
news_page_khan = list()
for(page in 1:6){
  url.khan = paste0(url1, page, url2)
  web.khan = GET(url.khan)
  html.khan = htmlTreeParse(web.khan, useInternalNodes = T, trim = T,
                            encoding = 'utf-8')
  rootNode = xmlRoot(html.khan)
  title = c()
  for(i in 1:9){
    xpath1 = '//*[@id="container"]/div[2]/div[2]/dl['
    xpath2 = ']/dt/a'
    xpath = paste0(xpath1, i, xpath2)
    x = xpathSApply(rootNode, xpath, xmlValue)
    title[i] = x
  }
  news_page_khan[[page]] = title
}
news_page_khan
news_page_khan = unlist(news_page_khan)
news_page_khan

#전처리
news_pre<-gsub("[\r\n\t]",'',news_page_khan)
news_pre
news_pre1<-gsub("[[:punct:]]",'',news_pre)
news_khan1 = news_pre1
news_data<-news_pre1[-c(21,31,32)]

str(news_khan1)
news_khan1 = as.data.frame(news_khan1)

#파일 저장과 읽기
getwd()
write.csv(news_khan1,"news_khan.csv",quote = F)
news_khan<-read.csv("news_khan.csv",header = T,stringsAsFactors = F, encoding = 'utf-8')
str(news_khan)
names(news_khan)<-c("no","news_text")
head(news_khan)

#토픽 분석
library(tm)
library(wordcloud)

khan.Corpus<-Corpus(VectorSource(news_khan))
khan.Corpus
inspect(khan.Corpus[1:5])
khan.tdm<-TermDocumentMatrix(khan.Corpus,control = list(wordLengths=c(4,16)))
Encoding(khan.tdm$dimnames$Terms)="utf-8"
khan.tdm
khan.tdm.df<-as.data.frame(as.matrix(khan.tdm))
str(khan.tdm.df)
#단어 출현 빈도수 구하기
wordResult.khan<-sort(rowSums(khan.tdm.df),decreasing = T)
wordResult.khan[1:10]

#단어 구름 생성
library(wordcloud)
myNames.khan<-names(wordResult.khan)
myNames.khan
str(myNames.khan)
str(wordResult.khan)

#단어 빈도수 구하기
library(wordcloud)
df.khan<-data.frame(word=myNames.khan,freq=wordResult.khan)
h
#단어구름 생성
pal<-brewer.pal(12,"Paired")
wordcloud(df.khan$word,df.khan$freq,min.freq = 2,
          random.order =F,scale=c(4,0.7),
          rot.per = .1,colors = pal,family="malgun")
