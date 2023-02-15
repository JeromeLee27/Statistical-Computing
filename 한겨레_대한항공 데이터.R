library(KoNLP)
library(rvest)
library(httr)
library(XML)
warning()


url1 = 'http://search.hani.co.kr/Search?command=query&keyword=%EB%8C%80%ED%95%9C%ED%95%AD%EA%B3%B5&media=news&submedia=&sort=d&period=month&datefrom=20201112&dateto=20201212&pageseq='
#url2 = '&sort=1&d1=20201112~20201212'
news_page_han = list()
for(page in 0:4){
  url.han = paste0(url1, page)
  web.han = GET(url.han)
  html.han = htmlTreeParse(web.han, useInternalNodes = T, trim = T,
                            encoding = 'utf-8')
  rootNode = xmlRoot(html.han)
  title = c()
  for(i in 1:10){
    xpath1 = '//*[@id="contents"]/div[3]/ul/li['
    xpath2 = ']/dl/dt/a'
    xpath = paste0(xpath1, i, xpath2)
    x = xpathSApply(rootNode, xpath, xmlValue)
    title[i] = x
  }
  news_page_han[[page+1]] = title
}
news_page_han
news_page_han = unlist(news_page_han)
news_page_han

#전처리
news_pre<-gsub("[\r\n\t]",'',news_page_han)
news_pre
news_pre1<-gsub("[[:punct:]]",'',news_pre)
news_han1 = news_pre1
news_data<-news_pre1[-c(21,31,32)]

str(news_han1)
news_han1 = as.data.frame(news_han1)

#파일 저장과 읽기
getwd()
write.csv(news_han1,"news_han.csv",quote = F)
news_han<-read.csv("news_han.csv",header = T,stringsAsFactors = F, encoding = 'utf-8')
str(news_han)
names(news_han)<-c("no","news_text")
head(news_han)

#토픽 분석
library(tm)
library(wordcloud)

han.Corpus<-Corpus(VectorSource(news_han))
han.Corpus
inspect(khan.Corpus[1:5])
han.tdm<-TermDocumentMatrix(han.Corpus,control = list(wordLengths=c(4,16)))
Encoding(han.tdm$dimnames$Terms)="utf-8"
han.tdm
han.tdm.df<-as.data.frame(as.matrix(han.tdm))
str(han.tdm.df)
#단어 출현 빈도수 구하기
wordResult.han<-sort(rowSums(han.tdm.df),decreasing = T)
wordResult.han[1:10]

#단어 구름 생성
library(wordcloud)
myNames.han<-names(wordResult.han)
myNames.han
str(myNames.han)
str(wordResult.han)

#단어 빈도수 구하기
library(wordcloud)
df.han<-data.frame(word=myNames.han,freq=wordResult.han)
h
#단어구름 생성
pal<-brewer.pal(12,"Paired")
wordcloud(df.han$word,df.han$freq,min.freq = 2,
          random.order =F,scale=c(4,0.7),
          rot.per = .1,colors = pal,family="malgun")
