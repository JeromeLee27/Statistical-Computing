url1 = 'http://search.hani.co.kr/Search?command=query&keyword=%EC%95%84%EC%8B%9C%EC%95%84%EB%82%98%ED%95%AD%EA%B3%B5&media=news&submedia=&sort=d&period=month&datefrom=20201112&dateto=20201212&pageseq='
#url2 = '&sort=1&d1=20201112~20201212'
news_page_han_A = list()
for(page in 0:3){
  url.han_A = paste0(url1, page)
  web.han_A = GET(url.han_A)
  html.han_A = htmlTreeParse(web.han_A, useInternalNodes = T, trim = T,
                           encoding = 'utf-8')
  rootNode = xmlRoot(html.han_A)
  title = c()
  for(i in 1:10){
    xpath1 = '//*[@id="contents"]/div[3]/ul/li['
    xpath2 = ']/dl/dt/a'
    xpath = paste0(xpath1, i, xpath2)
    x = xpathSApply(rootNode, xpath, xmlValue)
    title[i] = x
  }
  news_page_han_A[[page+1]] = title
}
news_page_han_A
news_page_han_A = unlist(news_page_han_A)
news_page_han_A

#전처리
news_pre<-gsub("[\r\n\t]",'',news_page_han_A)
news_pre
news_pre1<-gsub("[[:punct:]]",'',news_pre)
news_han1_A = news_pre1
#news_data<-news_pre1[-c(21,31,32)]
news_han1_A
str(news_han1_A)
news_han1_A = as.data.frame(news_han1_A)

#파일 저장과 읽기
getwd()
write.csv(news_han1_A,"news_han_A.csv",quote = F)
news_han_A<-read.csv("news_han_A.csv",header = T,stringsAsFactors = F, encoding = 'utf-8')
str(news_han_A)
names(news_han_A)<-c("no","news_text")
head(news_han_A)

#토픽 분석
library(tm)
library(wordcloud)

han_A.Corpus<-Corpus(VectorSource(news_han_A))
han_A.Corpus
inspect(han_A.Corpus[1:5])
han_A.tdm<-TermDocumentMatrix(han_A.Corpus,control = list(wordLengths=c(4,16)))
Encoding(han_A.tdm$dimnames$Terms)="utf-8"
han_A.tdm
han_A.tdm.df<-as.data.frame(as.matrix(han_A.tdm))
str(han_A.tdm.df)
#단어 출현 빈도수 구하기
wordResult.han_A<-sort(rowSums(han_A.tdm.df),decreasing = T)
wordResult.han_A[1:10]

#단어 구름 생성
library(wordcloud)
myNames.han_A<-names(wordResult.han_A)
myNames.han_A
str(myNames.han_A)
str(wordResult.han_A)

#단어 빈도수 구하기
library(wordcloud)
df.han_A<-data.frame(word=myNames.han_A,freq=wordResult.han_A)
h
#단어구름 생성
pal<-brewer.pal(12,"Paired")
wordcloud(df.han_A$word,df.han_A$freq,min.freq = 2,
          random.order =F,scale=c(4,0.7),
          rot.per = .1,colors = pal,family="malgun")
