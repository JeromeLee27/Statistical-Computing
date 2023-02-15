url1 = 'http://search.khan.co.kr/search.html?stb=khan&dm=3&q=%EC%95%84%EC%8B%9C%EC%95%84%EB%82%98%ED%95%AD%EA%B3%B5&pg='
url2 = '&sort=1&d1=20201112~20201212'
news_page_khan_A = list()
for(page in 1:5){
  url.khan_A = paste0(url1, page, url2)
  web.khan_A = GET(url.khan_A)
  html.khan_A = htmlTreeParse(web.khan_A, useInternalNodes = T, trim = T,
                            encoding = 'utf-8')
  rootNode = xmlRoot(html.khan_A)
  title = c()
  for(i in 1:9){
    xpath1 = '//*[@id="container"]/div[2]/div[2]/dl['
    xpath2 = ']/dt/a'
    xpath = paste0(xpath1, i, xpath2)
    x = xpathSApply(rootNode, xpath, xmlValue)
    title[i] = x
  }
  news_page_khan_A[[page]] = title
}
news_page_khan_A
news_page_khan_A = unlist(news_page_khan_A)
news_page_khan_A

#전처리
news_pre<-gsub("[\r\n\t]",'',news_page_khan_A)
news_pre
news_pre1<-gsub("[[:punct:]]",'',news_pre)
news_khan1_A = news_pre1


str(news_khan1_A)
news_khan1_A = as.data.frame(news_khan1_A)

#파일 저장과 읽기
getwd()
write.csv(news_khan1_A,"news_khan_A.csv",quote = F)
news_khan_A<-read.csv("news_khan_A.csv",header = T,stringsAsFactors = F, encoding = 'utf-8')
str(news_khan_A)
names(news_khan_A)<-c("no","news_text")
head(news_khan_A)

#토픽 분석
library(tm)
library(wordcloud)

khan_A.Corpus<-Corpus(VectorSource(news_khan_A))
khan_A.Corpus
inspect(khan_A.Corpus[1:5])
khan_A.tdm<-TermDocumentMatrix(khan_A.Corpus,control = list(wordLengths=c(4,16)))
Encoding(khan_A.tdm$dimnames$Terms)="utf-8"
khan_A.tdm
khan_A.tdm.df<-as.data.frame(as.matrix(khan_A.tdm))
str(khan.tdm_A.df)
#단어 출현 빈도수 구하기
wordResult.khan_A<-sort(rowSums(khan_A.tdm.df),decreasing = T)
wordResult.khan_A[1:10]

#단어 구름 생성
library(wordcloud)
myNames.khan_A<-names(wordResult.khan_A)
myNames.khan_A
str(myNames.khan_A)
str(wordResult.khan_A)

#단어 빈도수 구하기
library(wordcloud)
df.khan_A<-data.frame(word=myNames.khan_A,freq=wordResult.khan_A)

#단어구름 생성
pal<-brewer.pal(12,"Paired")
wordcloud(df.khan_A$word,df.khan_A$freq,min.freq = 2,
          random.order =F,scale=c(4,0.7),
          rot.per = .1,colors = pal,family="malgun")
