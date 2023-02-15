library(KoNLP)
library(rvest)
library(httr)
library(XML)
install.packages('knitcitations')
library (knitcitations)
warning()


#<div class = 'main content'~~~>
url1 = 'https://news.joins.com/Search/JoongangNews?page='
url2 = '&Keyword=%EC%95%84%EC%8B%9C%EC%95%84%EB%82%98%ED%95%AD%EA%B3%B5&PeriodType=OneMonth&SortType=New&SearchCategoryType=TotalNews'
news_page_J_A = list()

for(page in 1:14){
  url_joongang_J_A = paste0(url1,page,url2)
  web_J_A = GET(url_joongang_J_A)
  html_J_A = htmlTreeParse(web_J_A, useInternalNodes = T, trim = T, encoding = 'utf-8')
  rootNode_J_A = xmlRoot(html_J_A)
  title = c()
  for(i in 1:10){
    xpath1 = '//*[@id="content"]/div[2]/div[2]/ul/li['
    xpath2 = ']/div/h2/a'
    xpath = paste0(xpath1, i, xpath2)
    news = xpathSApply(rootNode_J_A, xpath, xmlValue)
    title[i] = news
  }
  news_page_J_A[[page]] = title
}
news_page_J_A
news_page_J_A<-unlist(news_page_J_A)
news_page_J_A

#전처리
news_pre<-gsub("[\r\n\t]",'',news_page_J_A)
news_pre
news_pre1<-gsub("[[:punct:]]",'',news_pre)
news_page_J_A = news_pre1
str(news_page_J_A)
news_page_J_A.df<-as.data.frame(news_page_J_A)
str(news_page_J_A.df)
news_page_J_A.df

#파일 저장과 읽기
setwd("C:/Users/user/Desktop/2020 - 2/전산통계1/기말프로젝트")
write.csv(news_page_J_A,"news_page_J_A.csv",quote = F)
news_page_J_A<-read.csv("news_page_J_A.csv",header = T,stringsAsFactors = F, encoding = 'utf-8')
str(news_page_J_A)
names(news_page_J_A)<-c("no","news_text")
head(news_page_J_A)


#말뭉치 생성
news_J_A_Corpus<-Corpus(VectorSource(news_page_J_A))
news_J_A_Corpus
inspect(news_J_A_Corpus)
TDM_J_A<-TermDocumentMatrix(news_J_A_Corpus,control=list(wordLengths=c(4,16)))
Encoding(TDM_J_A$dimnames$Terms) = 'utf-8'
TDM_J_A
news_J_A.df<-as.data.frame(as.matrix(TDM_J_A))
str(news_J_A.df)

#단어 출현 빈도수 구하기
wordResult_J_A<-sort(rowSums(news_J_A.df),decreasing = T)
wordResult_J_A[1:10]
#단어구름 생성
myNames_J_A<-names(wordResult_J_A)
myNames_J_A
#단어 빈도수 구하기
df_J_A<-data.frame(word=myNames_J_A,freq=wordResult_J_A)
head(df_J_A)
df_J_A
#단어구름 생성
pal<-brewer.pal(12,"Paired")
wordcloud(df_J_A$word,df_J_A$freq,min.freq = 2,
          random.order =F,scale=c(4,0.7),
          rot.per = .1,colors = pal,family="malgun")

