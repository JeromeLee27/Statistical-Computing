library(KoNLP)
library(rvest)
library(httr)
library(XML)
warning()


#<div class = 'main content'~~~>
url1 = 'http://search.zum.com/search.zum?method=news&option=accu&query=%eb%8c%80%ed%95%9c%ed%95%ad%ea%b3%b5&rd=1&cluster=&startdate=20201112&enddate=20201212&datetype=month&scp=0&page='
url2 = '&mm=direct&docCategory=2003'
news_page_zum = list()

for(page in 1:50){
  url_zum = paste0(url1,page,url2)
  web_zum = GET(url_zum)
  html_zum = htmlTreeParse(web_zum, useInternalNodes = T, trim = T, encoding = 'utf-8')
  rootNode_zum = xmlRoot(html_zum)
  title = c()
  for(i in 1:10){
    xpath1 = '//*[@id="newsItemUl"]/li['
    xpath2 = ']/dl/dt/a'
    xpath = paste0(xpath1, i, xpath2)
    news = xpathSApply(rootNode_zum, xpath, xmlValue)
    title[i] = news
  }
  news_page_zum[[page]] = title
}
news_page_zum
news_page_zum<-unlist(news_page_zum)
news_page_zum

#전처리
news_pre<-gsub("[\r\n\t]",'',news_page_zum)
news_pre
news_pre1<-gsub("[[:punct:]]",'',news_pre)
news_page_zum = news_pre1
str(news_page_zum)
news_page_zum.df<-as.data.frame(news_page_zum)
str(news_page_zum.df)
news_page_zum.df

#파일 저장과 읽기
setwd("C:/Users/user/Desktop/2020 - 2/전산통계1/기말프로젝트")
write.csv(news_page_zum,"news_page_zum.csv",quote = F)
news_page_zum<-read.csv("news_page_zum.csv",header = T,stringsAsFactors = F, encoding = 'utf-8')
str(news_page_zum)
names(news_page_zum)<-c("no","news_text")
head(news_page_zum)


#말뭉치 생성
news_zum_Corpus<-Corpus(VectorSource(news_page_zum))
news_zum_Corpus
inspect(news_zum_Corpus)
TDM_zum<-TermDocumentMatrix(news_zum_Corpus,control=list(wordLengths=c(4,16)))
Encoding(TDM_zum$dimnames$Terms) = 'utf-8'
TDM_zum
news_zum.df<-as.data.frame(as.matrix(TDM_zum))
str(news_zum.df)

#단어 출현 빈도수 구하기
wordResult_zum<-sort(rowSums(news_zum.df),decreasing = T)
wordResult_zum[1:10]
#단어구름 생성
myNames_zum<-names(wordResult_zum)
myNames_zum
#단어 빈도수 구하기
df_zum<-data.frame(word=myNames_zum,freq=wordResult_zum)
head(df_zum)
df_zum
#단어구름 생성
pal<-brewer.pal(12,"Paired")
wordcloud(df_zum$word,df_zum$freq,min.freq = 2,
          random.order =F,scale=c(4,0.7),
          rot.per = .1,colors = pal,family="malgun")

