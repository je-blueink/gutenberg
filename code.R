getwd()
# 구텐베르크 프로젝트 이용한 소설 분류 연습

# 사용할 패키지 설치 및 활성화
library(rvest)
library(dplyr)
library(textstem)
library(stopwords)
library(tidytext)
library(wordcloud)
library(corrplot)

# 월간 Top 30 작품 목록 확인
top30 <- read.csv("GutenbergTop30.CSV")
top30 <- data.frame(top30[-31,-5])

# 텍스트 불러오기를 위한 함수 작성

crawl_books <- function(dataframe) {
  
  # 결과를 저장할 리스트를 초기화
  text_vectors <- vector("list", length = nrow(dataframe))
  
  # 각 웹페이지에서 텍스트를 스캔
  for (i in seq_len(nrow(dataframe))) {
  url <- dataframe$address[i]
  text <- scan(url, what = character(), encoding = "UTF-8", sep = "\n")
  
  # 각 URL에 대한 텍스트를 리스트에 추가
  text_vectors[[i]] <- text
  }
  
  # 최종 결과 반환
  return(text_vectors)
}

book_lists <- crawl_books(top30)
