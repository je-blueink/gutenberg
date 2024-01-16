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

# 본문 추출
book1_chpt <- grep(book_lists[[1]], pattern = "CHAPTER")
book1_end <- grep(book_lists[[1]], pattern = "END OF THE PROJECT GUTENBERG EBOOK")-2
book_lists[[1]][562]
book_lists[[1]][19048]
book1_body <- book_lists[[1]][(book1_chpt[136]):book1_end] 

# 전처리
book1_body %>% paste(collapse = " ") %>%
  tolower() %>%
  gsub(pattern = "’s", replacement="") %>%
  gsub(pattern = "([^[:alnum:][:blank:]’-])", replacement = "")

#토큰화
book1_token <- unlist(strsplit(book1_body, " "))

#원형복원 
book1_lemma <- lemmatize_strings(book1_token) 

#불용어 삭제(단어 벡터)
book1_words <- book1_lemma[!book1_lemma %in% c(stopwords(), "")] 

#단어 도수분포표 만들기
book1_table <- sort(table(book1_words), decreasing = T) 

#최빈출 단어 100개 추출
book1_topwords <- book1_table %>% head(100) %>% names() %>% as.vector()
