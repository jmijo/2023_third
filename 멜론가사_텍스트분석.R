library(KoNLP)
library(stringr)
library(dplyr)
library(tidytext)

raw_lyric2021 <- readLines("C:/Users/admin/Desktop/2021melon50", encoding = "UTF-8")
raw_lyric2022 <- readLines("C:/Users/admin/Desktop/2022melon50", encoding = "UTF-8")

# 전처리 과정 (한글만 남기고, 중복 공백 제거, tibble로 변환)
lyric2021 <- raw_lyric2021 %>%
  str_replace_all("[^가-힣]", " ") %>%
  str_squish() %>%
  as_tibble()

# 텍스트 토큰화 작업 실시
word_lyric2021 <- lyric2021 %>%
  unnest_tokens(input = value, output = word, token = extractNoun)

# 2번 이상 나타난 단어들의 빈도 추출 후 정렬
word_noun2021 <- word_lyric2021 %>%
  count(word, sort = TRUE) %>%
  filter(str_count(word) > 1)

# 상위 30개 추출 
top30_2021 <- word_noun2021 %>% head(30)


# 2022 데이터에서 동일 과정 반복
lyric2022 <- raw_lyric2022 %>%
  str_replace_all("[^가-힣]", " ") %>%
  str_squish() %>%
  as_tibble()

word_lyric2022 <- lyric2022 %>%
  unnest_tokens(input = value, output = word, token = extractNoun)

word_noun2022 <- word_lyric2022 %>%
  count(word, sort = TRUE) %>%
  filter(str_count(word) > 1)

top30_2022 <- word_noun2022 %>% head(30)


# 시각화 
library(ggplot2)

# 막대 그래프 
ggplot(top30_2021, aes(x = reorder(word, n), y = n)) + geom_col() +
  coord_flip() + geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL)

ggplot(top30_2022, aes(x = reorder(word, n), y = n)) + geom_col() +
  coord_flip() + geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL)

# 워드클라우드 (전체 단어 기준)
library(ggwordcloud)

ggplot(word_noun2021, aes(label = word, size = n, col = n)) + geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA), range = c(3,30)) + scale_color_gradient(low = '#3F7BDC', high = '#2200DC')

ggplot(word_noun2022, aes(label = word, size = n, col = n)) + geom_text_wordcloud(seed = 1234) + 
  scale_radius(limits = c(3, NA), range = c(3,30)) + scale_color_gradient(low = '#3F7BDC', high = '#2200DC')
