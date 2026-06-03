#26-0523 sat 09:47

#
library(tidyverse)
library(googlesheets4)

# 1 불러오기 ----
## 1.1 sheet ----
#gs4_auth(cache = ".secrets", email = TRUE)

koica_evalrep_1_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1FdBaFNHuJs80EdOVZcEJe5sK15vZqxnQWwVc0fazENs/edit?gid=8927854#gid=8927854")

#
# koica_clean <- koica_evalrep_1_sheet |> 
#   mutate(
#     # 기초선 열 변환
#     기초선 = map_chr(기초선, \(x) if (is.null(x)) NA_character_ else as.character(x)),
#     # 목표치 열 변환
#     목표치 = map_chr(목표치, \(x) if (is.null(x)) NA_character_ else as.character(x))
#   )

# ## 1.2 view  ----
# koica_clean |> view()
# 
# # 2 구조 보기
# ## 2.1 glimpse ----
# koica_clean |> glimpse()

## 2.2 고유값 ----
koica_evalrep_1_sheet |> 
  sapply(n_distinct)

## 2.3 고유값 테이블 ----  
koica_evalrep_1_sheet |> 
  sapply(n_distinct) |> 
  enframe(name = "변수명", value = "고유값_개수")

## 2.4 범위 ----
koica_evalrep_1_sheet |> 
  select(사업번호, 시작년도, 종료년도) |> 
  summary()

# 3 변수 저장 ----
## 3.1. 변수 ----
koica_evalrep_1_sheet |> 
  sapply(n_distinct) |> 
  enframe(name = "변수명", value = "고유값_개수") -> koica_evalrep_2_tibble

## 3.2 변수 조회
koica_evalrep_2_tibble

# 4 그래프 ----
## 4.1 도메인 막대그래프 ----
koica_evalrep_1_sheet |> 
  count(도메인) |> 
  ggplot(aes(x = 도메인, y = n)) +
  geom_bar(stat = 'identity') 

## 4.1.1 한글 표기 ----
library(showtext)
showtext_auto()

koica_evalrep_1_sheet |> 
  count(도메인) |> 
  ggplot(aes(x = 도메인, y = n)) +
  geom_bar(stat = 'identity') 


### 4.1.2 값 추가 ----
koica_evalrep_1_sheet |> 
  count(도메인) |> 
  ggplot(aes(x = 도메인, y = n)) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = n), size = 8) +
  labs(title = 'KOICA 종료평가 보고 건수', 
       subtitle = '기간: 2007~2024')


### 4.1.3 주석 ----
koica_evalrep_1_sheet |> 
  count(도메인) |> 
  ggplot(aes(x = 도메인, y = n)) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = n), size = 8) +
  labs(title = 'KOICA 종료평가 보고 건수', 
       subtitle = '기간: 2007~2024')


# 5 스타일 ----
## 5.1 devtools ----
if(!require(devtools))install.packages("devtools")
#install.packages('devtools')

## 5.2 bbplot ----
devtools::install_github('bbc/bbplot')
library(bbplot) 

## 5.3. bbc style ----
koica_evalrep_1_sheet |> 
  count(도메인) |> 
  ggplot(aes(x = 도메인, y = n)) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = n), size = 8) +
  labs(title = 'KOICA 종료평가 보고 건수', 
       subtitle = '기간: 2007~2024') +
  bbc_style()

### 6 지표_대분류 막대 ----
koica_evalrep_1_sheet |> 
  count(국가명, sort = T) |> 
  gt::gt()
  

#
# koica_evalrep_1_sheet |> 
#   count(지표_대분류) |> 
#   ggplot(aes(x = 지표_대분류, y = n)) +
#   geom_bar(stat = 'identity') +
#   geom_label(aes(label = n), size = 4) +
#   coord_flip()
#   labs(title = 'KOICA 종료평가 보고 건수', 
#        subtitle = '기간: 2007~2024') +
#   bbc_style()
# 
#   koica_evalrep_1_sheet |> 
#     head() |> 
#     view()





































#join ----
# source("./04_lectures/week_2/04.국가코드찾는법iso.R")
# koica_evalrep_1_sheet |> 
#   filter(도메인 == '교육') |> 
#   left_join(codelist_2ko, by = c('국가명' = 'cldr.name.ko')) 


# #3 ----
# koica_evalrep_1_sheet |> 
#   group_by(사업번호) |> 
#   reframe(합계_사업금액_usd = sum(사업금액_usd), 사업건수 = n()) |> 
#   arrange(desc(합계_사업금액_usd))
# 
# koica_evalrep_1_sheet |> 
#   filter(사업번호 == '2016-00001') 
# 
# koica_evalrep_1_sheet |> 
#   filter(사업번호 == '2016-00001') |> 
#   count(지표_대분류, 지표_중분류) |> 
#   print(n = Inf)
# 

# # replace_na ----
# koica_evalrep_1_sheet |> 
#   colnames()
# 
# koica_evalrep_1_sheet |> 
#   mutate(목표치 = na_if(목표치, "NULL")) 



