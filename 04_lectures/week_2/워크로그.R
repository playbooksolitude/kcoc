#26-0529 fri 18:46

#
library(tidyverse)

install.packages("gt")
library(gt)

KOICA_SDGs_20231231
KOICA_SDGs_20231231 |> 
  select(사업연도, 사업유형명, 국가명, 지역명, `SDGs 필드`, 
         `지원액(달러)`, 전체사업시작일자) |> 
  mutate(번호 = row_number(), .before = 1) |> 
  head() |> 
  gt::gt() |> 
  gt::tab_header(
    title = ("KOICA 소규모 무상원조 SDGs 연계 현황")
  )
KOICA_SDGs_20231231 |> 
  select(사업연도, 사업유형명, 국가명, 지역명, `SDGs 필드`, 
         `지원액(달러)`, 전체사업시작일자) |> 
  mutate(번호 = row_number(), .before = 1) |> 
  head() |> 
  gt::gt() |> 
  gt::tab_header(
    title = ("KOICA 소규모 무상원조 SDGs 연계 현황")
    )


KOICA_SDGs_20231231

## 참고
Titanic
Titanic |> str()

Titanic |> 
  mosaicplot()

# 데이터 펼쳐보기
KOICA_SDGs_20231231 |> view()
KOICA_SDGs_20231231 |> dim()
KOICA_SDGs_20231231 |> nrow()
KOICA_SDGs_20231231 |> ncol()

#
KOICA_SDGs_20231231 |> 
  filter(사업유형명 == '소규모무상원조') |> 
  select(사업연도, 사업유형명, `지원액(달러)`) |> 
  arrange(desc(`지원액(달러)`)) |> 
  slice(1:5)


arrange(filter(select(KOICA_SDGs_20231231, 사업연도, 사업유형명, `지원액(달러)`), 사업유형명 == '소규모무상원조'), desc(`지원액(달러)`))

#
arrange(
  filter(
    select(KOICA_SDGs_20231231, 사업연도, 사업유형명, `지원액(달러)`), 
    사업유형명 == '소규모무상원조'), 
  desc(`지원액(달러)`))



