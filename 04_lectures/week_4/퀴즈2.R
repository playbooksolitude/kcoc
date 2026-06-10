#26-0609 

library(tidyverse)

#
read_csv("./99.files/koica_wfk_20260331.csv") -> koica_1csv

# 1 ----
koica_1csv |> 
  filter(국가명 == '네팔') |> 
  group_by(국가명) |> 
  reframe(총합 = sum(인원))

#2 ----
koica_1csv |> 
  filter(국가명 %in% c('탄자니아', '모로코')) |> 
  #distinct(파견분야)
  count(파견분야, 국가명)

#3 ----
koica_1csv |> 
  filter(파견직종 == '한국어교육(심화 및 일반)')

koica_1csv |> 
  filter(
    agrepl("한국어", 파견직종)
  ) |> 
  arrange(desc(인원))

#4 ----
koica_1csv |> 
  group_by(국가명) |> 
  reframe(인원합계 = sum(인원)) |> 
  filter(인원합계 > 30)

#5 ----
koica_1csv |> 
  filter(국가명 == '태국')

# 6 ----
koica_1csv |> 
  filter(국가명 == '피지', 파견직종 == '축산') |> 
  select(국가명, 파견직종, 파견분야)

# 7 ----
koica_1csv |> 
  filter(국가명 %in% c('가나', '네팔'), 
         파견직종 == '유아교육') |> 
  reframe(합계 = sum(인원))

# 8 ----
koica_1csv |>
  filter(국가명 %in% c('에티오피아', '가나', '파라과이', 
                    '라오스')) |> 
  count(국가명, 파견직종, 파견분야) |> 
  filter(파견직종 == '간호') 

# 9 ----
koica_1csv |> 
  filter(
    agrepl("태권도", 파견직종)
  )

# 10 ----
koica_1csv |> 
  filter(국가명 %in% c('르완다', '모로코', '에콰도르')) |> 
  filter(파견직종 == '사회복지')

# NEW ----
## 1 ----
koica_1csv |> 
  distinct(국가명) |> 
  reframe(n = n())

## 2 ----
koica_1csv |> 
  filter(
    agrepl("한국어", 파견직종)
  ) |> 
  group_by(국가명) |> 
  reframe(합계 = sum(인원)) |> 
  arrange(desc(합계))

## 3 ----
koica_1csv |> 
  filter(국가명 == '동티모르') |> 
  group_by(파견분야) |> 
  reframe(합계 = sum(인원))

## 4 ----
koica_1csv |> 
  filter(파견분야 == '기술환경에너지') |> 
  distinct(파견직종)

## 5 ----
koica_1csv |> 
  filter(국가명 == '이집트') |> 
  reframe(합계 = sum(인원))



















