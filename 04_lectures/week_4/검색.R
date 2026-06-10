#26-0604 thu

#
library(tidyverse)
library(readxl)
library(palmerpenguins)

#
read_xlsx("./99.files/KOICA_SDGs_20231231.xlsx") -> koicasdgs_1_xlsx

#
koicasdgs_1_xlsx |> glimpse()
koicasdgs_1_xlsx |> 
  filter(사업연도 == '2023', 
         국가명 == '팔라우') |> 
  select(`SDGs 필드`)

#
koicasdgs_1_xlsx |> 
  filter(국가명 == '필리핀') |> 
  #split(.$사업유형명)
  #group_split(사업유형명)
  split(x = _, _$사업유형명)

#
koicasdgs_1_xlsx %>%
  filter(국가명 == '필리핀') %>%
  split(.$사업유형명)


#
koicasdgs_1_xlsx |> 
  filter(
    agrepl("화산", `사업명(한글)`, max.distance = 0)
  ) |> 
  select(`사업명(한글)`)

#
koicasdgs_1_xlsx |> 
  filter(사업유형명 == '민관협력사업') |> 
  select(지역명, 국가명, `지원액(달러)`, `사업명(한글)`) |> 
  group_split(지역명) 

#
koicasdgs_1_xlsx |> 
  split(koicasdgs_1_xlsx$사업유형명)

#
koicasdgs_1_xlsx %>%
  filter(사업유형명 == '개발컨설팅') %>%
  select(지역명, 국가명, `지원액(달러)`, `사업명(한글)`) %>%
  split(.$지역명)

# penguins ----
penguins |> 
  mutate(
    bill_length_mm = replace_na(bill_length_mm, 0)
  )
  #replace_na("0", bill_length_mm)
  #replace_na(bill_length_mm, 0)

na_if()
