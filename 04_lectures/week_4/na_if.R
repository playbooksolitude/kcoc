#26-0529 fri 09:00

#
library(tidyverse)
library(gapminder)

#
gapminder |> 
  summary()

gapminder |> 
  filter(pop == 0)

gapminder |> 
  filter(is.na(colSums()))

gapminder |> 
  is.na() |> 
  colSums()

starwars
library(dplyr)

# 1. 원본 데이터 상태 확인
# 진짜 NA도 있고, 'unknown'이라는 문자열 결측치도 3개 존재합니다.
table(starwars$eye_color, useNA = "ifany")

# 2. na_if()를 사용해 문자열 "unknown"을 진짜 NA로 한방에 바꾸기
starwars_clean <- starwars %>%
  mutate(eye_color = na_if(eye_color, "unknown"))

# 3. 결과 확인
# 'unknown' 3개가 사라지고, 기존 NA(3개)와 합쳐져 총 6개의 NA가 되었습니다!
table(starwars_clean$eye_color, useNA = "ifany")

#
starwars |> 
  mutate(hair_color = replace_na(hair_color, "unknown"))

starwars |> 
  mutate(hair_color = replace_na(hair_color, "unknown"), 
         sex = na_if(sex, 'none'))





