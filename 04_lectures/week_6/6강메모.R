#
library(tidyverse)
library(readxl)
library(writexl)

readxl::read_xlsx("./04_lectures/week_6/2023+OECD+CRS.xlsx") -> 
  koica_1_2023xlsx
#
koica_1_2023xlsx |> 

#
read_xlsx("./04_lectures/week_6/2024+OECD+CRS.xlsx") -> koica_1_2024
koica_1_2024 |> 
  count(사업실시기관명, sort = T)

koica_1_2024 |> 
  filter(사업실시기관명 %in% c('국제이주기구', '유엔아동기금')) |> 
  select(사업실시기관명, 자금구분, 약정액, 지출액) |> 
#  view()
  group_by(사업실시기관명) |> 
  reframe(평균지출액 = mean(지출액)) |> 
  gt::gt()


#
koica_1_2023xlsx |> 
  glimpse()

#
koica_1_2023xlsx |> 
  count(대륙명)
  

library(tidyverse)

koica_1_2023xlsx_cleaned <- koica_1_2023xlsx %>% 
  mutate(
    # ' > '를 기준으로 쪼개어 가장 첫 번째(index 1) 텍스트만 추출
    대륙_대분류 = str_split_i(대륙명, " > ", 1)
  )

# 전처리 결과 확인
koica_1_2023xlsx_cleaned %>% count(대륙_대분류)

# 전처리 
koica_1_2023xlsx

# 1. 컬럼명 정리
names(koica_1_2023xlsx) <- str_replace_all(names(koica_1_2023xlsx), 
                                           "\n", " ") %>% 
  str_trim()

# 2. 대륙 그룹 변수 생성
koica_1_2023xlsx %>%
  mutate(대륙그룹 = case_when(
    str_starts(대륙명, "아시아") ~ "아시아",
    str_starts(대륙명, "아프리카") ~ "아프리카",
    str_starts(대륙명, "아메리카") ~ "아메리카",
    TRUE ~ "기타"
  )) |> view()


# 3. 분석 추천 필터 (장학금 vs 프로젝트 원조 구분)
df_proj <- df %>% filter(원조유형 == "프로젝트 원조")





koica_1_2023xlsx |> 
  separate_wider_delim(
    대륙명, 
    delim = " > ",           # 구분자 (공백 포함)
    names = c("대륙", "지역1", "지역2"),   # 새 컬럼 이름
    too_few = "align_start", # 부족할 때 왼쪽부터 채움
    too_many = "drop" , cols_remove = F       # 너무 많으면 버림 (현재 데이터는 최대 3단계)
  ) |> 
  select(사업번호, 대륙명, 대륙, 지역1, 지역2)

# 결과 확인
koica |> 
  count(대륙, 지역1, 지역2, sort = TRUE)

koica_1_2023xlsx |> 
  count(대륙명)

koica_1_2023xlsx |> 
  separate_wider_delim(
    대륙명, 
    delim = " > ", 
    names = c("대륙", "지역"), 
    too_few = "align_start", 
    too_many = "merge"
  ) |> 
  select(사업번호, 대륙, 지역)


koica_1_2023xlsx |> 
  separate_wider_delim(
    대륙명, 
    delim = " > ", 
    names = c("대륙명_1", "대륙명_2", "대륙명_3"), 
    too_few = "align_start"
    #too_many = "merge"
  ) -> koica_2_2023xlsx
  #select(사업번호, 대륙명_1, 대륙명_2, 대륙명_3)

oecd2023_5select |> dim()
koica_1_2023xlsx |> dim()
koica_2_2023xlsx |> dim()
koica_2_2023xlsx |> write_xlsx("./2023+OECD+CRS_edit.xlsx")

oecd2023_5select |> colnames()


#체크
```{r}

koica_1_2023xlsx |> 
  separate_wider_delim(
    대륙명, 
    delim = " > ", 
    names = c("대륙", "지역1", "지역2"), 
    too_few = "align_start"
    #too_many = "merge"
  ) |> 
  select(사업번호, 대륙, 지역1, 지역2)

```