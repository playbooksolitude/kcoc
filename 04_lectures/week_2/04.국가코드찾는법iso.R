#26-0524 sun 10:38

#install.packages("countrycode")
library(countrycode)
library(tidyverse)

#
countrycode::codelist |> view()
countrycode::cldr_examples
countrycode::codelist

codelist |> colnames()

codelist |> 
  colnames() |> 
  str_subset('ko')


codelist |> 
  select(country.name.en, cldr.name.ko)


codelist |> 
  colnames() |> 
  enframe(name = '번호', value = '컬럼이름') |> 
  filter(컬럼이름 == 'cldr.name.ko') #cldr.name.ko


# 1. 원본 데이터 (예시)
my_data <- tibble(
  kor_name = c("대한민국", "미국", "일본", "프랑스", "가나"),
  value = c(100, 200, 150, 300, 50)
)

# 2. codelist에서 한국어 국가명과 iso2c만 뽑아서 맵핑 테이블 만들기
ko_mapping <- countrycode::codelist %>% 
  select(cldr.name.ko, iso2c) %>% 
  filter(!is.na(cldr.name.ko)) # 결측치 제거

# 3. left_join으로 원본 데이터에 iso2c 붙이기
my_data_cleaned <- my_data %>% 
  left_join(ko_mapping, by = c("kor_name" = "cldr.name.ko"))

# 결과 확인
print(my_data_cleaned)

#
codelist |> #view()
  colnames() |> 
  enframe() |> 
  print(n = 50)
  
#
codelist |> 
  select(contains('iso'))

codelist

#
(codelist |> 
  select(cldr.name.ko, country.name.en,continent, 
         region,   #세계은행 기준 7개 분류
         region23, # UN M49 규격 지리적 기준22~23개 분류
         iso2c, iso3c) -> codelist_2ko)

#베트남 찾는 법 ----
codelist_2ko |> 
  filter(
    agrepl(
      '베트남', cldr.name.ko
    )
  )

  



