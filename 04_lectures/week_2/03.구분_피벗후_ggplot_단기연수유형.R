#26-0523 sat 21:51

#
library(tidyverse)
tidyverse_update()
library(googlesheets4)
library(showtext)
showtext_auto()

#
read_sheet("https://docs.google.com/spreadsheets/d/1KTi8EWWmHcbykuf8xN2Vh0s0kniLJ6TneKkYmc9K88U/edit?gid=1835542775#gid=1835542775", 
           sheet = "유형 피봇 테이블 2", 
           skip = 1) -> koica_단기연수_유형별_1sheet

#
koica_단기연수_유형별_1sheet |> 
  pivot_longer(cols = c(비대면:총계), 
               names_to = '구분', 
               values_to = '값') -> koica_단기연수_유형별_2pivot


koica_단기연수_유형별_2pivot |> 
  filter(국가 != '총계') |> 
  ggplot(aes(x = 구분, y = 국가, fill = 값)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = 값), color = 'snow')


# 총계 제외
koica_단기연수_유형별_2pivot |> 
  filter(국가 != '총계') |> 
  filter(구분 != '총계') |> 
  ggplot(aes(x = 구분, y = 국가, fill = 값)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = 값), color = 'snow')


#다국가 제외
koica_단기연수_유형별_2pivot |> 
  filter(!국가 %in% c('총계', '다국가')) |> 
  filter(구분 != '총계') |> 
  ggplot(aes(x = 구분, 
             y = 국가,
             fill = 값)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = 값), color = 'snow')


#순서 정렬
koica_단기연수_유형별_2pivot |> 
  filter(!국가 %in% c('총계', '다국가')) |> 
  filter(구분 != '총계') |> 
  ggplot(aes(x = 구분, 
             y = fct_reorder(국가, 값, .fun = sum), 
             fill = 값)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = 값), color = 'snow') +
  labs(y = '국가') +
  theme(legend.position = 'none')

# 면분할
#순서 정렬
koica_단기연수_전체_1sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1KTi8EWWmHcbykuf8xN2Vh0s0kniLJ6TneKkYmc9K88U/edit?gid=876842275#gid=876842275", 
           sheet = "글로벌연수(단기연수) 과정정보")

koica_단기연수_전체_1sheet |> 
  count(지역, 국가, `유형2(초청_현지_비대면)`) |> 
  filter(!국가 %in% c('총계', '다국가')) |> 
  ggplot(aes(x = `유형2(초청_현지_비대면)`, y = 국가, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow') +
  labs(y = '국가') +
  theme(legend.position = 'none') +
  facet_wrap(.~지역, scales = 'free_y', nrow = 1)


koica_단기연수_전체_1sheet |> 
  count(지역, 국가, `유형2(초청_현지_비대면)`) |> 
  mutate(유형2 = `유형2(초청_현지_비대면)`, .keep = 'unused', .before = 3, 
         유형2 = fct_relevel(유형2, '초청', '현지', '비대면')) |> 
  filter(!국가 %in% c('총계', '다국가')) |> 
  ggplot(aes(x = 유형2, y = 국가, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow') +
  labs(y = '국가') +
  theme(legend.position = 'none') +
  facet_wrap(.~지역, scales = 'free_y', nrow = 1) 


#오세아니아 제외
koica_단기연수_전체_1sheet |> 
  count(지역, 국가, `유형2(초청_현지_비대면)`) |> 
  mutate(유형2 = `유형2(초청_현지_비대면)`, .keep = 'unused', .before = 3, 
         유형2 = fct_relevel(유형2, '초청', '현지', '비대면')) |> 
  filter(!국가 %in% c('총계', '다국가')) |> 
  filter(지역 != '오세아니아') |> 
  ggplot(aes(x = 유형2, y = 국가, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow') +
  labs(y = '국가') +
  theme(legend.position = 'none') +
  facet_wrap(.~지역, scales = 'free_y', nrow = 1) 

 

