#26-0522 fri 20:24

#install.packages("datasauRus")
library(tidyverse) # 그래프 그리는 패키지
library(datasauRus) #공룡 나오는 데이터
datasaurus_dozen #그냥 실행
library(showtext)
showtext_auto()

ggplot(data = datasaurus_dozen, 
       mapping = 
         aes(x = x, y = y))+
  geom_point() +
  facet_wrap(.~dataset, ncol = 5) -> kcoc_datasaurus

kcoc_datasaurus

mpg
diamonds |> 
  ggplot(aes(x = x)) +
  geom_histogram() +
  coord_cartesian(ylim = c(0,10))


library(googlesheets4)
read_sheet("https://docs.google.com/spreadsheets/d/1UWSL9OYU_bSdp5Yya4GlXci8p0295NjwfY0jqqqSHEs/edit?gid=1227049853#gid=1227049853") -> sdgs_1_sheet

sdgs_1_sheet |> colnames()
sdgs_1_sheet |> 
  count(사업유형명, 지역명)

sdgs_1_sheet |> 
  count(사업유형명)

sdgs_1_sheet |> 
  count(지역명)

sdgs_1_sheet |> 
  count(지역명, 사업유형명) |> 
ggplot(aes(x = 지역명, y = 사업유형명, fill = n)) +
  geom_tile(color = "white") +
  MetBrewer::scale_fill_met_d()
  scale_fill_gradient(low = "#f7fbff", high = "#08306b") + # 연한 파랑 -> 진한 파랑
  theme_minimal() +
  labs(title = "지역별 사업유형 분포 히트맵", x = "지역명", y = "사업유형명", fill = "사업 수(n)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 지역명이 겹치지 않도록 회전
MetBrewer::display_all()
MetBrewer::MetPalettes

sdgs_1_sheet |> 
  count(지역명, 사업유형명) |> 
  ggplot(aes(x = 지역명, y = 사업유형명, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#e7e5cc", high = "#192813") + # 연한 파랑 -> 진한 파랑
  theme_minimal() +
  geom_text(aes(label = n), color = 'tomato') +
  labs(title = "지역별 사업유형 분포 히트맵", x = "지역명", y = "사업유형명", fill = "사업 수(n)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 지역명이 겹치지 않도록 회전



