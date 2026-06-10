#26-0605 fri

#
library(tidyverse)
n_distinct()
dplyr::n_distinct



koica_wfk_20260331 |> 
  count(국가명, sort = T) # 많은 순서대로 정렬할 때는 sort = T

#
koica_wfk_20260331 |> 
  count(국가명) |> 
  arrange(desc(n))

#
koica_wfk_20260331 |> 
  #view()
  #filter(파견분야 == '교육') |> 
  print(n = Inf)

koica_wfk_20260331 |> colnames()
unique(koica_wfk_20260331$파견분야)
unique(koica_wfk_20260331$파견직종)  

koica_wfk_20260331$파견분야 |> unique()
unique(koica_wfk_20260331$파견분야)  

koica_wfk_20260331 |> 
  filter(파견분야 == '공공행정') |> 
  count(국가명)

# 해당 조건을 충족하는 모든 데이터를 위에서부터 25개만 출력

# 파견분야 == 교육
## 국가명 == 네팔
koica_wfk_20260331 |> 
  filter(파견분야 == '교육', 
         국가명 == '네팔') |> 
  print(n = 25)

koica_wfk_20260331 |> 
  split(koica_wfk_20260331$파견직종)

koica_wfk_20260331 |> 
  split(koica_wfk_20260331$국가명)

koica_wfk_20260331 |> 
  sample_n(10)

koica_wfk_20260331 |> 
  filter(국가명 %in% c('탄자니아', 
                    '모로코')) |> 
  pivot_wider(names_from = '국가명', 
              values_from = '인원') |> 
  view()
  

# 가로막대 ----
koica_wfk_20260331 |> 
  group_by(국가명, 파견분야) |> 
  reframe(파견인원_합계 = sum(인원)) |> 
  ggplot(aes(x = 국가명, y = 파견인원_합계, fill = 파견분야)) +
  geom_bar(stat = 'identity')  +
  theme(
    axis.text.x = element_text(
      angle = 90, 
      hjust = .9,
      size = 14
      ),
    #legend.position = "none"
    legend.position = "top"
    #legend.position = "bottom"
  )

# 세로막대
koica_wfk_20260331 |> 
  group_by(국가명, 파견분야) |> 
  reframe(파견인원_합계 = sum(인원)) |> 
  ggplot(aes(x = 국가명, y = 파견인원_합계, fill = 파견분야)) +
  geom_bar(stat = 'identity')  +
  coord_flip() +
  #scale_fill_brewer(palette = 'Set1')
  #scale_fill_brewer(palette = 'Dark2')
  scale_fill_brewer(palette = 'Paired')

RColorBrewer::display.brewer.all()
  
koica_wfk_20260331 |> 
  group_by(국가명, 파견분야) |> 
  reframe(파견인원_합계 = sum(인원)) |> 
  ggplot(aes(x = 파견분야, y = 국가명, fill = 파견분야)) +
  geom_tile()

koica_wfk_20260331 |> 
  group_by(국가명, 파견분야) |> 
  reframe(파견인원_합계 = sum(인원)) |> 
  filter(국가명 %in% c("페루", "라오스", "몽골","네팔", 
                    "탄자니아", "베트남", "필리핀")) |> 
  ggplot(aes(x = 파견분야, y = 국가명, fill = 파견인원_합계)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = 파견인원_합계), color = 'snow', size = 7) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 11))

koica_wfk_20260331 |> 
  group_by(국가명, 파견분야) |> 
  reframe()

koica_wfk_20260331 |> 
  ggplot(aes(x = 인원)) +
  geom_histogram(bins = 10) +
  coord_cartesian(y = c(0,5)) #확대하는 함수

koica_wfk_20260331 |> 
  select(-연도, -단원형태)




