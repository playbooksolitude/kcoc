#26-0612 fri

#
library(tidyverse)
library(showtext)
showtext_auto()
library(janitor)

#
koica_1csv

koica_1csv |> 
  filter(국가명 == '피지', 
         파견직종 == '축산')

koica_1csv |> 
  filter(국가명 == '피지') |> 
  filter(파견직종 == '축산')

#3.4 ‘가나’와 ’네팔’의 ’유아교육’ 파견 인원을 합산하면 총 몇 명입니까?
koica_1csv |> 
  filter(국가명 %in% c('가나', '네팔') & 파견직종 == '유아교육')

#’파견분야’별로 파견인원 합계를 출력하시오
koica_1csv |> 
  #count(파견분야)
  group_by(파견분야) |> 
  reframe(파견인원_합계 = sum(인원), 
          n = n())

#filter(국가명 %in% c(‘르완다’, ‘모로코’, ‘에콰도르’))

koica_1csv |> 
  filter(국가명 %in% c('르완다', '모로코', '에콰도르'), 
         파견직종 == '사회복지')

koica_1csv |> view()

koica_1csv |> 
  group_by(국가명) |> 
  reframe(파견인원_합계 = sum(인원)) |> 
  filter(파견인원_합계 > 30)

penguins |> 
  view()
?penguins
penguins |> 
  filter(species == '아델리(Adelie)')

penguins |> 
  select(body_mass) |> 
  tibble()
  
penguins |> 
  tibble() |> 
  filter(flipper_len < 180)

penguins$species |> table() |> 
  tibble()

library(GGally)
penguins |> 
  ggpairs(columns = c("bill_length_mm", 
                      "bill_depth_mm", 
                      "flipper_length_mm", 
                      "body_mass_g",
                      "species"))

penguins |> colnames()

# 색깔 없는 것
penguins |> 
  ggpairs(columns = c("bill_length_mm", 
                      "bill_depth_mm", 
                      "flipper_length_mm", 
                      "body_mass_g"))
                      
# 색깔 부여 (종별로)
penguins |> 
  ggpairs(columns = c("bill_length_mm", 
                      "bill_depth_mm", 
                      "flipper_length_mm", 
                      "body_mass_g", 
                      "species"), 
          aes(color = species))

penguins |> 
  count(species)






