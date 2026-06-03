#26-0529

#
KOICA_SDGs_20231231 |> 
  count(사업유형명, sort = T)

KOICA_SDGs_20231231 |> 
  colnames()

unique(KOICA_SDGs_20231231$'SDGs 필드')


KOICA_SDGs_20231231 |> 
  slice_max(order_by = `지원액(달러)`)


#전체사업시작일자
ggplot(KOICA_SDGs_20231231, 
       aes(x = 전체사업시작일자)) + 
  geom_histogram()


#전체사업종료일자
ggplot(KOICA_SDGs_20231231, aes(x = 전체사업종료일자)) +
  geom_histogram() +
  labs(title = '전체사업 종료일자 히스토그램',
       subtitle = '2023년 KOICA',
       tag = '1')
 
?penguins
?penguins

penguins |> 
  ggplot(aes(x = bill_length_mm, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() -> penguins_kcoc

#변수
penguins_kcoc +
  facet_wrap(.~species)

penguins_kcoc +
  facet_grid(sex~species)

#변수를 변경

starwars |> view()
starwars |> print(n = 20)

starwars |> 
  drop_na(hair_color)

starwars |> print(n = 12)
starwars |> 
  mutate(
    mass = replace_na(mass, 0)
  ) |> print(n = 12)
  
  


