#26-0622 mon

#
library(tidyverse)
library(palmerpenguins)
library(treemapify)

#https://chatgpt.com/c/6a3a0ffb-a15c-83ee-b407-8db004f50f1d

#rm(list = ls())

#
mpg |> 
  group_by(year) |> 
  mutate(num = row_number(), .before = 1) |> 
  ungroup() |> 
  count(model, year)

#
qqnorm(penguins$bill_length_mm)
qqline(penguins$bill_length_mm)
penguins$bill_length_z <-
  scale(penguins$bill_length_mm)

penguins %>%
  mutate(
    bill_length_z =
      (bill_length_mm -
         mean(bill_length_mm, na.rm=TRUE))
    /
      sd(bill_length_mm, na.rm=TRUE)
  )

penguins$bill_length_z <-
  scale(penguins$bill_length_mm)

mean(abs(penguins$bill_length_z) <= 1)
mean(abs(bill_length_z) <= 1)
data(package = 'palmerpenguins', penguins)
penguins

#

library(dplyr)

penguins |>
  summarise(
    within_1sd =
      mean(abs(scale(body_mass_g)) <= 1,
           na.rm = TRUE),
    
    within_2sd =
      mean(abs(scale(body_mass_g)) <= 2,
           na.rm = TRUE),
    
    within_3sd =
      mean(abs(scale(body_mass_g)) <= 3,
           na.rm = TRUE)
  )


#
mpg |> 
  group_by(drv, class) |> 
  reframe(mean_hwy = mean(hwy), 
          mean_displ = mean(displ),
          n = n()) |> 
  ggplot(aes(area = n, 
             fill = mean_hwy, 
             label = class, 
             subgroup = drv)
         ) +
  geom_treemap() +
  geom_treemap_text(color = 'snow')  +
  geom_treemap_subgroup_border(color = 'snow') +
  geom_treemap_subgroup_text(place = 'center', 
                             color = 'snow', 
                             alpha = .5,
                             grow = T)
  
  
penguins |> 
  group_by(species) |> 
  reframe(mean_body = mean(body_mass_g, na.rm = T), 
          n = n())

diamonds
?geom_treemap

# 기각역 ----
penguins |> 
  filter(species %in% c('Adelie', 'Chinstrap')) |> 
  drop_na(bill_length_mm) -> penguins_clean

penguins_clean |> 
  reframe(
    n = n(),
    mean_length = mean(bill_length_mm), 
    sd_length = sd(bill_length_mm)
  )

t.test(bill_length_mm ~ species, data = penguins_clean)
penguins_clean |> 
  ggplot(aes(x = bill_length_mm, fill = species)) +
  geom_density() +
  scale_fill_brewer(palette = 'Set2') +
  geom_vline(
    xintercept =
      penguins_clean |>
      filter(species == "Adelie") |>
      with(mean(bill_length_mm))
  ) +
  theme_minimal()

# t 분포 ----
t_result <- t.test(
  bill_length_mm ~ species,
  data = penguins_clean
)

t_result$statistic

#
ggplot(
  penguins_clean,
  aes(bill_length_mm,
      fill = species)) +
  geom_density(alpha = .5)

#
(penguins |> 
    drop_na() |> 
    group_by(species) |> 
    reframe(mean_length = round(mean(bill_length_mm, na.rm = T),1),
            mean_depth = round(mean(bill_depth_mm, na.rm = T), 1),
            mean_mass = round(mean(body_mass_g, na.rm = T), 1),
            n = n()) -> penguins_1_table)

# t값 구하기 ----
# 1 drop_na
penguins |> 
  drop_na() -> penguins_1_clean

# 2 Chinstrap_Gentoo
penguins_1_clean |>   
  filter(species %in% c('Chinstrap', 'Gentoo')
         ) -> penguins_2_Chinstrap_Gentoo

# 2.1 밀도함수
penguins_2_Chinstrap_Gentoo |> 
  filter(species %in% c('Chinstrap', 'Gentoo')) |> 
  ggplot(aes(x = bill_length_mm, fill = species)) +
  geom_density()

# 2.2 t.test
t.test(bill_length_mm ~ species, data = penguins_2_Chinstrap_Gentoo)


# 2.3 효과크기 
effectsize::cohens_d(
  bill_length_mm ~ species,
  data = penguins_2_Chinstrap_Gentoo
  )

# 3 평균과 표준편차
(penguins_2_Chinstrap_Gentoo |> 
  group_by(species) |> 
  reframe(
    n = n(),
    mean_bill_length = mean(bill_length_mm), 
    sd_bill_length = sd(bill_length_mm)
    ) -> penguins_3_table)
  

# 3.1 평균 차이                           #1.265756
penguins_3_table |> 
  pull(mean_bill_length) |> 
  nth(1) - penguins_3_table |> 
  pull(mean_bill_length) |> 
  nth(2) -> penguins_3_table_1평균차이


# 3.2 표준오차 Chinstrap                   #0.4049443
penguins_3_table |> 
  filter(species == 'Chinstrap') |> 
  pull(sd_bill_length) /                   #3.339256
  sqrt(                                    #8.246211  
    penguins_2_Chinstrap_Gentoo |> 
      filter(species == 'Chinstrap') |> 
      nrow()
  ) -> penguins_3_table_2Chinstrap_n값


# 3.3 표준오차 Gentoo                      #0.282
penguins_3_table |> 
  filter(species == 'Gentoo') |> 
  pull(sd_bill_length) /                   #3.106116
  sqrt(                                    #10.90871  
    penguins_2_Chinstrap_Gentoo |> 
      filter(species == 'Gentoo') |> 
      nrow()
  ) -> penguins_3_table_3Gentoo_n값

# 3.4 표준편차 Chinstrap        #3.339256
(penguins_3_table |> 
  pull(sd_bill_length) |> 
  nth(1) -> penguins_3_table_4Chinstrap_sd)


# 3.5 표준편차 Gentoo          #3.106116
(penguins_3_table |> 
  pull(sd_bill_length) |> 
  nth(2) -> penguins_3_table_5Gentoo_sd)


# 3.6 Chinstrap n값
penguins_2_Chinstrap_Gentoo |> 
  filter(species == 'Chinstrap') |> 
  nrow() -> penguins_3_table_6Chinstrap_n

# 3.7 Gentoo n값
penguins_2_Chinstrap_Gentoo |> 
  filter(species == 'Gentoo') |> 
  nrow() -> penguins_3_table_7Gentoo_n



# 6 평균 차이의 표준오차 계산
## Gentoo SD

(((penguins_3_table_4Chinstrap_sd^2) / penguins_3_table_6Chinstrap_n) +
  ((penguins_3_table_5Gentoo_sd^2) / penguins_3_table_7Gentoo_n) |> 
  sqrt() -> penguins_3_table_8표준오차)

  
penguins_3_table_1평균차이 / penguins_3_table_8표준오차

























