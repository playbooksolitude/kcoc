#26-0619


#
library(tidyverse)
#
penguins |> 
    group_by(species) |> 
    reframe(mean_length = round(mean(bill_length_mm, na.rm = T),1))

penguins |> 
  group_by(species) |> 
  reframe(mean_length = mean(bill_length_mm, na.rm = T),
          mean_depth = mean(bill_depth_mm, na.rm = T),
          mean_mass = mean(body_mass_g, na.rm = T))

(penguins |> 
    group_by(species) |> 
    reframe(mean_length = round(mean(bill_length_mm, na.rm = T),1),
            mean_depth = round(mean(bill_depth_mm, na.rm = T), 1),
            mean_mass = round(mean(body_mass_g, na.rm = T), 1),
            n = n()) -> penguins_1_table)
penguins_1_table |>                                           
  pivot_longer(cols = !species,
               names_to = "type", 
               values_to = "value") 

#
penguins_1_table |>                                           
  pivot_longer(cols = !species,
               names_to = "type", 
               values_to = "value") |> 
  ggplot(aes(x = species, y = value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(.~type, scale = 'free_y') +
  geom_label(aes(label = value))                  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))


#
penguins |> 
  drop_na() |> 
  ggplot(aes(x = bill_length_mm, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  facet_grid(sex~ island)

library(readxl)
read_xlsx("./99.files/jerrybag.xlsx") -> jerry_1xlsx
jerry_1xlsx |> view()
jerry_1xlsx |> glimpse()
jerry_1xlsx |> str()
jerry_1xlsx |> 
  sapply(n_distinct) |> 
  enframe()


library(showtext)
showtext_auto()

jerry_1xlsx |> 
  count(`Age Group`) |> 
  ggplot(aes(x = `Age Group`, y = n)) +
  geom_bar(stat = 'identity') +
  bbplot::bbc_style() +
  geom_label(aes(label = n), size = 7)

jerry_1xlsx |> 
  count(`Customer Type`, `Age Group`) |> 
  ggplot(aes(x = `Customer Type`, y = `Age Group`, 
             fill = n))  +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow', size = 7) +
  # bbplot::bbc_style() +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 17), 
    axis.title = element_blank(),
    legend.position = 'none',
    plot.title = element_text(size = 28), 
    plot.subtitle = element_text(size = 22, just = .1)
  ) +
  labs(title = "jerrybag", subtitle = '2026.06.19') 

#
jerry_1xlsx |> 
  filter(`Age Group` %in% c("20대", "30대", 
                            "40대", "50대")) |> 
  ggplot(aes(x = Date)) +
  geom_histogram(color = 'snow', aes(fill = Gender)) +
  #facet_wrap(.~`Age Group`) +
  facet_grid(`Age Group`~`Customer Type`) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_blank(),
    # legend.position = 'none',
    plot.title = element_text(size = 28), 
    plot.subtitle = element_text(size = 22, just = .1)
  ) +
  scale_fill_brewer(palette = 'Set1', direction = -1)
  
#
jerry_1xlsx |> 
  #colnames()
  rename(
    디자인 = "Bag Type", 
    연령대 = "Age Group", 
    고객유형 = "Customer Type", 
    성별 = "Gender"
  ) -> jerry_2rename

jerry_2rename |> view()

jerry_2rename |> 
  count(디자인)

jerry_2rename |> 
  sapply(n_distinct) |> 
  enframe()

jerry_2rename |> 
  count(디자인, sort = T) |> 
  #slice(1:10)
  filter(n > 15) |> 
  ggplot(aes(x = 디자인 |> fct_reorder(n), 
             y = n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  bbplot::bbc_style() +
  ggtitle(label = '15개 이상 판매된 디자인', subtitle = "")
  #print(n = 30)
  ggplot(aes(x = n ))
  #geom_histogram()
  #geom_density()
  

  
jerry_1xlsx |> 
  #colnames()
  rename(
    디자인 = "Bag Type", 
    연령대 = "Age Group", 
    고객유형 = "Customer Type", 
    성별 = "Gender"
  ) |> 
  count(디자인, sort = T) |> 
  mutate(
    디자인_case = 
      case_when(
      디자인 == '맨인블랙 웨이스트 블랙' ~ '소', 
      디자인 == '타폴린 퀼팅 메신저 카키' ~ '중',
      .default = T
    )
    )




jerry_1xlsx





  #facet_wrap(.~`Age Group`) +
  facet_grid(`Age Group`~`Customer Type`) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12), 
    axis.title = element_blank(),
    # legend.position = 'none',
    plot.title = element_text(size = 28), 
    plot.subtitle = element_text(size = 22, just = .1)
  ) +
  scale_fill_brewer(palette = 'Set1')

# 면분할, 성별, 연령  
  jerry_1xlsx |> 
    filter(`Age Group` %in% c("20대", "30대", 
                              "40대", "50대")) |> 
    ggplot(aes(x = Date)) +
    geom_histogram(color = 'snow', aes(fill = Gender)) +
    #facet_wrap(.~`Age Group`) +
    facet_grid(`Age Group`~Gender) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12), 
      axis.title = element_blank(),
      # legend.position = 'none',
      plot.title = element_text(size = 28), 
      plot.subtitle = element_text(size = 22, just = .1)
    ) +
    scale_fill_brewer(palette = 'Set1', direction = -1)
  
jerry_2rename |> 
  select(-last_col()) |> 
  count(`Date Time`)

jerry_1xlsx |> view()

#
table1
table2  
table3
table4a
table4b

table1 |> 
  ggplot(aes(x = country, y = population)) +
  geom_bar(stat = 'identity', 
           aes(fill = factor(year)), 
           position = 'dodge') +
  scale_y_continuous(labels = scales::comma)

#
table2 |> 
  ggplot(aes(x =country, y = count)) +
  geom_bar(stat = 'identity', aes(fill = type))

#
table1 |> 
  pivot_longer(cols = c(cases, population), 
               names_to = 'type', 
               values_to = 'count')

# 
table2
library(palmerpenguins)
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F)


#
G20
library(treemapify)

G20
ggplot(data = G20, aes(area = gdp_mil_usd, 
                       fill = hdi)) +
  geom_treemap()

#
data(package = 'treemapify')

















