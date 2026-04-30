#26-0428

library(tidyverse)
library(palmerpenguins)
library(MetBrewer)

penguins
penguins |> 
  filter(is.na(flipper_length_mm))

#
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()

#
mpg |> 
  distinct(.keep_all = T)

#모두 동일한  것을 찾을 때
mpg |> 
  group_by_all() |> 
  filter(n() > 1)

mpg |> 
  filter(n() > 1)

# table ----
penguins |> 
  count(species)

penguins |> 
  count(species, island)

penguins |> 
  count(species, island) |> 
  pivot_wider(names_from = 'species', values_from = n)

penguins |> 
  count(species, island) |> 
  ggplot(aes(x = species, y = n, fill = island)) +
  geom_bar(stat = 'identity')


penguins |> 
  count(species, island) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = 'snow', size = 12)

# 경우의 수 추가 1 ----
penguins |> 
  count(species, island) |> 
  complete(species, island, fill = list(n = 0)) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = 'snow', size = 12) 

# 경우의 수 추가 2  
penguins |> 
  count(species, island) |> 
  pivot_wider(names_from = 'species', values_from = n) |> 
    replace_na(list(Adelie = 0, Chinstrap = 0, Gentoo = 0)) |> 
  pivot_longer(cols = c(Adelie:Gentoo), 
               names_to = 'species', 
               values_to = 'n') |> 
    ggplot(aes(x = species, y = island, fill = n)) +
    geom_tile() +
    geom_text(aes(label = n), color = 'snow', size = 12) 

# 꾸미기   
penguins |> 
  count(species, island) |> 
  pivot_wider(names_from = 'species', values_from = n) |> 
  replace_na(list(Adelie = 0, Chinstrap = 0, Gentoo = 0)) |> 
  pivot_longer(cols = c(Adelie:Gentoo), 
               names_to = 'species', 
               values_to = 'n') |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile(color = 'snow', size = 2) +
  geom_text(aes(label = n), color = 'snow', size = 12)  +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18), 
    panel.grid = element_blank(), 
    axis.title = element_text(size = 22), 
    legend.position = 'none'
  ) 

# 전역 + 로컬 ----
penguins |> 
  ggplot(aes(x = sex, y = after_stat(count))) +
  geom_bar(aes(fill = species), position = 'dodge')  +
  geom_label(aes(x = sex, y = after_stat(count), 
                 group = species,
                 label = after_stat(count)), 
                 stat = 'count',
             position = position_dodge(.9), 
             size = 6) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1')  +
  theme(
    strip.text = element_text(size = 30),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 18), 
    panel.grid = element_blank(), 
    axis.title = element_text(size = 22), 
    legend.position = 'none'
  ) +
  facet_wrap(.~island)


# 면분할 ----
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  facet_wrap(species~.)


penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species)) +
  facet_grid(island~sex) +
  theme(
    legend.position = 'top'
  )

#
penguins |> 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(se = F, method = 'lm')



#
  penguins |> 
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm, 
               color = species)) +
    geom_point() +
    geom_smooth(se = F, method = 'lm') +
  scale_color_brewer(palette = 'Set1')
  

#  
penguins |> 
  count(island, species, sex) |> 
  ggplot(aes(x = sex, y = n)) +
  geom_bar(stat = 'identity', aes(fill = island)) +
  facet_wrap(.~species)
  
  
#pallete
MetBrewer::colorblind_palettes
MetBrewer::display_all()

penguins |> 
  count(island, species, sex) |> 
  ggplot(aes(x = sex, y = n)) +
  geom_bar(stat = 'identity', aes(fill = island)) +
  facet_wrap(.~species) +
  MetBrewer::scale_fill_met_d(name = 'VanGogh1')





