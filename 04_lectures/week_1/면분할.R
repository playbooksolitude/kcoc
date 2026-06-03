#26-0522 fri 14:46

#
library(tidyverse)
library(palmerpenguins)

#
penguins

ggplot(data = penguins, 
       mapping = aes(
         x = bill_length_mm, 
         y = bill_depth_mm)
       ) +
  geom_point()

#
ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()

#
ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(data = penguins |> select(-species), 
             aes(x = bill_length_mm, y = bill_depth_mm), color = 'snow') +
  geom_point(aes(color = species)) +
  facet_wrap(.~species) +
  theme_minimal()

ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(data = penguins |> select(-species, island), 
             aes(x = bill_length_mm, y = bill_depth_mm), color = 'snow') +
  geom_point(aes(color = species)) +
  facet_grid(island~species) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = 'grey90'))

penguins |> 
  filter(species == 'Gentoo') |> 
  count('Biscoe')

#
# facet ----
mpg |> 
  ggplot() + 
  geom_point(data = mpg |> select(-class), 
             aes(x = displ, y = hwy), color = 'grey', alpha = .7) +
  geom_point(data = mpg, aes(x = displ, y = hwy, color = class), size = 2) +
  facet_wrap(.~class) +
  theme_minimal()