#26-0611 thu 11:14

#
# 필요한 패키지 로드
library(showtext)
showtext_auto()
library(palmerpenguins)
library(tidyverse)

# 시각화 (stat_summary 활용)
penguins |> 
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.8, show.legend = FALSE) +
  stat_summary(fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "errorbar", width = 0.2, linewidth = 0.8) +
  theme_minimal()

#
penguins |> 
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  stat_summary(fun = mean, geom = 'bar') +
  scale_fill_brewer(palette = 'Dark2') +
  stat_summary(fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "errorbar", width = 0.2, linewidth = 0.8) +
  theme_minimal()

# 표준편차 직관 ----
ggplot(mpg, aes(x = class, y = hwy, fill = class)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.8, width = 0.6, show.legend = FALSE) +
  stat_summary(fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "errorbar", width = 0.15, linewidth = 1, color = "black") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

#
mpg |> 
  filter(class %in% c('subcompact', 'pickup')) |> 
  count(class, model)
