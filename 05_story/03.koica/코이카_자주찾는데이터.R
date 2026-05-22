#26-0310 

#
library(showtext)
showtext_auto()
library(tidyverse)
read_csv("./01.kcoc/05.files/income_year.csv") -> kcoc_1year

kcoc_1year

kcoc_1year |> 
  ggplot(aes(x = Category)) +
  theme_minimal() + 
  
  geom_point(aes(y = `기타저소득국(LICs)`)) +
  geom_line(aes(y = `기타저소득국(LICs)`)) +
  geom_point(aes(y = `최빈국(LDCs)`)) +
  geom_line(aes(y = `최빈국(LDCs)`)) +
  geom_point(aes(y = `중저소득국(LMICs)`)) + 
  geom_line(aes(y = `중저소득국(LMICs)`)) +
  geom_point(aes(y = `고중소득국(UMICs)`)) +
  geom_line(aes(y = `고중소득국(UMICs)`)) +
  geom_point(aes(y = 미분류)) +
  geom_line(aes(y = 미분류))
  
kcoc_1year |> 
  pivot_longer(cols = !1, 
               names_to = '소득수준', 
               values_to = '지원액') |> 
  ggplot(aes(x = Category, y = 지원액, color = 소득수준))  +
  geom_line(size = 3) + 
  geom_point(size = 7, aes(shape = 소득수준)) +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  scale_color_brewer(palette = 'Set1')
  
#
read_csv("./01.kcoc/05.files/income_compare.csv") -> kcoc_2compare

kcoc_1year
kcoc_2compare |> 
  pivot_longer(cols = !1, 
               names_to = '소득수준', 
               values_to = '지원액') |> 
  ggplot(aes(x = factor(Category), y = 지원액, fill = 소득수준)) +
  geom_bar(stat = 'identity', position = 'fill')  +
  coord_flip() +
  theme_minimal() +
  scale_fill_brewer(palette = 'Set3')

mpg |> 
  group_by(class) |> 
  reframe(avg_hwy = mean(hwy),
          avg_cty = mean(cty), 
          max_hwy = max(hwy),
          max_cty = max(cty), 
          min_hwy = min(hwy),
          min_cty = min(cty)) |> 
  ggplot() +
  geom_segment(aes(x = min_cty, xend = max_cty, y = class, yend = class, 
                   color = class), 
               size = 2) +
  geom_point(aes(x = min_cty, y = class, color = class), size = 4) +
  geom_point(aes(x = max_cty, y = class, color = class), size = 4) +
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  theme(legend.position = 'none')
  
  
