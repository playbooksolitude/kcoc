#26-0523 sat 21:51

#
library(tidyverse)
tidyverse_update()
library(googlesheets4)
library(showtext)
showtext_auto()

#
read_sheet("https://docs.google.com/spreadsheets/d/1KTi8EWWmHcbykuf8xN2Vh0s0kniLJ6TneKkYmc9K88U/edit?gid=1835542775#gid=1835542775", 
           sheet = "구분 피봇 테이블 1", skip = 1) -> koica_단기연수_1sheet

#
koica_단기연수_1sheet |> 
  pivot_longer(cols = c(강의:총계), 
               names_to = '구분', 
               values_to = '값') -> koica_단기연수_2pivot

#
koica_단기연수_2pivot |> 
  ggplot(aes(x = 지역, y = 구분, fill = 값)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = scales::comma(값)), color ='snow') +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.line = element_blank()
  ) 

#
koica_단기연수_2pivot |> 
  filter(구분 != '총계') |> 
  filter(지역 != '총계') |> 
  ggplot(aes(x = 지역, y = 구분, fill = 값)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = scales::comma(값)), color ='snow') +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.line = element_blank()
  ) 

#
koica_단기연수_2pivot |> 
  filter(구분 != '총계') |> 
  filter(지역 != '총계') |> 
  replace_na(list(값 = 0)) |> 
  ggplot(aes(x = 지역, y = 구분, fill = 값)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = scales::comma(값)), color ='snow') +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.line = element_blank()
  ) 

#
koica_단기연수_2pivot |> 
  filter(구분 != '총계') |> 
  filter(지역 != '총계') |> 
  replace_na(list(값 = 0)) |> 
  ggplot(aes(x = 지역, y = 구분, fill = 값)) +
  geom_tile(color = 'grey50') +
  geom_text(aes(label = scales::comma(값)), color ='black') +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    axis.line = element_blank()
  )  +
  #colorspace::scale_fill_continuous_sequential('Viridis')
  colorspace::scale_fill_continuous_sequential('Peach')

colorspace::hcl_palettes(plot = T)


