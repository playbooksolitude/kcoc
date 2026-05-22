#26-0516 sat 20:48


source("./p_load.R")
library(tidyverse)
library(datasauRus)
library(writexl)
library(readxl)
library(openintro)

#
penguins |> 
  mutate(
    bill_length_mm = replace_na(bill_length_mm, 0),
    #sex = replace_na(sex, 'unknown')
    sex = fct_na_value_to_level(sex, 'unknown')
  )


datasaurus_dozen |> tibble()
datasaurus_dozen_wide

datasaurus_dozen |> str()
datasaurus_dozen |> 
  write_xlsx("./chapter01/datasaurus.xlsx")
  

read_excel("./99.files/datasaurus_kcoc.xlsx") -> datasaurus_1_excel
datasaurus_1_excel

# openintro
starbucks |> 
  write_xlsx("./99.files/starbucks.xlsx")

burger |> 
  write_xlsx("./99.files/burgers.xlsx")

datasaurus_dozen |> 
#  filter(dataset == 'away') |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(.~dataset)

# burger
burger |> print(n = 30)


burger |> 
  mutate(number = row_number(), 
         .before = 1) |> 
  pivot_wider(names_from = best_burger_place, values_from = gender) |> 
  replace_na(list('Fat Burger' = '0'))
  mutate(Other = replace_na(Other , '0'))
  

?replace_na
