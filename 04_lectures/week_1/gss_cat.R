#26-05205 wedn 08:13

#
library(tidyverse)

#
gss_cat |> 
  sapply(n_distinct)

gss_cat |> 
  str()

#
gss_cat |> 
  count(marital, race) |> 
  complete(marital, race, fill = list(n = NA)) |> 
  ggplot(aes(x = race, y = marital, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow')


# 항목에는 있지만 실제 답변은 없었던 경우
gss_cat |> 
  count(race, rincome) |> 
  complete(race, rincome, fill = list(n = 0)) |> 
  ggplot(aes(x = race, y = rincome, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow')

gss_cat |> 
  count(race)

levels(gss_cat$race)

#과소 표본 찾는 법
gss_cat |> 
  count(relig, rincome) |> 
  complete(relig, rincome, fill = list(n = NA)) |> 
  ggplot(aes(x = relig, y = rincome, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow') +
  scale_fill_gradient(na.value = 'tomato3') +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .4)
  )


?gss_cat


#
gss_cat |> 
  count(relig, rincome) |> 
  complete(relig, rincome, fill = list(n = NA)) |> 
  ggplot(aes(x = relig, y = rincome, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow') +
  scale_fill_gradient(na.value = 'tomato3') +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .4)
  )


#
mpg |> 
  count(drv, class) |> 
  ggplot(aes(x = drv, y = class, fill = n)) +
  geom_tile()

mpg |> 
  count(drv, class) |> 
  complete(drv, class, fill = list(n = 0)) |> 
  ggplot(aes(x = drv, y = class, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow') +
  theme_minimal()


#
mpg |> 
  count(drv, class) |> 
  complete(drv, class, fill = list(n = NA)) |> 
  ggplot(aes(x = drv, y = class, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color = 'snow') +
  theme_minimal() +
  scale_fill_gradient(na.value = 'grey90')



#
set.seed(123)
diamonds |> 
  slice_sample(n = 1000) -> diamonds_small

mpg |> 
  count(drv, fl) |> 
  complete(drv, fl, fill = list(n = 0)) |> 
  ggplot(aes(x = drv, y = fl, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color ='snow') +
  ggtitle(label = 'mpg')

#
mpg |> 
  count(class, fl) |> 
  complete(class, fl, fill = list(n = 0)) |> 
  ggplot(aes(x = class, y = fl, fill = n)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = n), color ='darkblue') +
  ggtitle(label = 'mpg') +
  scale_fill_gradient(low = 'snow3', high = 'red')

#
mpg |> 
  count(class, drv) |> 
  complete(class, drv, fill = list(n = 0)) |> 
  ggplot(aes(x = class, y = drv, fill = n)) +
  geom_tile(color = 'black') +
  geom_text(aes(label = n), color ='darkblue') +
  ggtitle(label = 'mpg') +
  scale_fill_gradient(low = 'snow1', high = 'red') +
  theme_classic()


# msleep ----
msleep |> #str() 
  count(vore, conservation) |> # 4 * 6 (NA 포함시 5 * 7)
  complete(vore, conservation, fill = list(n = 0)) |> 
  #print(n = Inf) #각각 NA를 포함하므로
  ggplot(aes(x = vore, y = conservation, fill = n)) +
  geom_tile(color = 'black') +
  geom_text(aes(label = n), color = 'snow') +
  scale_fill_gradient(high = 'red', low = 'lightblue3')

#mpg
mpg |> distinct()
mpg |> #str()
  count(drv, fl, class) |> #drv 3 fl 5 class 7
  complete(drv, fl, class, fill = list(n = NA)) |> 
  drop_na() |> 
  ggplot(aes(x = fl, y = class, fill = n)) +
  geom_tile(color = 'snow') +
  facet_wrap(.~drv) +
  geom_text(aes(label = n), color = 'snow')

3*5*7

# na 처리1 ----
mpg |> #str()
  count(drv, fl, class) |> #drv 3 fl 5 class 7
  pivot_wider(names_from = class, values_from = n, values_fill = 0)

# na 처리2 ----
mpg |> #str()
  count(drv, fl, class) |> #drv 3 fl 5 class 7
  pivot_wider(names_from = class, values_from = n, values_fill = 0)


#
mpg |> 
  count(drv, class) |> 
  pivot_wider(names_from = drv, values_from = n)

# complete(n = NA) ----
## n = NA
mpg |> 
  count(drv, class) |> 
  complete(drv, class, fill = list(n = NA)) |> 
  #print(n = Inf)
  ggplot(aes(x = drv, y = class, fill = n)) +
  geom_tile(color = 'grey70') +
  scale_fill_gradient(low = 'grey90', high = 'red', na.value = 'snow') +
  geom_text(aes(label = n), color = 'black')

# complete(n = 0) ----
## n = 0
mpg |> 
  count(drv, class) |> 
  complete(drv, class, fill = list(n = 0)) |> 
  ggplot(aes(x = drv, y = class, fill = n)) +
  geom_tile(color = 'grey70', show.legend = F) +
  scale_fill_gradient(low = 'grey90', high = 'red', na.value = 'snow') +
  geom_text(aes(label = n), color = 'black') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 18)) +
  labs(title = 'complete()', subtitle = "", caption = "")



