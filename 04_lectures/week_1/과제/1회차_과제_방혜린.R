# 2026.05.28

library(tidyverse)

# Penguins
#install.packages("palmerpenguins")
library(palmerpenguins)
#install.packages("ggthemes")
library(ggthemes)
penguins
glimpse(penguins)

library(ggplot2)
ggplot(data = penguins)

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x= "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()


# 산점도 bill_depth_mm - bill_length_mm 
ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, 
                     y = bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# 산점도 species - bill_depth_mm > boxplot
ggplot(data = penguins,
       mapping = aes(x = species, 
                     y = bill_depth_mm)
       ) +
  geom_boxplot()

# Error in `geom_point()` > x,y값 추가 필요
ggplot(data = penguins) +
  geom_point()


ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE )


#그래프 같음
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
)

# 파이프기호 ct+sh+m
penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# 범주형 변수
ggplot(penguins, aes(x = species)) +
  geom_bar()

ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

# 수치형 변수
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)


ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# Species -> y
ggplot(penguins, aes(y = species)) +
  geom_bar()

# color / fill 
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

#밀도 함수 그래프
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)

ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)
# alpha는 투명도


# 범주형 변수 관계
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

# 수치형 변수 관계
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

# 셋 이상 변수 관계
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

#facet별 분할
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

# mpg pck
mpg

ggplot(
  data = mpg,
  mapping = aes(x = hwy, y = displ)
) +
  geom_point(aes(color = manufacturer))

# 범례 하나로
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm,
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species", shape = "Species")

# 다름
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")


# png 저장 - 산점도, why..?
ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")

