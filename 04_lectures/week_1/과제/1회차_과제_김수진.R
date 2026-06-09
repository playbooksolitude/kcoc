#26-0528 Thu 22:42

library(tidyverse)
library(palmerpenguins)

# glimpse(penguins)



ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) + geom_point()

#
ggplot(data = penguins,
       mapping = aes(X = flipper_length_mm, 
                     y = body_mass_g)) + geom_point()

# ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g, colour = species))
+ geom_point() + geom_smooth(method = "lm")

ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(mapping = aes(colour = species)) + 
  geom_smooth(method = "lm")

ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(mapping = aes(colour = species, shape = species)) + 
  geom_smooth(method = "lm") + 
  labs(title = "Body mass and flipper length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins", 
       x = "Flipper length(mm)", y = "Body mass(g)", color = "Species", 
       shape = "Species") + scale_color_colorblind()

ggplot(data = penguins, 
       mapping = aes(x = bill_depth_mm, y = bill_depth_mm)) + 
  geom_point()

ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point()

ggplot(data = penguins) + geom_point()

ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(na.rm = TRUE) + 
  labs(caption = "palmerpenguins 패키지에서 데이터를 가지고 왔습니다.")
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(mapping = aes(colour = bill_depth_mm)) +
  geom_smooth(mathod = "lm")

ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(mapping = aes(colour = bill_depth_mm), na.rm = TRUE) +
  geom_smooth(mathod = "lm")

ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)) + 
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point() + geom_smooth()

ggplot() + 
  geom_point(data = penguins, 
             mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_smooth(data = penguins, 
              mapping = aes(x = flipper_length_mm, y = body_mass_g))

# ggplot2 호출하기
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + geom_point(
# 분포시각화하기
ggplot(penguins, aes(y = species)) + geom_bar()
ggplot(penguins, aes(x = species)) + geom_bar(color = 'red')
ggplot(penguins, aes(x = species)) + geom_bar(fill = 'red')
ggplot(penguins, aes(x = body_mass_g)) + geom_histogram(bins = 20)
ggplot(diamonds, aes(x = carat)) + geom_histogram(binwidth = 20)

# 변수의 관계 시각화하기
ggplot(penguins, aes(x = species, y = body_mass_g)) + geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, color = species)) + geom_density(linewidth = 0.75)
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) 
+ geom_density(alpha = 0.5)
ggplot(penguins, aes(x = islands, fill = species)) + 
  geom_bar(position = "fill")
ggplot(data = penguins, aes(x = island, fill = species)) + 
  geom_bar(position = "fill")

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species, shape = species)) + 
  facet_wrap(~island)

#연습문제
ggplot(mpg, aes(x = hwy, y = displ)) + geom_point()
ggplot(mpg, aes(x = hwy, y = displ)) + geom_point(aes(color = year))
ggplot(mpg, aes(x = hwy, fill = displ)) + geom_bar(fill = 'blue')
ggplot(mpg, aes(x = hwy)) + 
  geom_density(linewidth = 0.75)
ggplot(mpg, aes(x = hwy, y = displ)) + 
  geom_point(aes(color = cty)) + 
  facet_wrap(~manufacturer)

ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(mapping = aes(colour = species))

ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(mapping = aes(colour = species)) + facet_wrap(~island)

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = bill_depth_mm, 
                                      color = species, shape = species)) +
  geom_point() + labs(color = "Species")

ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = bill_depth_mm, 
                                      color = species, shape = species)) +
  geom_point() +
  labs(color = "Species", shape = "Species")

ggplot(penguins, aes(x = island, fill = species)) + 
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) + 
  geom_bar(position = "fill")
ggsave(filename = "penguin-plot.png")
ggplot(mpg, aes(x = class)) + geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) + geom_point() 
ggsave("mpg-plot.png")
ggplot(mpg, aes(x = cty, y = hwy)) + geom_point() 
ggsave("mpg-plot.pdf")