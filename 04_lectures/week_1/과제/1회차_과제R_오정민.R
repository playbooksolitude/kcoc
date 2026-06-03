# 26

library(tidyverse)
#install.packages("stringi")

library(tidyverse)

#install.packages("palmerpenguins","ggthemes")

#install.packages("palmerpenguins")

#install.packages("ggthemes")

penguins
glimpse(penguins)
ggplot(data=penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_len, y = body_mass)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mess and flipper length ",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper Length (mm)", y = "Body mass(g)",
    color = "Species", shape = "Species"
  ) 
ggsave(filename = "penguin-plot.png")

ggplot(penguins, aes(x = fct_infreq(species))) + 
  geom_bar()
ggplot(penguins, aes(x = body_mass)) + 
  geom_histogram(binwidth = 200)
  
ggplot(penguins, aes(x = body_mass)) + 
  geom_density()

ggplot(penguins, aes(y = fct_infreq(species))) + 
  geom_bar(color = "red")

ggplot(penguins, aes(y = fct_infreq(species))) + 
  geom_bar(fill = "red")

diamonds
glimpse(diamonds)
ggplot(diamonds, aes(x = carat)) + 
  geom_histogram(binwidth = 0.01)

ggplot(penguins, aes(x = species, y = body_mass)) +
  geom_boxplot()

ggplot(penguins, aes(x = body_mass, colour = species, fill = species)) +
  geom_density(alpha = 0.5)

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
