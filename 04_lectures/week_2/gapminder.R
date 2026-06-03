
#25-0827 wedn 22:43

#
library(tidyverse)
library(gapminder)

#
gapminder |> glimpse()

gapminder |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = country), show.legend = F) +
  scale_y_log10() +
  theme_minimal()

ggplot(gapminder) +
  aes(x = lifeExp, y = gdpPercap, colour = continent,
      size = pop, frame = year) +
  geom_point(alpha = 0.9) +
  scale_y_log10() +
  theme_minimal()

gapminder |> 
  arrange(desc(gdpPercap))

#
gapminder::country_colors |> 
  as.data.frame() |> 
  rownames_to_column('country') |> 
  tibble() |> 
  rename(colors = 2) -> country_colors2

# (gapminder::gapminder |> 
#   left_join(country_colors2, by = 'country') -> gapminder2)
# #sapply(n_distinct)
# 
# #
# gapminder2 |> 
#   filter(year == '2007') |> 
#   select(1,2,5,7) |> 
#   pivot_wider(names_from = continent, values_from = country) |> 
#   ggplot(aes(x = continent, y = country)) +
#   geom_tile(aes(size = pop))


#
gapminder |> 
  filter(continent %in% c('Asia'))

gapminder |> 
  #print(n = 20) |> 
  filter(year == '2007') |> 
  group_by(continent) |> 
  reframe(lifeExp_avg = mean(lifeExp), n = n())

#
gapminder |> 
  filter(year == '2007') |> 
  group_by(continent) |> 
  reframe(lifeExp_avg = mean(lifeExp),
          gdpPercap_avg = mean(gdpPercap), 
          n = n()) |> 
  ggplot(aes(x = continent, y = gdpPercap_avg)) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = round(gdpPercap_avg,1))) +
  labs(title = 'Average mean gdpPercap', subtitle = '') +
  scale_y_continuous(labels = scales::dollar)


#
aggregate(lifeExp ~ continent, gapminder, median)


gapminder |> 
  #filter(continent == 'Asia') |> print(n = Inf)
  filter(year %in% (c('1987', '2007'))) |> 
  #pivot_wider(names_from = year, values_from = lifeExp)
  group_by(continent, year) |> 
  reframe(median(lifeExp), n = n())


#
gapminder %>%
  group_by(continent, country) %>%
  select(country, year, continent, lifeExp) %>%
  mutate(le_delta = lifeExp - lag(lifeExp)) %>%
  summarize(worst_le_delta = min(le_delta, na.rm = TRUE)) %>%
  filter(min_rank(worst_le_delta) < 2) %>%  # Top 1 per continent
  arrange(worst_le_delta)


#
cor.test(gapminder$lifeExp[gapminder$year == 2007], 
         gapminder$gdpPercap[gapminder$year == 2007])

#
(gapminder_with_loggdp <- gapminder %>%
    mutate(log_gdpPercap = log(gdpPercap)))



gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap)) %>%
  mutate(lifeExp_percentile = percent_rank(lifeExp)) %>%
  select(country, gdpPercap, lifeExp, lifeExp_percentile)

ggplot(gapminder, aes(x = gdpPercap, 
                      y = lifeExp, 
                      size = pop, 
                      color = continent)) +
  geom_point(alpha = 0.6) +
  scale_x_log10(labels = scales::dollar) +  # Log scale for GDP
  labs(title = "Life Expectancy vs. GDP per Capita (2007)",
       x = "GDP per Capita (log scale)", y = "Life Expectancy (years)",
       size = "Population", color = "Continent") +
  theme_minimal() +
  facet_wrap(.~continent)


ggplot(subset(gapminder, country == "Afghanistan"), 
       aes(x = year, y = lifeExp)) +
  geom_line() + geom_point() +
  labs(title = "Life Expectancy in Afghanistan Over Time")


ggplot(gapminder, aes(x = lifeExp, fill = continent)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density of Life Expectancy by Continent",
       x = "Life Expectancy (years)")

#
model <- lm(lifeExp ~ year + continent, data = gapminder)
summary(model)


gapminder %>%
  filter(year == 2007, lifeExp >= quantile(lifeExp, 0.9)) %>%
  arrange(desc(lifeExp)) %>%
  slice_head(n = 10)


gapminder %>%
  group_by(year, continent) %>%
  summarise(avg_lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x = year, y = avg_lifeExp, color = continent)) +
  geom_line(size = 3) +
  labs(title = "Average Life Expectancy by Continent Over Time",
       x = "Year", y = "Average Life Expectancy") +
  theme_minimal()

vignette('gapminder')

#
gapminder %>%
  filter(year == 2007) %>%
  filter(continent != 'Oceania') |>  #Oceania 제외
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  labs(title = "Life Expectancy vs. GDP per Capita (2007)") +
  theme_minimal() +
  facet_wrap(.~continent)


gapminder |> 
  count(country)

gapminder |> 
  filter(year %in% c(1952, 2007)) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point() +
  facet_grid(continent~year)




gapminder::gapminder |> 
  filter(year %in% c(1952, 2007)) |> 
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) + 
  #facet_wrap(.~ year)
  facet_grid(continent~year)
