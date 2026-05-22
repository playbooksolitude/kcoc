#26-0301 sun 


#https://grok.com/c/7ceabf7f-baae-4b59-86a8-00eb362a54a6?rid=1aaacbc0-3864-4384-a60e-9f05192a827e

#
library(tidyverse)
library(gapminder)
library(scales)   # 천단위 쉼표, 퍼센트 등에 유용
library(showtext)
showtext_auto()

gapminder |> colnames()

#
gapminder |> 
  group_by(year, continent) |> 
  reframe(avg_gdpPercap = mean(gdpPercap)) |> 
  pivot_wider(names_from = continent, values_from = avg_gdpPercap) |> 
  filter(year %in% c(1952, 2007)) |> 
  pivot_longer(cols = !1, 
               names_to = 'continent', 
               values_to = 'gdp') |> 
  ggplot(aes(x = continent, y = gdp, fill = factor(year))) +
  geom_bar(stat = 'identity', position = position_dodge())


# 데이터 준비 (이미 주신 wide 형태 그대로 사용 가능)
(gap_wide <- gapminder |>
  group_by(year, continent) |>
  summarise(avg_gdpPercap = mean(gdpPercap), .groups = "drop") |>
  pivot_wider(names_from = continent, values_from = avg_gdpPercap))

# long 형태로도 자주 쓰이므로 미리 준비
(gap_long <- gapminder |>
  group_by(year, continent) |>
  summarise(avg_gdpPercap = mean(gdpPercap), .groups = "drop"))


# 3 ggplot
### line ----
ggplot(gap_long, aes(x = year, y = avg_gdpPercap, color = continent)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = dollar_format(scale = 1, prefix = "")) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "대륙별 1인당 GDP 평균 추이 (1952~2007)",
    x = "연도", 
    y = "평균 1인당 GDP (국제 달러)",
    color = "대륙"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")


###
ggplot(gap_long, aes(x = year, y = avg_gdpPercap)) +
  geom_line(color = "#2c7bb6", linewidth = 1.2) +
  geom_point(color = "#2c7bb6", size = 2.5) +
  facet_wrap(~ continent, scales = "free_x", ncol = 3) +
  scale_y_continuous(labels = dollar_format(scale = 1, prefix = "")) +
  labs(
    title = "대륙별 1인당 GDP 추이 (각각 독립 y축)",
    x = "연도",
    y = "평균 1인당 GDP (국제 달러)"
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold", size = 12))


#
# wide 형태를 long으로 다시 변환해 사용 (또는 직접 pivot_longer 없이도 가능)
gap_long |>
  mutate(year = factor(year)) |>
  ggplot(aes(x = year, y = continent, fill = avg_gdpPercap)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(avg_gdpPercap, 0)), color = "black", size = 3.5) +
  scale_fill_viridis_c(option = "C", direction = 1, 
                       labels = dollar_format(scale = 1, prefix = "")) +
  labs(
    title = "대륙별 · 연도별 평균 1인당 GDP 히트맵",
    x = "연도", 
    y = NULL,
    fill = "평균 GDP per capita"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"))

#
# 먼저 CAGR 계산
cagr <- gap_long |>
  filter(year %in% c(1952, 2007)) |>
  pivot_wider(names_from = year, values_from = avg_gdpPercap) |>
  mutate(
    years = 2007 - 1952,
    cagr = (`2007` / `1952`) ^ (1 / years) - 1
  ) |>
  arrange(desc(cagr))

# 플롯
ggplot(cagr, aes(x = reorder(continent, cagr), y = cagr)) +
  geom_col(aes(fill = continent), width = 0.7) +
  geom_text(aes(label = scales::percent(cagr, accuracy = 0.01)), 
            vjust = -0.4, fontface = "bold", size = 4) +
  scale_y_continuous(labels = percent_format(accuracy = 1), 
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(
    title = "1952→2007 대륙별 연평균 성장률 (CAGR)",
    x = NULL,
    y = "연평균 성장률 (CAGR)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank())
