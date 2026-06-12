#26-0610 wedn

library(palmerpenguins)
library(GGally)
library(bbplot)
library(showtext)
showtext_auto()
penguins

#
penguins |> 
  is.na() |> 
  colSums() |> 
  enframe()

#
penguins |> 
  is.na() |> 
  colSums()

palmerpenguins::penguins_raw

penguins |> 
  filter(
    if_any(everything(),is.na)
  )



library(naniar)

penguins |> 
  vis_miss()

penguins_1_number |> 
  count(species) |> 
  ggplot(aes(x = species, y= n)) +
  geom_bar(stat = 'identity') +
  geom_label(aes(label = n), size = 5) +
  #theme_minimal()
  #theme_dark()
  #theme_bw()
  #theme_classic()
  theme_void()



penguins |> 
  get_dupes()

mpg |> 
  get_dupes()

diamonds |> 
  get_dupes()

penguins_1_number |> 
  count(species, island)


penguins_1_number |> 
  count(species, island) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile()

penguins_1_number |> 
  count(species, island) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile() + 
  geom_text(aes(label = n), size = 6, color = 'snow') +
  bbc_style()



penguins_1_number |> 
  count(species, island, sex) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile() + 
  facet_wrap(.~sex, ncol = 1) +
  geom_text(aes(label = n), size = 6, color = 'snow')


penguins_1_number |> 
  count(species, island) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile() + 
  geom_text(aes(label = n), size = 6, color = 'snow') +
  bbc_style() +
  theme(legend.position = 'none') +
  labs(title = '종족별 서식지 분포', 
       subtitle = "")


penguins_1_number |> 
  count(species, island, sex) |> 
  complete(species, island, sex, fill = list(n = 0))

#
penguins_1_number |> 
  filter(is.na(sex))

# facet ----
penguins_1_number |> 
  drop_na(sex) |> 
  count(species, island, sex) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile() + 
  geom_text(aes(label = n), size = 6, color = 'snow') +
  bbc_style() +
  theme(legend.position = 'none') +
  labs(title = '종족별 서식지 분포', 
       subtitle = "성별 NA 11마리 제외") +
  facet_wrap(.~sex)


#
penguins_1_number |> 
  count(species, island, sex) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile() + 
  geom_text(aes(label = n), size = 6, color = 'snow') +
  bbc_style() +
  theme(legend.position = 'none') +
  labs(title = '종족별 서식지 분포', 
       subtitle = "333마리") +
  facet_wrap(.~sex)

#
penguins_1_number |> 
  drop_na(sex) |> 
  count(species, island, sex) |> 
  ggplot(aes(x = species, y = island, fill = n)) +
  geom_tile() + 
  geom_text(aes(label = n), size = 6, color = 'snow') +
  bbc_style() +
  theme(legend.position = 'none') +
  labs(title = '종족별 서식지 분포', 
       subtitle = "333마리 (sex NA 11마리 제외)") +
  facet_wrap(.~sex)


penguins_1_number |> 
  ggplot(aes(x = bill_length_mm)) +
  geom_histogram()


penguins_1_number |> 
  ggplot(aes(x = bill_length_mm, fill = species)) +
  geom_histogram() +
  facet_wrap(.~species, ncol = 1)

#
penguins_1_number |> 
  ggplot(aes(x = bill_length_mm, fill = species)) +
  geom_histogram(color = 'snow') +
  facet_wrap(.~species, ncol = 1) +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal()


penguins_1_number |> 
  filter(species == 'Gentoo') |>
  drop_na(sex) |> 
  ggplot(aes(x = bill_length_mm, fill = sex)) +
  geom_histogram(color = 'snow') +
  facet_wrap(.~sex, ncol = 1) +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal()


penguins_1_number |> 
  filter(species == 'Gentoo') |>
  drop_na(sex) |> 
  ggplot(aes(x = bill_length_mm, fill = sex)) +
  geom_density(alpha = .7)  +
  facet_wrap(.~species, ncol = 1) +
  scale_fill_brewer(palette = 'Set1') +
  theme_minimal()



penguins |> 
  ggpairs(columns = c("bill_length_mm", 
                      "bill_depth_mm", 
                      "flipper_length_mm", 
                      "body_mass_g", 
                      "species"), 
          aes(color = species)) 


# 1 ----
# 데이터셋의 변수별 결측치 비율/건수 시각화
penguins |> 
  is.na() |> 
  colSums() |> 
  enframe(name = "변수명", value = "결측치_건수") |> 
  ggplot(aes(x = reorder(변수명, 결측치_건수), y = 결측치_건수)) +
  geom_bar(fill = "steelblue", stat = 'identity') +
  geom_text(aes(label = 결측치_건수), hjust = -0.2) +
  coord_flip() +
  labs(title = "변수별 결측치(Missing Value) 현황 파악", 
       x = "", 
       y = "결측치 건수") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12)
  )

penguins |> 
  ggpairs(columns = c("bill_length_mm", 
                      "bill_depth_mm", 
                      "flipper_length_mm", 
                      "body_mass_g",
                      "species"), 
          aes(color = species)
  ) +
  theme(
    strip.text = element_text(size = 16),
    axis.text = element_text(size = 10)
  ) +
  theme_bw()


