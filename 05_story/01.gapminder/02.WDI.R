#26-0301 sun

#https://grok.com/c/7ceabf7f-baae-4b59-86a8-00eb362a54a6?rid=db9d74f2-0c4e-4d3a-ade4-16521cbce347
# obsidian://open?vault=neojohn&file=Clippings%2C%20gapminder%2C%20gdp%2FGrok%202026-03-01T193640%2B0900
library(wbstats)
library(tidyverse)
library(countrycode)   # ISO 코드 → continent 변환에 유용


# Population for every country from 1960 until present
d <- wb_data("SP.POP.TOTL")
d
head(d)



# gapminder ----
(my_indicators <- c(
  life_exp = "SP.DYN.LE00.IN", 
  gdp_capita ="NY.GDP.PCAP.CD", 
  pop = "SP.POP.TOTL"))

(d <- wb_data(my_indicators, 
              start_date = 2016, 
              end_date = 2022))
#(d <- wb_data(my_indicators, start_date = 2016))
d
range(d$date)

### ggplots ----
d %>%
  left_join(wb_countries(), "iso3c") %>%
  ggplot() +
  geom_point(
    aes(
      x = gdp_capita, 
      y = life_exp, 
      size = pop, 
      color = region
    )
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(),
    breaks = scales::log_breaks(n = 10)
  ) +
  #coord_trans(x = 'log10') +
  scale_size_continuous(
    labels = scales::number_format(scale = 1/1e6, suffix = "m"),
    breaks = seq(1e8,1e9, 2e8),
    range = c(1,20)
  ) +
  theme_minimal() +
  labs(
    title = "An Example of Hans Rosling's Gapminder using wbstats",
    x = "GDP per Capita (log scale)",
    y = "Life Expectancy at Birth",
    size = "Population",
    color = NULL,
    caption = "Source: World Bank"
  ) 


#
d |> 
  count(date)

# 1952 ~ 2007 ----
# 사용할 지표 (gapminder와 가장 유사하게 맞춤)
indicators <- c(
  lifeExp   = "SP.DYN.LE00.IN",      # Life expectancy at birth, total (years)
  gdp_Percap_cd = "NY.GDP.PCAP.CD",  # 환율기준, 물가변동 커서 비교 어려움
  gdpPercap = "NY.GDP.PCAP.PP.KD",   # 물가수준 보정, 비교적 정확
  pop       = "SP.POP.TOTL")         # Population, total
  # 만약 current US$를 원하면 "NY.GDP.PCAP.CD" 사용

## gdp 
# NY : National Accounts (국민소득계정) 카테고리
# GDP : Gross Domestic Product (국내총생산)
# PCAP : Per Capita (1인당, 인구로 나눔)
# PP : Purchasing Power Parity (구매력평가, PPP)
# KD : Constant Dollars (불변 가격, 인플레이션 조정됨)
#목적이 gapminder 스타일로 시간에 따른 성장 추이 + 국가 간 비교라면 무조건 NY.GDP.PCAP.PP.KD 써야함
# 명목(current US$)은 "그 해 실제 달러 규모"나 "수출입·채무" 관련할 때만 쓰고,
# 실질 비교·성장 분석에는 PP.KD가 훨씬 정확하고 공정합니다.

# 1960년부터 최신까지 모든 국가 데이터 다운로드
# (gapminder처럼 1952~2007만 원하면 start_date = 1952, end_date = 2007 추가)
wb_raw <- wb_data(
  indicator   = indicators,
  start_date  = 1952,
  end_date    = 2024,
  #gapfill     = FALSE,             # ← 기본값이 FALSE라 생략 가능
  #mrv         = 100,
  country     = "all"
)

wb_raw

gapminder |> 
  pull(country) |> 
  unique() -> gapminder_country

wb_raw |> 
  filter(country %in% gapminder_country) -> wb_raw_2edit
  
wb_raw_2edit |> 
  filter(iso2c %in% c('KR', 'JP'),
         date %in% c(
           '1957', '1967', '1977', '1987', '1997', '2007', '2017', '2023')
         )

gapminder |> 
  filter(country %in% c('Korea, Rep.', 'Japan'),
         year %in% c(
           '1957', '1967', '1977', '1987', '1997', '2007')
         )


# 필요한 컬럼만 정리 + continent 추가 (countrycode 패키지 활용)
gapminder_like <- wb_raw |>
  as_tibble() |>
  #select(
  #  country   = country,
  #  iso2c     = iso2c,               # 또는 iso3c
  #  year      = date,
    #lifeExp   = SP.DYN.LE00.IN,
    #pop       = SP.POP.TOTL,
    #gdpPercap = NY.GDP.PCAP.PP.KD
  #) |>
  mutate(
    continent = countrycode(
      sourcevar   = iso2c,
      origin      = "iso2c",
      destination = "continent"
    )
  ) |>
  relocate(country, continent, year, lifeExp, pop, gdpPercap) |>
  arrange(country, year)

# 결과 미리보기
head(gapminder_like)
glimpse(gapminder_like)


gapminder |> 
  filter(
    agrepl('korea', country)
  )

gapminder |> 
  filter(
    grepl('kor',country, ignore.case = T)
  )

gapminder |> 
  filter(
    grepl('kor',country, ignore.case = T)
  )


