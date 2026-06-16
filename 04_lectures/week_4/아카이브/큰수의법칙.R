getwd()


# gif
library(tidyverse)
library(gganimate)
library(gifski)     # GIF 저장을 위해 필요

# 큰수의 법칙 하루 ----
set.seed(123)



# 10분 ----
sim_data_10 <- lapply(1:10, function(i) {
  replicate(144, sample(1:30, 1, replace = FALSE)) |> 
    as.data.frame() |> 
    rename(value = 1) |> 
    mutate(simulation = i)
}) |> 
  bind_rows()

# 애니메이션 만들기
sim_data_10_p <- ggplot(sim_data_10, aes(x = value)) +
  geom_histogram(binwidth = 1, color = "snow", fill = "#1f77b4") +
  scale_x_continuous(breaks = 1:30) +
  labs(title = "Random Sampling Simulation {closest_state} / 10",
       subtitle = "144 draws from 1:30 without replacement each time",
       x = "Value", y = "Count") +
  theme_minimal(base_size = 14) +
  transition_states(simulation, transition_length = 2, state_length = 3) +
  ease_aes('linear')


# GIF로 저장
animate(sim_data_10_p, nframes = 200, fps = 20, 
        width = 1200, height = 700, 
        renderer = gifski_renderer("sampling_simulation_10분.gif"))


#


# 1년 ----
sim_yearly <- lapply(1:10, function(i) {
  replicate(525600, sample(1:30, 1, replace = FALSE)) |> 
    as.data.frame() |> 
    rename(value = 1) |> 
    mutate(simulation = i)
}) |> 
  bind_rows()

# 애니메이션 만들기
sim_yearly_p <- ggplot(sim_yearly, aes(x = value)) +
  geom_histogram(binwidth = 1, color = "snow", fill = "#1f77b4") +
  # geom_label(stat = "bin", binwidth = 1, 
  #            aes(label = after_stat(count)),
  #            vjust = -0.3, size = 3.5) +
  scale_x_continuous(breaks = 1:30) +
  #scale_y_continuous(breaks = c(seq(0,20000,1000))) +
  labs(title = "Random Sampling Simulation {closest_state} / 10",
       subtitle = "525600 draws from 1:30 without replacement each time",
       x = "Value", y = "Count") +
  theme_minimal(base_size = 14) +
  transition_states(simulation, transition_length = 2, state_length = 3) +
  ease_aes('linear')

# GIF로 저장
animate(sim_yearly_p, nframes = 200, fps = 20, 
        width = 1200, height = 700, 
        renderer = gifski_renderer("sampling_simulation_1년.gif"))