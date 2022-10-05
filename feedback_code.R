library(tidyverse)
heatmap_df <- tibble(challenge = c(3, 3, 1, 2, 4, 3, 2, 3, 2,
                                   3, 2, 3, 1, 3, 1, 2, 2, 3,
                                   2, 1, 3, 3, 2),
                     time = c(2, 3, 1, 3, 3, 3, 1, 2, 1,
                              3, 1, 3, 1, 3, 2, 3, 2, 3,
                              1, 2, 2, 2, 1)) |>
  mutate(challenge = as.factor(challenge),
         time = as.factor(time)) |>
  mutate(time = fct_rev(fct_expand(time, "4")))

heatmap_sum <- heatmap_df |> 
  group_by(challenge, time, .drop = FALSE) |>
  summarise(n_student = n())

ggplot(heatmap_sum, aes(x = challenge, y = time,
                        fill = n_student)) +
  geom_tile() +
  scale_fill_viridis_c(direction = 1) +
  theme_minimal()
