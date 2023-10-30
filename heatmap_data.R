library(tidyverse)
heatmap_df <- tibble::tibble(time = c("Third", "Third", "Most Time", "Second",
                                      "Third", "Second", "Second", "Least Time", "Second", "Second",
                                      "Most Time", "Third", "Second", "Third", "Second", "Least Time",
                                      "Third", "Least Time", "Third", "Third", "Third", "Third", "Third",
                                      "Least Time", "Second", "Third", "Third", "Third", "Second",
                                      "Second", "Second", "Second", "Second", "Third", "Least Time",
                                      "Third", "Second", "Second", "Third", "Third", "Most Time", "Second",
                                      "Third", "Third", "Third", "Second", "Second", "Second", "Second",
                                      "Third", "Third", "Second"),
                             challenge = c("Third", "Third", "Most Challenge", "Least Challenge",
                                           "Second", "Third", "Most Challenge", "Least Challenge",
                                           "Third", "Second", "Most Challenge", "Third",
                                           "Most Challenge", "Third", "Third", "Least Challenge",
                                           "Third", "Least Challenge", "Third", "Third",
                                           "Least Challenge", "Third", "Third", "Least Challenge",
                                           "Most Challenge", "Second", "Second", "Third",
                                           "Most Challenge", "Least Challenge", "Most Challenge",
                                           "Most Challenge", "Second", "Third", "Least Challenge",
                                           "Third", "Second", "Second", "Third", "Second",
                                           "Most Challenge", "Most Challenge", "Most Challenge",
                                           "Third", "Third", "Most Challenge", "Least Challenge",
                                           "Second", "Third", "Third", "Least Challenge", "Second"))
heatmap_sum <- heatmap_df |> 
  mutate(challenge = fct_relevel(challenge, c("Most Challenge",
                                              "Second", 
                                              "Third",
                                              "Least Challenge")),
         time = fct_relevel(time, c("Most Time",
                                    "Second", 
                                    "Third",
                                    "Least Time")) |> fct_rev()) |>
  group_by(time, challenge, .drop = FALSE) |>
  summarise(n_students = n())

## use geom_tile() to make heatmap
ggplot(data = heatmap_sum, aes(x = challenge, y = time)) +
  geom_tile(aes(fill = n_students)) +
  scale_fill_viridis_c() +
  scale_x_discrete(position = "top")




heatmap_start2 <- tibble::tibble(challenge = c("Most Challenge", "Second", "Third",
                                               "Least Challenge"),
                                 `Least Time` = c(0, 0, 0, 5),
                                 Third = c(1, 4, 16, 2), Second = c(8, 6, 4, 3),
                                 `Most Time` = c(3, 0, 0, 0))
heatmap_start2 |> pivot_longer(2:5, names_to = "time",
                               values_to = "n_students")
## do the same fct releveling and make the heatmap
