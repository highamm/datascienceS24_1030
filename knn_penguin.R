library(palmerpenguins)
library(tidyverse)
library(class)

set.seed(11072022)
penguins_scaled <- penguins |>
  filter(complete.cases(penguins)) |>
  mutate(across(where(is.numeric),  ~ (.x - min(.x)) /
                  (max(.x) - min(.x))))
penguins_train <- penguins_scaled |> slice_sample(n = 200)
penguins_test <- anti_join(penguins_scaled, penguins_train)

train_knn <- penguins_train |> select(bill_length_mm)
test_knn <- penguins_test |> select(bill_length_mm)

train_penguin_spec <- penguins_train |> pull(species)
test_penguin_spec <- penguins_test |> pull(species)

get_class_rate <- function(k = 10) {

knn_penguin <- knn(train = train_knn, test = test_knn,
                   cl = train_penguin_spec, k = k)

res_tab <- table(knn_penguin, test_penguin_spec)
sum(diag(res_tab)) / sum(res_tab)
}
get_class_rate(k = 15)

class_rates <- map(1:50, get_class_rate) |> unlist()
class_rate_df <- tibble(ks = 1:50, class_rates)

ggplot(data = class_rate_df,
       aes(x = ks, y = class_rates)) +
  geom_line()
class_rate_df |> filter(class_rates == max(class_rates))
## task 1: make a plot of class_rates vs. ks
## task 2: write code to find k with largest classification
## rate