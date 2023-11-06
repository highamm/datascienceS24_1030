library(tidyverse)
library(here)
pokemon <- read_csv(here("data/pokemon_full.csv")) |>
  filter(Type %in% c("Steel", "Dark", "Fire", "Ice"))
set.seed(11232020) ## run this line so that you get the same
## results as I do!

## scale the quantitative predictors
pokemon_scaled <- pokemon |>
  mutate(across(where(is.numeric), ~ (.x - min(.x)) /
                  (max(.x) - min(.x))))

train_sample_2 <- pokemon_scaled |>
  slice_sample(n = 80)
test_sample_2 <- anti_join(pokemon_scaled, train_sample_2)

library(class)

## create a data frame that only has the predictors
## that we will use
train_small <- train_sample_2 |> select(HP, Attack, Defense, Speed,
                                        SpAtk, SpDef, height, weight)
test_small <- test_sample_2 |> select(HP, Attack, Defense, Speed,
                                      SpAtk, SpDef, height, weight)

## put our response variable into a vector
train_cat <- train_sample_2$Type
test_cat <- test_sample_2$Type

knn_mod <- knn(train = train_small, test = test_small,
               cl = train_cat, k = 9)
knn_mod

tab <- table(knn_mod, test_cat)
sum(diag(tab)) / sum(tab)

## 1. Write a function that returns classification rate for
## a user-specified k

get_class_rate <- function(k_val) {
  
  knn_mod <- knn(train = train_small, test = test_small,
                 cl = train_cat, k = k_val)
  knn_mod
  
  tab <- table(knn_mod, test_cat)
  sum(diag(tab)) / sum(tab)
  
}

get_class_rate(k_val = 25)

k_vec <- 1:30
k_vec

class_rate_vec <- map_dbl(k_vec, get_class_rate)
class_rate_df <- tibble(k_vec, class_rate_vec)
ggplot(data = class_rate_df, aes(x = k_vec,
                                 y = class_rate_vec)) +
  geom_line()

