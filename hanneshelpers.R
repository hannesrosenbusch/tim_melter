library(dplyr)
library(tidyr)
id_vars = c("gear")
vars_to_be_stacked_in_one_col = c("am", "vs")
df = mtcars

df_long = df %>% 
  select(c(id_vars, vars_to_be_stacked_in_one_col)) %>%
  pivot_longer(., cols = vars_to_be_stacked_in_one_col, names_to = "variable", values_to = "value")