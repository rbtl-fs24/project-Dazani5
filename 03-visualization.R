library(ggplot2)
library(tidyr)

generation_long <- data_short %>%
  select(contains("home_generate"),
         contains("work_generate"),
         contains("work_recycle"),
         contains("home_recycle")) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "var", values_to = "value") %>%
  mutate(location = case_when(
    grepl("home", var) ~ "Home",
    grepl("work", var) ~ "Work")) %>%
  mutate(material = case_when(
    grepl("alu", var) ~ "Aluminum",
    grepl("pet", var) ~ "PET",
    grepl("glass", var) ~ "Glass",
    grepl("cardboard", var) ~ "Cardboard",
    grepl("paper", var) ~ "Paper",
    grepl("plastic", var) ~ "Plastic",
    grepl("electric", var) ~ "Electric",
    grepl("organic", var) ~ "Organic",
    grepl("general", var) ~ "General"))
  

ggplot(data_short, aes(x=uni, y=))