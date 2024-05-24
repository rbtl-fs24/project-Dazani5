library(ggplot2)
library(tidyr)
library(gt)
library(knitr)
library(forcats)

data_short <- read.csv("/cloud/project/data/processed/data_short.csv")

generation_long <- data_short %>%
  select(contains("home_generate"),
         contains("work_generate"),
         contains("work_recycle"),
         contains("home_recycle")) %>%
  mutate(id = row_number()) %>%
  filter(id != 1) %>%
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
    grepl("general", var) ~ "General")) %>%
  mutate(activity = case_when(
    grepl("generate", var) ~ "generation",
    grepl("recycle", var) ~ "recycling")) %>%
  select(-var) %>%
  pivot_wider(names_from = activity, values_from = value, values_fn = list) %>%
  mutate(generation = case_when(
    generation == "Less than 1 piece" ~ "<1",
    generation == "1-2 pieces" ~ "1-2",
    generation == "3-5 pieces" ~ "3-5",
    generation == "More than 5 pieces" ~ ">5",
    generation == "Don't know" ~ "Don't know" )) %>%
  mutate(recycling = case_when(
    recycling == "Never" ~ "Never",
    recycling == "10% of the time" ~ "10%",
    recycling == "30% of the time" ~ "30%",
    recycling == "60% of the time" ~ "60%",
    recycling == "90% of the time" ~ "90%",
    recycling == "Always" ~ "Always",
    recycling == "Don't know" ~ "Don't know")) %>%
  mutate(generation = as.character(generation),
         recycling = as.character(recycling)) %>%
  mutate(generation = factor(generation, levels = c("<1", "1-2", "3-5", ">5", "Don't know")),
         recycling = factor(recycling, levels = c("Never", "10%", "30%", "60%", "90%", "Always", "Don't know")))



ggplot(generation_long %>% filter(!is.na(recycling)), 
       aes(x = generation, y = recycling, shape = location)) +
  geom_point(color = "black", size = 3) +
  facet_wrap(~material) +
  labs(
    title = NULL,  # If you don't want a title
    x = "Generation (pieces per day)",
    y = "Recycling frequency",
    shape = "Location"
  ) +
  scale_shape_manual(values = c(1, 4)) +  # Shapes: 1 for circle, 4 for X
  theme_minimal()


distance_long <- data_short %>%
  select(contains("work_dist"),
         contains("work_recycle")) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = -id, names_to = "var", values_to = "value") %>%
  mutate(material = case_when(
    grepl("alu", var) ~ "Aluminum",
    grepl("pet", var) ~ "PET",
    grepl("glass", var) ~ "Glass",
    grepl("cardboard", var) ~ "Cardboard",
    grepl("paper", var) ~ "Paper",
    grepl("plastic", var) ~ "Plastic",
    grepl("electric", var) ~ "Electric",
    grepl("organic", var) ~ "Organic",
    grepl("general", var) ~ "General")) %>%
  mutate(activity = case_when(
    grepl("dist", var) ~ "distance",
    grepl("recycle", var) ~ "recycling")) %>%
  select(-var) %>%
  pivot_wider(names_from = activity, values_from = value, values_fn = list) %>%
  mutate(recycling = case_when(
    recycling == "Never" ~ "Never",
    recycling == "10% of the time" ~ "10%",
    recycling == "30% of the time" ~ "30%",
    recycling == "60% of the time" ~ "60%",
    recycling == "90% of the time" ~ "90%",
    recycling == "Always" ~ "Always",
    recycling == "Don't know" ~ "Don't know")) %>%
  mutate(distance = as.character(distance),
         recycling = as.character(recycling)) %>%
  mutate(recycling = factor(recycling, levels = c("Never", "10%", "30%", "60%", "90%", "Always", "Don't know"))) %>%
  mutate(distance = factor(distance, levels = c("0-5 meters", "5-20 meters", "20-50 meters", "50-100 meters", "Over 100 meters", "Don't know")),
         distance = fct_recode(distance,
                               "0-5" = "0-5 meters",
                               "5-20" = "5-20 meters",
                               "20-50" = "20-50 meters",
                               "50-100" = "50-100 meters",
                               "Over 100" = "Over 100 meters",
                               "Don't know" = "Don't know")) %>%
  mutate(distance_cont = case_when(
    distance == "0-5 meters" ~ 2.5,
    distance == "5-20 meters" ~ 12.5,
    distance == "20-50 meters" ~ 35,
    distance == "50-100 meters" ~ 75,
    distance == "Over 100 meters" ~ 125,
    distance == "Don't know" ~ NA)) %>%
  mutate(recycling_cont = case_when(
    recycling == "Never" ~ 0,
    recycling == "10% of the time" ~ 10,
    recycling == "30% of the time" ~ 30,
    recycling == "60% of the time" ~ 60,
    recycling == "90% of the time" ~ 90,
    recycling == "Always" ~ 100,
    recycling == "Don't know" ~ NA))



ggplot(distance_long %>% filter(material != "General"), aes(x=distance, y=recycling, color = material)) +
  geom_point() +
  facet_wrap(~material) +
  theme_minimal() +
  labs(
    title = NULL,  # If you don't want a title
    x = "Distance to bin (meters)",
    y = "Recycling frequency",
    shape = "Location"
  )

ggplot(distance_long, aes(x=distance_cont, y=recycling_cont)) +
  geom_point() +
  facet_wrap(~material) +
  theme_minimal()



facility_more <- data_short %>%
  select(("work_facilities"),
         ("work_waste_knowledge"),
         ("home_waste_knowledge"),
         ("gen_more_home"),
         ("recycle_more_home")) %>%
  mutate(gen_more_home = case_when(
    gen_more_home == "TRUE" ~ "True",
    gen_more_home ==  "FALSE" ~ "False",
    TRUE ~ gen_more_home
  )) %>%
  mutate(recycle_more_home = case_when(
    recycle_more_home == "TRUE" ~ "True",
    recycle_more_home ==  "FALSE" ~ "False",
    TRUE ~ recycle_more_home
  ))

gt(distance_long %>% 
     select(-id) %>%
     group_by(material, distance) %>%
     summarise(count = n())
   )

kable(generation_long %>% 
     select(-id) %>%
     group_by(material, recycling, location) %>%
     summarise(count = n())
)

gt(facility_more %>% 
        group_by(work_facilities) %>%
        summarise(count = n())
)

gt(facility_more %>% 
     group_by(gen_more_home) %>%
     summarise(count = n()) %>%
     rename("Generate more waste at home?" = gen_more_home,
            "Answers" = count)
)

gt(facility_more %>% 
     group_by(recycle_more_home) %>%
     summarise(count = n())%>%
     rename("Recycle more waste at home?" = recycle_more_home,
            "Answers" = count)
)

gt(facility_more %>% 
     group_by(home_waste_knowledge) %>%
     summarise(count = n())
)

gt(facility_more %>% 
     group_by(work_waste_knowledge) %>%
     summarise(count = n())
)

