library(dplyr)
library(tidyr)

data_short <- rawdata %>%
  rename(time = 1,
         participate = 2,
         uni = 3,
         group = 4,
         employment = 5,
         days_office = 6,
         work_facilities = 7,
         work_waste_knowledge = 8,
         home_waste_knowledge = 9,
         home_generate_alu = 10,
         home_generate_pet = 11,
         home_generate_glass = 12,
         home_generate_cardboard = 13,
         home_generate_paper = 14,
         home_generate_plastic = 15,
         home_generate_electric = 16,
         home_generate_organic = 17,
         home_generate_general = 18,
         work_generate_alu = 19,
         work_generate_pet = 20,
         work_generate_glass = 21,
         work_generate_cardboard = 22,
         work_generate_paper = 23,
         work_generate_plastic = 24,
         work_generate_electric = 25,
         work_generate_organic = 26,
         work_generate_general = 27,
         home_recycle_alu = 28,
         home_recycle_pet = 29,
         home_recycle_glass = 30,
         home_recycle_cardboard = 31,
         home_recycle_paper = 32,
         home_recycle_plastic = 33,
         home_recycle_electric = 34,
         home_recycle_organic = 35,
         work_recycle_alu = 36,
         work_recycle_pet = 37,
         work_recycle_glass = 38,
         work_recycle_cardboard = 39,
         work_recycle_paper = 40,
         work_recycle_plastic = 41,
         work_recycle_electric = 42,
         work_recycle_organic = 43,
         gen_more_home = 44,
         gen_more_home_why = 45,
         gen_more_work_why = 46,
         recycle_more_home = 47,
         recycle_less_work_why = 48,
         recycle_less_home_why = 49,
         comments = 50,
         work_dist_alu = 51,
         work_dist_pet = 52,
         work_dist_glass = 53,
         work_dist_cardboard = 54,
         work_dist_paper = 55,
         work_dist_plastic = 56,
         work_dist_electric = 57,
         work_dist_organic = 58,
         work_dist_general = 59) %>%
  filter(grepl("understand", participate)) %>%
  select(-participate) %>%
  mutate(work_facility_code = case_when(
    grepl("prepare", work_facilities) ~ 1,
    grepl("warm", work_facilities) ~ 2,
    grepl("break", work_facilities) ~ 3,
    grepl("no such", work_facilities) ~ 4,
    .default = 99)) %>%
  mutate(gen_more_home = as.character(gen_more_home),
         recycle_more_home = as.character(recycle_more_home))

write.csv(data_short, "/cloud/project/data/processed/data_short.csv")

# Get column names of the dataframe
column_names <- names(data_short)

# Convert column names to a single-column dataframe
dictionary <- data.frame(Column_Names = column_names)

# Write the dataframe to a CSV file
write.csv(dictionary, "/cloud/project/data/final/dictionary.csv", row.names = FALSE)


recycle_less_work_why <- data_short %>%
  filter(recycle_more_home == TRUE) %>%
  select(recycle_less_work_why) %>%
  separate_rows(recycle_less_work_why, sep = ",") %>%
  group_by(recycle_less_work_why) %>%
  summarise(count = n())%>%
  mutate(share = count / sum(count))

recycle_less_home_why <- data_short %>%
  filter(recycle_more_home == FALSE) %>%
  select(recycle_less_home_why) %>%
  separate_rows(recycle_less_home_why, sep = ",") %>%
  group_by(recycle_less_home_why) %>%
  summarise(count = n())%>%
  mutate(share = count / sum(count))

generate_less_work_why <- data_short %>%
  filter(gen_more_home == TRUE) %>%
  select(gen_more_work_why) %>%
  separate_rows(gen_more_work_why, sep = ",") %>%
  group_by(gen_more_work_why) %>%
  summarise(count = n())%>%
  mutate(share = count / sum(count))

generate_less_home_why <- data_short %>%
  filter(gen_more_home == FALSE) %>%
  select(gen_more_home_why) %>%
  separate_rows(gen_more_home_why, sep = ",") %>%
  group_by(gen_more_home_why) %>%
  summarise(count = n())%>%
  mutate(share = count / sum(count))


