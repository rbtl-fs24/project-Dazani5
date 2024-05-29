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

# # Get column names of the dataframe
# column_names <- names(data_short)
# 
# # Convert column names to a single-column dataframe
# dictionary <- data.frame(Column_Names = column_names) %>%
#   rename("variable_name" = Column_Names) %>%
#   mutate(description = c(
#     "Time the survey was submitted",
#     "University at which the respondent works",
#     "Research group at which the respondent works",
#     "Level of employment",
#     "Number of days spent in the office per week",
#     "Cooking facilities available at the office",
#     "Knowledge of recycling facilities at the office",
#     "Knowledge of recycling facilities at home",
#     "Generation of waste at home (aluminum), in pieces per day",
#     "Generation of waste at home (PET), in pieces per day",
#     "Generation of waste at home (glass), in pieces per day",
#     "Generation of waste at home (cardboard), in pieces per day",
#     "Generation of waste at home (paper), in pieces per day",
#     "Generation of waste at home (plastic), in pieces per day",
#     "Generation of waste at home (electric), in pieces per day",
#     "Generation of waste at home (organic), in pieces per day",
#     "Generation of waste at home (general), in pieces per day",
#     "Generation of waste at the office (aluminum), in pieces per day",
#     "Generation of waste at the office (PET), in pieces per day",
#     "Generation of waste at the office (glass), in pieces per day",
#     "Generation of waste at the office (cardboard), in pieces per day",
#     "Generation of waste at the office (paper), in pieces per day",
#     "Generation of waste at the office (plastic), in pieces per day",
#     "Generation of waste at the office (electric), in pieces per day",
#     "Generation of waste at the office (organic), in pieces per day",
#     "Generation of waste at the office (general), in pieces per day",
#     "Recycling of waste at home (aluminum), in percent",
#     "Recycling of waste at home (PET), in percent",
#     "Recycling of waste at home (glass), in percent",
#     "Recycling of waste at home (cardboard), in percent",
#     "Recycling of waste at home (paper), in percent",
#     "Recycling of waste at home (plastic), in percent",
#     "Recycling of waste at home (electric), in percent",
#     "Recycling of waste at home (organic), in percent",
#     "Recycling of waste at the office (aluminum), in percent",
#     "Recycling of waste at the office (PET), in percent",
#     "Recycling of waste at the office (glass), in percent",
#     "Recycling of waste at the office (cardboard), in percent",
#     "Recycling of waste at the office (paper), in percent",
#     "Recycling of waste at the office (plastic), in percent",
#     "Recycling of waste at the office (electric), in percent",
#     "Recycling of waste at the office (organic), in percent",
#     "Whether the participant generates more waste at home versus the office",
#     "Why the participant generates more waste at home versus the office",
#     "Why the participant generates more waste at the office versus at home",
#     "Whether the participant recycles more waste at home versus the office",
#     "Why the participant recycles less waste at the office versus at home",
#     "Why the participant recycles less waste at home versus at the office",
#     "Additional comments the participant may have",
#     "Walking distance to aluminum recycling bin (in meters) at the office",
#     "Walking distance to PET recycling bin (in meters) at the office",
#     "Walking distance to glass recycling bin (in meters) at the office",
#     "Walking distance to cardboard recycling bin (in meters) at the office",
#     "Walking distance to paper recycling bin (in meters) at the office",
#     "Walking distance to plastic recycling bin (in meters) at the office",
#     "Walking distance to electric recycling bin (in meters) at the office",
#     "Walking distance to organic recycling bin (in meters) at the office",
#     "Walking distance to general waste bin (in meters) at the office",
#     "A code for the cooking facility available at work: 1 = full facility to cook; 2 = can only heat food; 3 = break room only without food preparation or heating; 4 = no break room at all"
#   ))
# 
# 
# # Write the dataframe to a CSV file
# write.csv(dictionary, "/cloud/project/data/processed/dictionary.csv", row.names = FALSE)


# recycle_less_work_why <- data_short %>%
#   filter(recycle_more_home == TRUE) %>%
#   select(recycle_less_work_why) %>%
#   separate_rows(recycle_less_work_why, sep = ",") %>%
#   group_by(recycle_less_work_why) %>%
#   summarise(count = n())%>%
#   mutate(share = count / sum(count))
# 
# recycle_less_home_why <- data_short %>%
#   filter(recycle_more_home == FALSE) %>%
#   select(recycle_less_home_why) %>%
#   separate_rows(recycle_less_home_why, sep = ",") %>%
#   group_by(recycle_less_home_why) %>%
#   summarise(count = n())%>%
#   mutate(share = count / sum(count))
# 
# generate_less_work_why <- data_short %>%
#   filter(gen_more_home == TRUE) %>%
#   select(gen_more_work_why) %>%
#   separate_rows(gen_more_work_why, sep = ",") %>%
#   group_by(gen_more_work_why) %>%
#   summarise(count = n())%>%
#   mutate(share = count / sum(count))
# 
# generate_less_home_why <- data_short %>%
#   filter(gen_more_home == FALSE) %>%
#   select(gen_more_home_why) %>%
#   separate_rows(gen_more_home_why, sep = ",") %>%
#   group_by(gen_more_home_why) %>%
#   summarise(count = n())%>%
#   mutate(share = count / sum(count))


