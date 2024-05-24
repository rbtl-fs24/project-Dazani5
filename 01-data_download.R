library(googlesheets4)
rawdata <- read_sheet("https://docs.google.com/spreadsheets/d/15ygeV36SaC2VW5lEJ0N4z7TzT_zPtvmbkAKEDVDmhhg/edit#gid=1070185829")

rawdata <- rawdata %>%
  mutate(`Overall, I think I recycle more frequently at home than at work.` = as.character(`Overall, I think I recycle more frequently at home than at work.`),
         `Overall, I think I generate more waste at home than at work (including recyclables).` = as.character(`Overall, I think I generate more waste at home than at work (including recyclables).`)) %>%
  filter(Timestamp >= as.POSIXct("2024-05-21"))

write.csv(rawdata, "/cloud/project/data/raw/rawdata.csv")
