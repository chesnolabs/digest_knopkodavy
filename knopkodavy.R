library(tidyverse)
library(lubridate)

# Спершу треба завантажити в оточення функції з knopkodavy_functions.R

# Записуємо в директорію скачаний з адмінки файл кнопкодавів
# (неособості порушення, не забути відфільтрувати за ВР8 і за опублікованими)
knopk <- read_csv("knopkodavy.csv", locale = locale(encoding = "UTF-8"))
factions <- get_factions_open()

# Місяць, за який робиться дайджест
current_month <- 5 # ввести потрібний
current_year <- 2018

threshold_date_lower <- paste0(current_year, "-",
                         str_pad(current_month, 2, side = "left", pad = "0"),
                         "-01") %>% as.Date()
threshold_date_upper <- threshold_date_lower %m+% months(1)-1

# Отримуємо робочий датафрейм (правильні фракції, фільтр місяця...)
knopk_month <- knopk %>% 
  # select(-faction) %>% 
  rename(fullname = "full name") %>% 
  left_join(factions, by = "fullname") %>% 
  mutate(fullname = str_replace(fullname, "’", "'")) %>%
  filter(as.Date(date)>=threshold_date_lower&
           as.Date(date)<=threshold_date_upper)

# За всю каденцію (використовувати за потреби)
knopk_all <- knopk %>% 
  rename(fullname = "full name") %>% 
  mutate(fullname = str_replace_all(fullname, "\\<U\\+02BC\\>", "'")) %>% 
  left_join(factions, by = "fullname") 

# str_replace("Лук<U+02BC>янчук Руслан Валерійович", "\\<U\\+02BC\\>", "'")
# knopk_all$fullname <- str_replace(knopk_all$fullname, "\\<U\\+02BC\\>", "'")
# knopk_all$faction[grepl("янчук Руслан Валерійович", knopk_all$fullname)] <- "Народний фронт"
# knopk_all$faction[grepl("ян Давид Борисович", knopk_all$fullname)] <- "Блок Петра Порошенка"
# knopk_all$faction[knopk_all$fullname == "Нечаєв Олександр Ігорович"] <- "Опозиційний блок"

# Статистика за особами

knopk_stat <- knopk_month %>% 
  group_by(fullname, faction) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n), faction, fullname) %>% 
  mutate(lastname = str_split(fullname, " ", simplify = T)[, 1],
         firstname = str_split(fullname, " ", simplify = T)[, 2],
        info = (paste0(firstname, " ", lastname, ' ("', faction, '")'))) %>% 
  mutate(info = ifelse(n > 1, paste(info, "-", n), info)) %>% 
  select(-c(firstname, lastname))

# Список для вставлення в дайджест
knopk_stat$info <- paste0(c(1:nrow(knopk_stat)), ". ", knopk_stat$info)
cat(knopk_stat$info, sep = "\n")

# статистика за фракціями
# (увага! не є репрезентативною)
knopk_by_faction <- knopk_stat %>% 
  group_by(faction) %>% 
  summarize(n = sum(n)) %>% 
  arrange(desc(n))

# writing Excel table
month_to_write <- format(threshold_date_lower, "%y%m")
dir.create("output")

write.xlsx(as.data.frame(select(knopk_stat, -info)), file=paste0("output/knopk_VR8_", month_to_write, ".xlsx"),
           sheetName="Кнопкодави ВР8", row.names=FALSE)
write.xlsx(as.data.frame(knopk_by_faction), file=paste0("output/knopk_VR8_", month_to_write, ".xlsx"),
           sheetName="За фракцією", append = T, row.names=FALSE)
