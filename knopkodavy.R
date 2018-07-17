library(tidyverse)
library(lubridate)

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

knopk_all <- knopk %>% 
  rename(fullname = "full name") %>% 
  mutate(fullname = str_replace_all(fullname, "\\<U\\+02BC\\>", "'")) %>% 
  left_join(factions, by = "fullname") 
  
# str_replace("Лук<U+02BC>янчук Руслан Валерійович", "\\<U\\+02BC\\>", "'")
# knopk_all$fullname <- str_replace(knopk_all$fullname, "\\<U\\+02BC\\>", "'")

knopk_all$faction[grepl("янчук Руслан Валерійович", knopk_all$fullname)] <- "Народний фронт"
knopk_all$faction[grepl("ян Давид Борисович", knopk_all$fullname)] <- "Блок Петра Порошенка"
knopk_all$faction[knopk_all$fullname == "Нечаєв Олександр Ігорович"] <- "Опозиційний блок"

test <- knopk_all %>%
  filter(is.na(faction))

knopk_stat <- knopk_all %>% 
  group_by(fullname, faction) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n), faction, fullname) %>% 
  mutate(lastname = str_split(fullname, " ", simplify = T)[, 1],
         firstname = str_split(fullname, " ", simplify = T)[, 2],
        info = (paste0(firstname, " ", lastname, ' ("', faction, '")'))) %>% 
  mutate(info = ifelse(n > 1, paste(info, "-", n), info)) %>% 
  select(-c(firstname, lastname))
range(knopk_all$date, na.rm = T)

knopk_stat$info <- paste0(c(1:nrow(knopk_stat)), ". ", knopk_stat$info)

cat(knopk_stat$info, sep = "\n")

knopk_by_faction <- knopk_stat %>% 
  group_by(faction) %>% 
  summarize(n = sum(n)) %>% 
  arrange(desc(n))

april <- knopk_month %>% 
  filter(date >= "2018-04-01" & date <= "2018-04-30")
may <- knopk_month %>% 
  filter(date >= "2018-05-01" & date <= "2018-05-31")


# writing Excel table

write.xlsx(as.data.frame(select(knopk_stat, -info)), file="knopkodavy/output/knopk_VR8_180607.xlsx",
           sheetName="Кнопкодави ВР8", row.names=FALSE)
write.xlsx(as.data.frame(knopk_by_faction), file="knopkodavy/output/knopk_VR8_180607.xlsx",
           sheetName="За фракцією", append = T, row.names=FALSE)

# ranking for 8th convocation

knopk <- read_csv("data/knopkodavy.csv")%>% 
  rename(fullname = `full name`) %>% 
  mutate(faction = recode(faction, `Блок Петра Порошенка "Солідарність"` = "Блок Петра Порошенка",
                          `Група "Воля народу"` = "Воля народу",
                          `Група "Партія "Відродження"` = "Відродження",
                          `Радикальна партія Олега Ляшка` = "Радикальна партія Ляшка"))
knopk$fullname <- gsub("ʼ", "'", knopk$fullname)
knopk$faction[knopk$fullname == "Бендюженко Федір Володимирович"] <- "Народний фронт"

knopk_summary <- knopk %>% 
  group_by(fullname) %>% count() %>% 
  left_join(factions, by = "fullname") %>% 
  rename(unpers_vote = n) %>% 
  arrange(desc(unpers_vote)) %>% 
  select(fullname, faction, unpers_vote)

knopk_factions_summary <- knopk %>%
  group_by(faction, fullname) %>% 
  summarize(n = n()) %>% 
  group_by(faction) %>% 
  summarize(number_cases = sum(n), number_voters = n()) %>% 
  left_join(count(factions, faction)) %>% 
  rename(number_persons = n) %>% 
  mutate(percent_voters = round(number_voters/number_persons*100),
         cases_pro_person = round(number_cases/number_persons, 2)) %>% 
  arrange(desc(cases_pro_person))

knopk_vr7 <- read.csv("data/knopkodavy_vr7.csv", encoding = "UTF-8")

two_vrs <- data.frame(cases_vr7 = nrow(knopk_vr7),
                      cases_vr8 = nrow(knopk),
                      persons_vr7 = length(unique(knopk_vr7$full.name)),
                      persons_vr8 = length(unique(knopk$fullname)))

colnames(knopk_summary) <- c("ПІБ", "Фракція", "Кількість фактів")
colnames(knopk_factions_summary) <- c("Фракція", "Кількість фактів", "Кількість кнопкодавів",
                                      "Депутатів у фракції", "% кнопкодавів у фракції",
                                      "Середня кількість фактів на депутата")
colnames(two_vrs) <- c("Фактів у ВР 7", "Фактів у ВР 8",
                             "Кнопкодавів у ВР 7", "Кнопкодавів у ВР 8")

write.xlsx(as.data.frame(knopk_summary), file="knopkodavy/output/knopk_vr8_summary.xlsx",
           sheetName="Кнопкодави ВРУ 8-го скликання", row.names=FALSE)
write.xlsx(as.data.frame(knopk_factions_summary), file="knopkodavy/output/knopk_vr8_summary.xlsx",
           sheetName="Статистика за фракціями", row.names=FALSE, append = T)
write.xlsx(as.data.frame(two_vrs), file="knopkodavy/output/knopk_vr8_summary.xlsx",
           sheetName="ВР 7 vs. ВР 8", row.names=FALSE, append = T)




knopk_full <- read.csv("data/knopkodavy_full.csv", encoding = "UTF-8") %>% 
  rename(fullname = full.name)
knopk_full %>% 
  count(date) %>% 
  arrange(desc(n))

knopk %>% 
  filter(date != "2017-11-09") %>% 
  group_by(fullname) %>% 
  count()
