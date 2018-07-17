library(tidyverse)
library(stringr)
library(readxl)
library(jsonlite)
library(xlsx)

get_all_mps <- function(){
  mps <- fromJSON(readLines(file("http://data.rada.gov.ua/ogd/mps/skl8/mps-data.json",
                                 encoding = "UTF-16")))
  mps_list <- mps
  mps <- mps_list[[1]]
  mps$fullname <- with(mps, paste(surname, firstname, patronymic))
  mps$shortname <- paste0(mps$surname, " ", 
                          str_sub(mps$firstname, 1, 1), ".", 
                          str_sub(mps$patronymic, 1, 1), ".")
  mps$shortname <- gsub("'", "’", mps$shortname)
  mps$shortname[mps$fullname == "Тимошенко Юлія Володимирівна"] <- "Тимошенко Юлія В."
  mps$shortname[mps$fullname == "Тимошенко Юрій Володимирович"] <- "Тимошенко Юрій В."
  mps$fullname[mps$surname == "Найєм"] <- "Найєм Мустафа-Масі"
  mps$shortname[mps$surname == "Найєм"] <- "Найєм М. ."
  mps$fullname[mps$surname == "Джемілєв"] <- "Джемілєв Мустафа"
  mps$shortname[mps$surname == "Джемілєв"] <- "Джемілєв М. ."
  mps$absence_s <- with(mps,
                        round(presentAuto_absent/(presentAuto_absent+presentAuto_present)*100, 1))
  mps$mazhor <- ifelse(is.na(mps$district_num), "l", "m")
  return(mps)
}

get_factions_open <- function(){
  posts <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mp-posts_ids.csv")
  posts_ids <- read_tsv("http://data.rada.gov.ua/ogd/mps/skl8/mp-posts_unit.txt", 
                        locale(encoding = "windows-1251"), col_names = F, col_types = NULL) %>% 
    rename(unit_id = X1, unit = X2)
  mps <- read_csv("http://data.rada.gov.ua/ogd/mps/skl8/mps08-data.csv")
  
  factions_full <- posts %>% 
    left_join(mps[, c("id", "full_name")], by = c("mp_id" = "id")) %>% 
    left_join(posts_ids) %>% 
    filter(unit_type %in% c("grp", "fra")) %>% 
    select(mp_id, full_name, unit)
  
  factions_df <-  mps %>% 
    filter(is.na(resignation_text)) %>% 
    select(id, full_name) %>% 
    left_join(factions_full, by = c("id" = "mp_id", "full_name")) %>% 
    mutate(unit = ifelse(is.na(unit), "Позафракційні", unit)) %>% 
    rename(faction = unit, fullname = full_name) %>% 
    mutate(faction = recode(faction,
                            `Група "Воля народу"` = "Воля народу",
                            `Група "Партія "Відродження"` = "Відродження",
                            `Група "Відродження"` = "Відродження",
                            `Група "Економічний розвиток"` = "Економічний розвиток",
                            `Фракція ПАРТІЇ "БЛОК ПЕТРА ПОРОШЕНКА"` = "Блок Петра Порошенка",
                            `Фракція політичної партії "Всеукраїнське об'єднання "Батьківщина" у Верховній Раді України` = "Батьківщина",
                            `Фракція Політичної партії "НАРОДНИЙ ФРОНТ"` = "Народний фронт",
                            `Фракція Політичної партії "Опозиційний блок" у Верховній Раді України восьмого скликання` = "Опозиційний блок",
                            `Фракція Радикальної партії Олега Ляшка` = "Радикальна партія Ляшка",
                            `Фракція Політичної партії "Об'єднання "САМОПОМІЧ"` = "Самопоміч"))
  return(factions_df)
}

