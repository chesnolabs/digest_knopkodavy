rm(list = ls())
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

get_attendance <- function(excel_file){
  attendance <- read_excel(excel_file)
  names(attendance) <- c("id", "shortname", "start", "end", "voted",
                         "voting_part", "didnotvote", "absent", "total")
  attendance$start <- gsub("Початок повноважень", "", attendance$start)
  attendance$end <- gsub("Припинення повноважень", "", attendance$end)
  attendance$voted <- gsub("Голосував", "", attendance$voted)
  attendance$voting_part <- gsub("Голосував\\(%\\)", "", attendance$voting_part)
  attendance$voting_part <- gsub("%", "", attendance$voting_part)
  attendance$voting_part <- gsub(",", ".", attendance$voting_part)
  attendance$didnotvote <- gsub("Не голосував", "", attendance$didnotvote)
  attendance$absent <- gsub("Відсутній", "", attendance$absent)
  attendance$total <- gsub("Всього", "", attendance$total)
  attendance$shortname[which(attendance$shortname == "Тимошенко Ю.В.")[1]] <- "Тимошенко Юлія В."
  attendance$shortname[which(attendance$shortname == "Тимошенко Ю.В.")] <- "Тимошенко Юрій В."
    attendance <- attendance %>% 
    mutate_at(vars(voted:total), funs(as.numeric)) %>% 
    mutate(absence_v = round((absent/total*100), 1))
  attendance <- attendance %>% select(-id)
  return(attendance)
}

assemble_data <- function(){
  df <- mps %>% 
    select(id, rada_id, gender, district_num, surname, firstname, fullname, shortname, absence_s) %>%
    right_join(factions, by = "fullname") %>% 
    full_join(attendance, by = "shortname") %>% 
    arrange(fullname)
  return(df)
}

add_images_as_xlabels <- function(g, pics) {
  
  ## ensure that the input is a ggplot
  if(!inherits(g, "ggplot")) stop("Requires a valid ggplot to attach images to.")
  
  ## extract the components of the ggplot
  gb   <- ggplot_build(gg)
  xpos <- gb$panel$ranges[[1]]$x.major
  yrng <- gb$panel$ranges[[1]]$y.range
  
  ## ensure that the number of pictures to use for labels 
  ## matches the number of x categories
  if(length(xpos) != length(pics)) stop("Detected a different number of pictures to x categories")
  
  ## create a new grob of the images aligned to the x-axis
  ## at the categorical x positions
  my_g <- do.call("grobTree", Map(rasterGrob, pics, x=xpos, y=0))
  
  ## annotate the original ggplot with the new grob
  gg <- gg + annotation_custom(my_g,
                               xmin = -Inf, 
                               xmax =  Inf,
                               ymax = yrng[1] + 0.25*(yrng[2]-yrng[1])/npoints, 
                               ymin = yrng[1] - 0.50*(yrng[2]-yrng[1])/npoints)
  
  ## turn off clipping to allow plotting outside of the plot area
  gg2 <- ggplotGrob(gg)
  gg2$layout$clip[gg2$layout$name=="panel"] <- "off"
  
  ## produce the final, combined grob
  grid.newpage()
  grid.draw(gg2)
  
  return(invisible(NULL))
  
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

