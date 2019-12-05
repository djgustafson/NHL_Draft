rm(list=ls())

setwd("/Volumes/Samsung USB/NHL/NHL Draft Prediction")

library(dplyr)

full <- read.csv("NHL_draft_05_18_full.csv", stringsAsFactors = F)

pre_draft_agg <- read.csv("NHL_Draft_05_19_agg.csv")
source("rm_accent_fun.R")
pre_draft_agg$name <- as.character(pre_draft_agg$name)
pre_draft_agg$name <- gsub("[^[:alnum:] ]", "", pre_draft_agg$name)
pre_draft_agg$name <- rm_accent(pre_draft_agg$name)
pre_draft_agg$name <- iconv(pre_draft_agg$name, to='ASCII//TRANSLIT')
pre_draft_agg$name <- trimws(pre_draft_agg$name)
pre_draft_match <- pre_draft_agg[,c("name", "ss")]

css <- read.csv("css_05_19.csv", stringsAsFactors = F)
css$ss <- css$year
css$name <- trimws(css$name)

## I guess average them out? Figure out how to separate same names in future
css$na_rank <- ifelse(css$na_rank==1000, NA, css$na_rank)
css$eur_rank <- ifelse(css$eur_rank==1000, NA, css$eur_rank)

css <- css %>%
  group_by(name) %>%
  summarize(year = max(year), na_rank = mean(na_rank, na.rm=T), 
            eur_rank = mean(eur_rank, na.rm=T), ss = max(ss))

css <- as.data.frame(css)

source("css_name_changes.R")

d <- plyr::join(pre_draft_match, css, by = c('name', 'ss'))

d$oa <- ifelse(d$name %in% css$name & is.na(d$na_rank)==T,1,0)

for(i in 1:nrow(d)){
  if(d[i,'oa']==1){
    d[i,'year'] <- css[which(css$name == d[i,'name']),'year']
    d[i,'na_rank'] <- css[which(css$name == d[i,'name']),'na_rank'] 
    d[i,'eur_rank'] <- css[which(css$name == d[i,'name']),'eur_rank']
  }
}

d$na_rank <- ifelse(is.na(d$na_rank)==T, 250, d$na_rank)
d$eur_rank <- ifelse(is.na(d$eur_rank)==T, 250, d$eur_rank)

d <- d[,c("name", "na_rank", "eur_rank")]

pre_draft_agg <- plyr::join(pre_draft_agg, d, by = "name")




forwards <- pre_draft_agg %>%
  filter(position != "D")

defense <- pre_draft_agg %>%
  filter(position == "D")
  
  
  
  
post_draft <- full %>%
  filter(season_short_ > draft_year, league_ == "NHL", position != "G") %>%
  select(name, draft_year, season_short_, team_, games_played_, goals_, assists_, points_) %>%
  group_by(name) %>%
  summarise(games_played = sum(games_played_, na.rm = T), points = sum(points_, na.rm = T), 
            ppg = sum(points_, na.rm = T) / sum(games_played_, na.rm = T), 
            draft_year = mean(draft_year))

post_draft$name <- as.character(post_draft$name)
post_draft$name <- gsub("[^[:alnum:] ]", "", post_draft$name)
post_draft$name <- rm_accent(post_draft$name)
post_draft$name <- tolower(post_draft$name)

post_draft$pot_games_played <- ifelse(post_draft$draft_year < 2013,
                                      (2019-post_draft$draft_year) * 82 - 34,
                                      (2019-post_draft$draft_year) * 82)

post_draft$gp_per <- post_draft$games_played/post_draft$pot_games_played
post_draft$ppg <- ifelse(post_draft$games_played < 20, 0, post_draft$ppg)
post_draft$ppg_per <- post_draft$ppg/max(post_draft$ppg)
post_draft$comb <- (post_draft$gp_per + post_draft$ppg_per)/max(post_draft$gp_per + post_draft$ppg_per)

## Working with WAR from Evolving Hockey
war <- read.csv("EH_WAR.csv")
war <- war %>%
  mutate(name=as.character(player))%>%
  select(name, WAR) %>%
  group_by(name) %>%
  summarise(tot_war=sum(WAR)) %>%
  mutate(name = tolower(name))

init_index <- which(stringr::str_count(war$name, pattern = "\\.") > 1)
war[init_index,"name"] <- c('aj.greer', 'bj.crombeen','cj.smith', 'jt.brown', 
                              'jt.compher', 'jt.miller', 'martin.stlouis', 'pa.parenteau',
                              'pj.axelsson', 'pk.subban', 'rj.umberger', 'tj.galiardi',
                              'tj.hensick', 'tj.oshie')

war[51,'name'] <- 'alexandre.grenier'
war[74,'name'] <- 'alexei.marchenko'
war[154,'name'] <- 'artyom.anisimov'
war[489,'name'] <- 'danny.oregan'
war[547,'name'] <- 'denis.guryanov'
war[584,'name'] <- 'dmitri.kulikov'
war[585,'name'] <- 'dmitri.orlov'
war[746,'name'] <- 'ilya.zubov'
war[752,'name'] <- 'ivan.barbashyov'
war[1386,'name'] <- 'mikhail.sergachyov'
war[1388,'name'] <- 'mikkel.bodker'
war[1396,'name'] <- 'mirco.muller'
war[1399,'name'] <- 'mitchell.marner'
war[1469,'name'] <- 'nikolai.kulyomin'
war[1482,'name'] <- 'oliver.ekman larsson'
war[1573,'name'] <- 'phil.di giuseppe'
war[1608,'name'] <- 'richard.clune'
war[1621,'name'] <- 'rinat.valiyev'
war[1721,'name'] <- 'samuel.blais'
war[1794,'name'] <- 'stanislav.galiyev'
war[1832,'name'] <- 'sven.bartschi'
war[1848,'name'] <- 'teodors.blugers'
war[1789,'name'] <- 'vyacheslav.voynov'
war[662,'name'] <- 'yevgeni.dadonov'
war[663,'name'] <- 'yevgeni.grachyov'
war[664,'name'] <- 'yevgeni.kuznetsov'
war[666,'name'] <- 'yevgeni.svechnikov'

war$name <- gsub('\\.', ' ',war$name)

war$name <- gsub("[^[:alnum:] ]", "", war$name)

## Oh god need to fix names
war$name <- ifelse(grepl('alex ', war$name), gsub('alex ', 'alexander ', war$name), 
                   war$name)
war$name <- ifelse(grepl('evgeny ', war$name), gsub('alex ', 'alexander ', war$name), 
                   war$name)

war$tot_war <- ifelse(is.na(war$tot_war),0,war$tot_war)

post_draft$name <- ifelse(grepl('alex ', post_draft$name), 
                          gsub('alex ', 'alexander ', post_draft$name), post_draft$name)
post_draft$name <- iconv(post_draft$name, to='ASCII//TRANSLIT')

post_draft <- plyr::join(post_draft, war, by = "name")

post_draft$war_per82 <- (post_draft$tot_war/post_draft$pot_games_played) * 82


target <- data.frame(name = post_draft$name, ppg = post_draft$ppg, 
                     gp_per = post_draft$gp_per, comb = post_draft$comb, 
                     war = post_draft$war_per82, draft_year = post_draft$draft_year)

target$name <- as.character(target$name)

target[is.na(target)] <- 0


samp <- pre_draft_agg

post_samp <- target %>%
  select(name, war, draft_year)

full_samp <- plyr::join(samp, post_samp, "name")

full_samp[is.na(full_samp)] <- 0

write.csv(full_samp, "NHL_Draft_05_19.csv", row.names = F)