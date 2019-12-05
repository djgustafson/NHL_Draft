setwd("/Volumes/Samsung USB/NHL/NHL Draft Prediction")

library(rvest)
library(lubridate)

source("rm_accent_fun.R")

url <- "https://www.eliteprospects.com/draft-center/2019?view=stats&sort=tp"

prosp <- read_html(url)

prosp <- prosp %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

player_urls <- read_html(url) %>%
  html_nodes("#players a") %>%
  html_attr("href")

prospd <- prosp[[2]]
prospd <- prospd[-1,-c(1:2)]
prospd <- prospd[-which(prospd$Player==""),]

prospd$name <- gsub("\\(.*", "", prospd$Player)
prospd$goalie <- grepl("\\(G\\)", prospd$Player)
prospd$tag <- trimws(tolower(prospd$name))
prospd$tag <- gsub(" ", "-", prospd$tag)   
prospd$tag <- rm_accent(prospd$tag)
prospd$tag <- iconv(prospd$tag, to='ASCII//TRANSLIT')
prospd$tag <- gsub("\'", "-", prospd$tag)

prospd$url <- NA

for(i in 1:nrow(prospd)){
  for(j in 1:length(player_urls)){
    if(grepl(prospd[i,"tag"], player_urls[j])){
      prospd$url[i] <- player_urls[j]
    }
  }
}

prospd <- na.omit(prospd)
prospd <- prospd[prospd$goalie==FALSE,]

source("EP_scrape_mod_fun.R")


all_data <- list()

pb <- txtProgressBar(min = 0, max = nrow(prospd), style = 3)

for(i in 1:nrow(prospd)){
  page <- prospd$url[i] %>% xml2::read_html()
  
  vitals <- page %>% rvest::html_nodes("[class=\"col-xs-8 fac-lbl-dark\"]") %>% 
    rvest::html_text() %>% magrittr::extract(1:9) %>% 
    stringr::str_squish() %>% purrr::set_names("birthday", "age", "birth_place", 
                                               "birth_country", "youth_team", "position_",
                                               "height", "weight", "shot_handedness") %>% 
    t() %>% as_tibble() %>% mutate(birthday = lubridate::mdy(birthday, 
                                                             quiet = TRUE)) %>% 
    mutate(height = stringr::str_split(height, "\"", simplify = TRUE, n = 2)[, 1]) %>%
    mutate(feet_tall = stringr::str_split(height, "'", simplify = TRUE, n = 2)[, 1]) %>%
    mutate(inches_tall = stringr::str_split(height, "'", simplify = TRUE, n = 2)[, 2]) %>%
    mutate(height = (as.numeric(feet_tall) * 12) + as.numeric(inches_tall)) %>% 
    mutate(weight = stringr::str_split(weight, "lbs", simplify = TRUE, n = 2)[, 1]) %>%
    mutate(name_ = prospd$Player[i]) %>% 
    mutate(player_url_ = prospd$url[i]) %>% 
    mutate_all(~stringr::str_trim(., side = "both")) %>% mutate_all(~na_if(., "-")) %>% 
    mutate_all(~na_if(., "")) %>% select(-c(feet_tall, inches_tall, age, youth_team))
  
  player_statistics <- page %>% rvest::html_node("[class=\"table table-striped table-condensed table-sortable player-stats highlight-stats\"]") %>% 
    rvest::html_table() %>% purrr::set_names("season_", 
                                             "team_", "league_", "games_played_", "goals_", 
                                             "assists_", "points_", "penalty_minutes_", 
                                             "plus_minus_", "blank_", "playoffs_", 
                                             "games_played_playoffs_", "goals_playoffs_",
                                             "assists_playoffs_", "points_playoffs_", 
                                             "penalty_minutes_playoffs_", 
                                             "plus_minus_playoffs_") %>% 
    as_tibble() %>% mutate_all(~na_if(., "-")) %>% 
    mutate(captaincy_ = stringr::str_split(team_, "“", simplify = TRUE, n = 2)[, 2]) %>% 
    mutate(captaincy_ = stringr::str_split(captaincy_,"”", simplify = TRUE, n = 2)[, 1]) %>%
    mutate(team_ = stringr::str_split(team_, "“",simplify = TRUE, n = 2)[, 1]) %>% 
    mutate(season_ = replace(season_, season_ == "", NA)) %>% tidyr::fill(season_) %>% 
    mutate(season_short_ = as.numeric(stringr::str_split(season_, "-", simplify = TRUE, n = 2)[, 1]) + 1) %>% 
    mutate(birthday = vitals[["birthday"]]) %>% 
    mutate(draft_eligibility_date_ = stringr::str_c(as.character(season_short_),"09-15", sep = "-")) %>% 
    mutate(age_ = elite::get_years_difference(birthday,draft_eligibility_date_)) %>%
    mutate_all(stringr::str_squish) %>% 
    mutate_all(as.character) %>% 
    mutate_all(~na_if(., "")) %>% mutate(goals_against_average_ = NA) %>% 
    mutate(save_percentage_ = NA) %>% mutate(goals_against_average_playoffs_ = NA) %>% 
    mutate(save_percentage_playoffs_ = NA) %>% 
    select(-c(blank_, playoffs_, draft_eligibility_date_, 
              birthday)) %>% select(team_, league_, captaincy_, 
                                    season_, season_short_, age_, games_played_, 
                                    goals_, assists_, points_, penalty_minutes_, 
                                    plus_minus_, goals_against_average_, save_percentage_, 
                                    games_played_playoffs_, goals_playoffs_, 
                                    assists_playoffs_, points_playoffs_, 
                                    penalty_minutes_playoffs_, plus_minus_playoffs_,
                                    goals_against_average_playoffs_, 
                                    save_percentage_playoffs_) %>% 
    mutate_at(vars(c(team_,league_, captaincy_, season_)), as.character) %>% 
    mutate_at(vars(-c(team_, league_, captaincy_,season_)), as.numeric) %>% 
    tidyr::nest()
  
  all_data[[i]] <- vitals %>% bind_cols(player_statistics) %>% 
    rename(player_statistics = data)
  setTxtProgressBar(pb, i)
}
close(pb)

all_data <- do.call(rbind, all_data)

stats_full <- all_data %>% tidyr::unnest(player_statistics)

pre_draft <- stats_full %>%
  rename(name=name_, position=position_) %>%
  mutate(birth_date = date(birthday), 
         old_elig = date(paste(draft_year - 1, 9, 16, sep = "-")), 
         young_elig = date(paste(draft_year , 9, 15, sep = "-"))) %>%
  mutate(first_dy = ifelse(date(birth_date + years(18)) %within% 
                             interval(old_elig, young_elig) == T, 1, 0), 
         dy_age = as.numeric(birth_date + years(18) - young_elig)) %>%
  mutate(pre_draft_year = as.numeric(draft_year) - as.numeric(season_short_), 
         captaincy_ = ifelse(captaincy_ == "C", 2, ifelse(captaincy_ == "A", 1, 0))) %>%
  select(-c(goals_against_average_:save_percentage_, 
            goals_against_average_playoffs_:save_percentage_playoffs_)) %>%
  select(-c(birth_place,birthday, player_url_, season_, birth_date, old_elig, young_elig, team_)) %>%
  select(name, position, shot_handedness, birth_country, height, weight, first_dy, dy_age,
         league_:plus_minus_playoffs_, pre_draft_year)

for(i in 1:length(unique(pre_draft$position))){
  pre_draft$name <- gsub(paste('\\(', unique(pre_draft$position)[i], '\\)', sep=""),
                         "", pre_draft$name)
}

saveRDS(pre_draft, "pre_draft_2019.RDS")
