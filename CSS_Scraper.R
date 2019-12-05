setwd("/Volumes/Samsung USB/NHL/NHL Draft Prediction")

library(rvest)
library(dplyr)

#reg <- c('na','eur')
#
#dat_list <- list()
#
#for(y in 2005:2018){
#  for(r in 1:2){
#    print(paste('Draft year:', y, '; Region:', reg[r], sep = " "))
#    
#    if(y == 2005 & r == 2){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2005-2/1889-2/'
#    }
#    
#    else if(y == 2008 & r == 1){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2008-2/2008-2/'
#    }
#    
#    else if(y == 2008 & r == 2){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2008-2/2008-csb-final-na-skaters/'
#    }
#    
#    else if(y == 2013 & r == 2){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2013-2/2013-csb-final-european-skaters/'
#    }
#    
#    else if(y == 2014 & r == 1){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2014-2/csb2014-na-skaters/'
#    }
#    
#    else if(y == 2014 & r ==2){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2014-2/2014-csb-final-european-skaters/'
#    }
#    
#    else if(y == 2015 & r == 1){
#      url <-'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2015-2/2042-2/'
#    }
#    
#    else if(y == 2016 & r == 2){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2016-2/2016-csb-final-euro-skaters/'
#    }
#    
#    else if(y == 2017 & r == 2){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2017-2/2017-csb-final-european-skaters/'
#    }
#    
#    else if(y == 2018 & r == 2){
#      url <- 'https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/2018-2/2018-csb-final-european-skaters/'
#    }
#    
#    else{
#      url <- paste('https://www.thedraftanalyst.com/rankings/year-to-year-central-scouting-rankings/',y,'-2/',y,'-csb-final-',reg[r],'-skaters/', sep = "")
#    }
#      css <- read_html(url)
#     
#      css <- css %>%
#        html_nodes("table") %>%
#        html_table(fill = TRUE)
#      
#      css <- css[[1]]
#      
#      css$year <- y
#      
#      css$reg <- reg[r]
#      
#      dat_list[[length(dat_list) + 1]] <- css
#  }
#}

#css <- plyr::ldply(dat_list, data.frame)

#write.csv(css, 'css_raw.csv', row.names = F)

css <- read.csv('css_raw.csv', stringsAsFactors = F)

css[109,"Name"] <- "Nicholas Kemp"

css$Rank <- ifelse(is.na(css$Rank)==T, css$RANK, css$Rank)
css$Rank <- ifelse(is.na(css$Rank)==T, css$Final, css$Rank)
css$Rank <- ifelse(is.na(css$Rank)==T, css$FINAL, css$Rank)
css$Rank <- ifelse(is.na(css$Rank)==T, css$X1, css$Rank)
css <- css[-which(css$Rank == 'FINAL'),]
css$Rank <- as.numeric(css$Rank)
css <- css[-which(is.na(css$Rank)),]

css$Name <- ifelse(grepl(',', css$Name), paste(sub('.*, ', '', css$Name), sub(',.*', '', css$Name), sep = " "), css$Name)
css$Name <- ifelse(is.na(css$Name)==T, css$NAME, css$Name)
css$Name <- ifelse(grepl(',', css$Name), paste(sub('.*, ', '', css$NAME), sub(',.*', '', css$NAME), sep = " "), css$Name)
css$Name <- ifelse(grepl(',', css$Name), paste(sub('.*,', '', css$NAME), sub(',.*', '', css$NAME), sep = " "), css$Name)
css$Name <- ifelse(is.na(css$Name)==T & is.na(css$FIRST.NAME)==F, paste(css$FIRST.NAME, css$LAST.NAME, sep = " "), css$Name)
css$Name <- ifelse(is.na(css$Name)==T & is.na(css$Player)==F & grepl(',', css$Player), paste(sub('.*,', '', css$Player), sub(',.*', '', css$Player), sep = " "), css$Name)
css$Name <- ifelse(is.na(css$Name)==T & is.na(css$PLAYER)==F & grepl(',', css$PLAYER), paste(sub('.*,', '', css$PLAYER), sub(',.*', '', css$PLAYER), sep = " "), css$Name)
css$Name <- ifelse(is.na(css$Name)==T & is.na(css$X4)==F & grepl(',', css$X4), paste(sub('.*,', '', css$X4), sub(',.*', '', css$X4), sep = " "), css$Name)
css$Name <- ifelse(is.na(css$Name)==T & is.na(css$X3)==F & grepl(',', css$X3), paste(sub('.*,', '', css$X3), sub(',.*', '', css$X3), sep = " "), css$Name)


css$Name <- gsub("[^[:alnum:] ]", "", css$Name)

css <- css[-which(css$year==2005 & css$reg == 'eur'),]

css$na_rank <- ifelse(css$reg == 'na', css$Rank, 1000)
css$eur_rank <- ifelse(css$reg == 'eur', css$Rank, 1000)

css <- css %>% 
  rename(name = Name) %>%
  select(name, year, na_rank, eur_rank) %>%
  mutate(name = tolower(name))

css$name <- ifelse(grepl(" oa", css$name), sub(" oa", '', css$name), css$name)
css$name <- ifelse(grepl(" 2oa", css$name), sub(" 2oa", '', css$name), css$name)
css$name <- ifelse(grepl(" 3oa", css$name), sub(" 3oa", '', css$name), css$name)

euro_05 <- read.csv('css_euro_05.csv', stringsAsFactors = F)
euro_05$year <- 2005
euro_05$na_rank <- 1000
euro_05$eur_rank <- euro_05$rank
euro_05$name <- tolower(euro_05$name)
euro_05$name <- paste(sub('.* ', '', euro_05$name), sub(' .*', '', euro_05$name), sep = " ")
euro_05 <- euro_05[,c('name', 'year', 'na_rank', 'eur_rank')]

css <- rbind(css, euro_05)

cssf <- css[order(css$year),]

## CSS 2019
na_sets <- list()

for(i in 1:5){
  url <- paste("http://www.nhl.com/ice/draftprospectbrowse.htm?cat=1&sort=finalRank&year=2019&pg=",i,sep="")
  
  css <- read_html(url)
  
  css <- css %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  na_sets[[i]] <- css[[2]]
}

na2019 <- do.call(rbind, na_sets)

na2019 <- data.frame(name = na2019$`Player↑`, year = 2019, na_rank = na2019$`Final Rank↑`,
                     eur_rank = 1000)

na2019$name <- as.character(na2019$name)
na2019$na_rank <- as.numeric(as.character(na2019$na_rank))

na2019 <- na2019[is.na(na2019$na_rank)==F,]
na2019 <- na2019[na2019$na_rank<300,]
na2019$na_rank <- 1:nrow(na2019)

eur_sets <- list()

for(i in 1:3){
  url <- paste("http://www.nhl.com/ice/draftprospectbrowse.htm?cat=2&sort=finalRank&year=2019&pg=",i,sep="")
  
  css <- read_html(url)
  
  css <- css %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  
  eur_sets[[i]] <- css[[2]]
}

eur2019 <- do.call(rbind, eur_sets)

eur2019 <- data.frame(name = eur2019$`Player↑`, year = 2019, na_rank = 1000, 
                      eur_rank = eur2019$`Final Rank↑`)

eur2019$name <- as.character(eur2019$name)
eur2019$eur_rank <- as.numeric(as.character(eur2019$eur_rank))

eur2019 <- eur2019[is.na(eur2019$eur_rank)==F,]
eur2019$eur_rank <- 1:nrow(eur2019)

comb2019 <- rbind(na2019,eur2019)

comb2019$name <- tolower(comb2019$name)
comb2019$name <- sub(',', '', comb2019$name)
comb2019$name <- paste(sub('.* ', '', comb2019$name), sub(' .*', '', comb2019$name), 
                       sep = " ")

css <- rbind(cssf, comb2019)

write.csv(css, "css_05_19.csv", row.names = F)
