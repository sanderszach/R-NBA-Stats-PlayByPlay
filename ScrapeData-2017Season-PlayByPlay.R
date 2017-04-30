### Updated 20170425
### Scrapes play-by-play NBA data
### Compiles all play-by-play data into master data frame

library(shiny)
library(RSelenium)
library(rvest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RJSONIO)

#First install selenium and setup driver
remDr <- RSelenium::rsDriver(port = 4448L,browser="chrome") #start selenium browser

###--- Get Game IDs ---###
gameList <- lapply(seq(as.Date('2016-10-25'),Sys.Date()-1,'days'),function(X){
  url <- paste0("http://stats.nba.com/js/data/widgets/boxscore_breakdown_",format(as.Date(X),'%Y%m%d'),".json")
  print(X)
  remDr$client$navigate(url)
  raw_source <- remDr$client$getPageSource(url)
  #remDr$server$stop()
  text <- read_html(raw_source[[1]]) %>% 
    html_nodes("pre") %>% 
    html_text()
  json_text <- fromJSON(text)$results
  if(length(json_text)>0){
    recent_games <- as.data.frame(t( sapply(1:length(json_text),function(X){ c(json_text[[X]]$Game,json_text[[X]]$GameID,json_text[[X]]$HomeTeam[3],json_text[[X]]$VisitorTeam[[3]]) }) ))
    colnames(recent_games) <- c('Game','GameID','Home','Visitor')
    return(recent_games)
  }
})
gameFrame <- do.call(rbind.data.frame, gameList)



###---Get Game Data---###
#takes about 20 minutes to scrape play-by-play data

gameDetail <- lapply(gameFrame$GameID,function(X){
  url <- paste0("http://stats.nba.com/stats/playbyplayv2?EndPeriod=10&GameID=",X,"&StartPeriod=1")
  print(X)
  remDr$client$navigate(url)
  raw_source <- remDr$client$getPageSource(url)
  
  text <- read_html(raw_source[[1]]) %>% 
    html_nodes("pre") %>% 
    html_text()
  
  json_text <- fromJSON(text)$resultSets[[1]]
  
  df <- as.data.frame(t(sapply(1:length(json_text$rowSet), function(X){
    json_text$rowSet[[X]][sapply(json_text$rowSet[[X]], is.null)] <- NA
    unlist(json_text$rowSet[[X]]) }
  )))
  colnames(df) <- json_text$headers
  df
})
gameDetailFrame <- do.call(rbind.data.frame, gameDetail)

save(gameDetailFrame,file = 'gameDetailFrame.Rda')