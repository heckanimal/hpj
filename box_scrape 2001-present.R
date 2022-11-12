#clear workspace
rm(list = ls())

#packages
library(tidyverse)
library(rvest)

#set working directory
setwd('C:/Users/james/OneDrive/Documents/HPJ')

#read schedule file
schedule <- read_delim('Data/schedule_results.txt',delim = '|')
#schedule <- schedule %>% filter(season <= 2001)

#start timer
t0 <- Sys.time()
#initiate box scrape loop
for(g in 1:nrow(schedule)){
#for(g in 1:10){
  #create game url text
  game_page_url <- paste0('https://www.basketball-reference.com',schedule$box_link[g])
  
  #retrieve game page html info (build in condition for 10 attempts if web page cannot be resolved)
  game_page <- NA
  attempt <- 1
  while(is.na(game_page) & attempt <= 10){
    try(
      game_page <- read_html(game_page_url)
    )
    if(is.na(game_page)){
      print(paste0('attempt ',attempt,' failed: ',game_page_url))
      attempt <- attempt + 1
      Sys.sleep(1)
      }
  }
  
  
  #determine how many distinct quarter/half/OT selections the page has
  p <- length(html_elements(game_page,'.sr_preset'))
  
  #pull away box score stats (omitting head rows)
  html_table(game_page)[[1]][-c(1,7),] -> v_box_stats
  
  #retrieve player id from html attributes
  v_box_name_node <- html_elements(game_page,paste0('#box-',schedule$away_abv[g],'-game-basic .left:nth-child(1)'))
  v_player_ids <- html_attr(v_box_name_node,'data-append-csv')
  
  #combine visitor box score stats with other fields
  temp_v_box <- data.frame(v_player_ids,schedule$away[g],schedule$away_abv[g],schedule$season[g],schedule$date[g],schedule$time[g],'away',v_box_stats,schedule$game_id[g])
  
  #visitor column names
  colnames(temp_v_box) <- c('player_id','team','team_abv','season','date','time','site','name','mp','fg','fga','fgp','f3','f3a','f3p','ft','fta','ftp','orb','drb','trb','ast','stl','blk','tov','pf','pts','pm','game_id')
  
  
  
  #pull home box score stats (ommitting head rows)
  html_table(game_page)[[p+2]][-c(1,7),] -> h_box_stats
  
  #retrieve player id from html attributes
  h_box_name_node <- html_elements(game_page,paste0('#box-',schedule$home_abv[g],'-game-basic .left:nth-child(1)'))
  h_player_ids <- html_attr(h_box_name_node,'data-append-csv')
  
  #combine home box score stats with other fields
  temp_h_box <- data.frame(h_player_ids,schedule$home[g],schedule$home_abv[g],schedule$season[g],schedule$date[g],schedule$time[g],'home',h_box_stats,schedule$game_id[g])
  
  #visitor column names
  colnames(temp_h_box) <- c('player_id','team','team_abv','season','date','time','site','name','mp','fg','fga','fgp','f3','f3a','f3p','ft','fta','ftp','orb','drb','trb','ast','stl','blk','tov','pf','pts','pm','game_id')
 
  if(g == 1){
    box_df <- rbind(temp_v_box,temp_h_box)
  } else {
    box_df <- rbind(box_df,temp_v_box,temp_h_box)
  }
  print(paste0(schedule$game_id[g],' ',g,'/',nrow(schedule),'; ',Sys.time() - t0))
  #Sys.sleep(1)
} 


#clean box score data
box_df %>%
  #add player status field to store "did not play", "did not dress", etc.
  mutate(player_status = ifelse(nchar(pf) > 2,pf,NA)) %>%
  #remove "did not play", "did not dress", etc. from stat fields and replace w/ 0
  mutate_at(c('mp','fg','fga','fgp','f3','f3a','f3p','ft','fta','ftp',
              'orb','drb','trb','ast','stl','blk','tov','pf','pts','pm'),
            ~ifelse(is.na(player_status),.,'0')) %>%
  #if fg, ft, or 3 pt attempts are 0, then the % should be NA
  mutate(fgp = ifelse(fga == '0',NA,fgp),
         f3p = ifelse(f3a == '0',NA,f3p),
         ftp = ifelse(fta == '0',NA,ftp)) %>%
  #remove "+" from plus minus, and if plus minus is blank then replace w/ NA
  mutate(pm = gsub('\\+','',pm),
         pm = ifelse(pm == '',NA,pm)) %>%
  #for team observations, make player_id the team abv
  mutate(player_id = ifelse(is.na(player_id),team_abv,player_id)) %>%
  #add observation id
  mutate(box_key = paste0(player_id,'_',game_id)) -> box_df

write_delim(box_df,'box_set.txt',delim = '|')
  

