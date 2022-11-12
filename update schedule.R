#clear workspace
rm(list = ls())

#save console output to text file
sink(paste0('C:/Users/james/OneDrive/Documents/HPJ/Script Logs/schedule update log ',Sys.Date(),'.txt'))

#capture and print script initiation time
t0 <- Sys.time()
print(t0)

#packages
library(tidyverse)
library(rvest)

#set scrape period as 2001 - 2022
#NOTE: the schedule tables changed format in 2001, adding tip-off time.
#Separate script(s?)/function(s?) needed to pull prior to 2001.
sched_years <- c(2023)

#set working directory
setwd('C:/Users/james/OneDrive/Documents/HPJ/Data')

#read current schedule file
old_sched_df <- read_delim('schedule_results.txt',delim = '|')


#initiate year loop
for(y in 1:length(sched_years)){
  
  #dynamically generate schedule URL
  sched_url <- paste0('https://www.basketball-reference.com/leagues/NBA_',sched_years[y],'_games.html')
  
  #retrieve game page html info (build in condition for 10 attempts if web page cannot be resolved)
  sched_main_page <- NA
  attempt <- 1
  while(is.na(sched_main_page) & attempt <= 10){
    try(
      sched_main_page <- read_html(sched_url)
    )
    if(is.na(sched_main_page)){
      print(paste0('attempt ',attempt,' failed: ',sched_url))
      attempt <- attempt + 1
      Sys.sleep(1)
    }
  }
  
  
  #get the months when games were played from the month selector links on the page
  sched_months_node <- html_elements(sched_main_page,'.filter a')
  sched_months <- tolower(html_text(sched_months_node))
  #2020 season has two octobers. add dashes between october and year to match url
  sched_months <- ifelse(sched_months == 'october 2019','october-2019',sched_months)
  sched_months <- ifelse(sched_months == 'october 2020','october-2020',sched_months)
  
  #initiate month loop
  for(m in 1:length(sched_months)){
    
    #generate url text and read html data for each monthly schedule page within a season
    sched_page <- read_html(paste0('https://www.basketball-reference.com/leagues/NBA_',sched_years[y],'_games-',sched_months[m],'.html'))
    
    #initiate column scrape loop
    for(c in c(1,3,5)){
      
      #generate dynamic html node text for each column in schedule table
      #NOTE: the columns in the schedule table alternate node roots for some reason.
      #This means odd and even numbered columns must be pulled in separate steps.
      #Odd numbered columns are in the "left" node. Evens in the "right" node.
      sched_left_node_text <- paste0('.left:nth-child(',c,')')
      sched_right_node_text <- paste0('.right:nth-child(',c+1,')')
      
      #bring in data from column nodes using html data and node text
      #these steps bring in node data as lists
      sched_node_left <- html_elements(sched_page,sched_left_node_text)
      sched_node_right <- html_elements(sched_page,sched_right_node_text)
      #these steps turn the list data into text vectors
      sched_data_left <- html_text(sched_node_left)
      sched_data_right <- html_text(sched_node_right)
      
      #if these are the first two columns being brought in, combine columns to create
      #initial temp_sched_df. Otherwise add subsequent columns to existing temp_sched_df.
      if(c == 1){
        temp_sched_df <- data.frame(sched_years[y],sched_data_left,sched_data_right)
      } else {
        #this step pulls home/away team abbreviations out of the href html attributes
        team_abv <- sched_node_left %>% html_elements('a') %>% html_attr('href') %>% substr(8,10)
        sched_data_left <- sched_data_left[1:nrow(temp_sched_df)]
        sched_data_right <- sched_data_right[1:nrow(temp_sched_df)]
        temp_sched_df <- data.frame(temp_sched_df,sched_data_left,team_abv,sched_data_right)
      } 
    }
    
    #extract link to box score page for each game
    sched_node_center <- html_elements(sched_page,'.center:nth-child(7)') 
    sched_data_center <-  sched_node_center %>% html_elements('a') %>% html_attr('href')
    #fill in box score links for future games with blanks
    sched_data_center <- c(sched_data_center,rep('',nrow(temp_sched_df) - length(sched_data_center)))
    #add box score url path to temp df
    temp_sched_df <- data.frame(temp_sched_df,sched_data_center)
    
    #if this is the first month of data, save temp_sched_df as the permanent sched_df.
    #Otherwise bind new temp_sched_dfs onto existing sched_df.
    if(y == 1 & m == 1){
      new_sched_df <- temp_sched_df
    } else {
      new_sched_df <- bind_rows(new_sched_df,temp_sched_df)
    }
    
    #print progress
    print(paste0(sched_years[y],'_',sched_months[m],' ',Sys.time() - t0))
    #pause loop before next iteration
    #Sys.sleep(rnorm(1,1.5,.3))
  }
}

#set column headers
colnames(new_sched_df) <- c('season','date','time','away','away_abv','away_pts','home','home_abv','home_pts','box_link')

#parse date and time
new_sched_df %>%
  #all times have postscript "p" for "pm" or "a" for "am"
  mutate(time2 = gsub('p','pm',time),
         time2 = gsub('a','am',time2),
         #concatenate date and time
         date2 = paste0(date,' ',time2),
         #parse as POSIXct
         time = as.POSIXct(date2,format = '%a, %b %d, %Y %I:%M%p'),
         #parse date
         date = as.Date(date,format = '%a, %b %d, %Y')) %>%
  select(-c(time2,date2)) -> new_sched_df

#create game id
new_sched_df %>%
  mutate(game_id = paste(away_abv,home_abv,substr(date,1,10),sep = '_')) -> new_sched_df

#replace records from old schedule file with records from new schedule
old_sched_df %>%
  #filter out records with game ids that match new records
  filter(!game_id %in% new_sched_df$game_id) -> keep_sched_df
#test that records being kept do not match any new records
table(new_sched_df$game_id %in% keep_sched_df$game_id)
#convert all fields to character before binding data sets
keep_sched_df <- keep_sched_df %>% mutate_all(as.character)
new_sched_df <- new_sched_df %>% mutate_all(as.character)
#add new records
sched_df <- bind_rows(keep_sched_df,new_sched_df)

#write schedule table
write_delim(sched_df,'C:/Users/james/OneDrive/Documents/HPJ/Data/schedule_results.txt',delim = '|')
#write copy of old schedule data frame
write_delim(old_sched_df,paste0('C:/Users/james/OneDrive/Documents/HPJ/Data/Archive/schedule_results_',Sys.Date(),'.txt'),delim = '|')

#capture and print script completion time
t1 <- Sys.time()
print(t1-t0)
