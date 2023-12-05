{library(RSQLite)
library(plyr)
library(tidyverse)
library(janitor)
library(stringr)
library(baseballr)
library(Lahman)
library(rvest)
library(openxlsx)
library(curl)
}

# connect to database -----
db <- dbConnect(RSQLite::SQLite(), "C:/Users/tdmed/OneDrive/_Shiny/Coop2/coop2db.sqlite")
#db <- dbConnect(RSQLite::SQLite(), "C:/Users/tdmed/OneDrive/_Shiny/Coop2/coop2db.sqlite")

# team information
teams <- dbGetQuery(conn = db,"SELECT * FROM teams")
# league rosters
rosters <- dbGetQuery(conn = db,"SELECT * FROM rosters") %>%
  rename(`NO.` = 2,
         PLAYER_STATUS = 'STATUS') %>% # removed 2022
  
  # rosters <- rbind(rosters,rosters23) %>% 
  mutate(POSITION = toupper(POSITION)) %>%
  mutate(NAME = gsub('Tanner Jesson0Dalton', 'Tanner Jesson-Dalton', NAME),
         NAME = gsub('Caden OBrien', 'Caden OBrien', NAME),
         NAME = gsub('Caden Obrien', 'Caden OBrien', NAME)) %>%
  mutate(NAME = gsub('Robert Klinchock', 'Rob Klinchock', NAME) ) 
# %>%
#   mutate(LastName = gsub("[- | ']", "", word(NAME, 2, 2)),
#          FirstName = word(NAME, 1, 1),
#          bbref = tolower(paste0(substr(LastName, 1, 6),'000',substr(FirstName, 1, 3) ))
#          )


# Scrape BBref Pro Baseball Register ----
# Specify the URL of the page you want to scrape
url <- "https://www.baseball-reference.com/register/player.fcgi"

# Read the HTML content of the page
page <- read_html(url)

# Extract all the links on the page
links <- page %>%
  html_nodes("a") %>%
  html_attr("href") 

initial <- sub(".+\\?", "?", links[grepl('initial', links)])

#

bbref_players <- data.frame()

{starttime <- now()
  initial
  for (i in initial) {
    initial_url <- paste0(url, 
                          # initial[15]
                          i
                          )
    
   # initial_page <- read_html(initial_url)
   initial_page <- curl::curl(initial_url) %>% 
   read_html()

    link_texts <- initial_page %>%
      html_nodes("a") %>%
      html_text()
    
    links <- initial_page %>%
      html_nodes("a") %>%
      html_attr("href")
    
    # Combine link text and links into a data frame
    initial_df <- data.frame(LinkText = link_texts, Link = links) %>%
      filter(grepl('register/player\\.fcgi\\?id', Link)) %>%
      mutate(LastName = word(LinkText, 2, -1),
             FirstName = word(LinkText, 1, 1),)
    
    bbref_players <- rbind(bbref_players, initial_df)
    
    # Introduce a delay of, for example, 1 second between requests
    Sys.sleep(5)
  } 
  write.csv(bbref_players, "C:/Users/tonybaseball/OneDrive/bbrefplayers.csv")
endtime <- now()

endtime - starttime}


bbref_players <- read.csv("C:/Users/t.medina/OneDrive/bbrefplayers.csv") %>%
  mutate(LastName_strip = gsub("[- | ']", "", LastName),
         FirstName_strip =  gsub("[- | ']", "", FirstName),
         LastName_strip = str_replace_all(LastName_strip, c("é" = "e", "á" = "a","Á" = "A", "ó" = "o", "í" = "i", "ú" = "u", "ü" = "u")),
         FirstName_strip = str_replace_all(FirstName_strip, c("é" = "e", "á" = "a","Á" = "A", "ó" = "o", "í" = "i", "ú" = "u", "ü" = "u")),
         bbref_id = sub(".+\\=", "",  Link)
         ) %>%
  select(-1)

bbref_id <- bbref_players

# ----
#daws <- baseballr::playerid_lookup(last_name = "Craig", "Alec")
rosters2 <- rosters %>% select(1:10, 13:15) %>%
  filter(!grepl("breaker",NAME)) %>%
  left_join(bbref_id, by = c("LastName" = "LastName_strip", "FirstName" = "FirstName_strip"))

write.xlsx(rosters2, "C:/Users/t.medina/OneDrive/frontierleague bbrefid.xlsx")
# SCRAPE TABLES ! ! ! !!!!!!!!----

db <- dbConnect(RSQLite::SQLite(), "C:/Users/t.medina/OneDrive/_Shiny/Coop2/coop2db.sqlite")
#db <- dbConnect(RSQLite::SQLite(), "C:/Users/tdmed/OneDrive/_Shiny/Coop2/coop2db.sqlite")

# team information
teams <- dbGetQuery(conn = db,"SELECT * FROM teams")
# league rosters
rosters <- dbGetQuery(conn = db,"SELECT * FROM rosters") %>%
  rename(`NO.` = 2,
         PLAYER_STATUS = 'STATUS') %>% # removed 2022
  
  # rosters <- rbind(rosters,rosters23) %>% 
  mutate(POSITION = toupper(POSITION)) %>%
  mutate(NAME = gsub('Tanner Jesson0Dalton', 'Tanner Jesson-Dalton', NAME),
         NAME = gsub('Caden OBrien', 'Caden OBrien', NAME),
         NAME = gsub('Caden Obrien', 'Caden OBrien', NAME)) %>%
  mutate(NAME = gsub('Robert Klinchock', 'Rob Klinchock', NAME) ) 


player <- rosters %>%
  filter(NAME == 'Alec Craig')

# Specify the URL of the webpage
url <- paste0("https://www.baseball-reference.com/register/player.fcgi?id=",  player$bbref_id) 

# Read the HTML content of the webpage
webpage <- read_html(url) %>% html_table(fill = TRUE)

# The table_data is a list, and you can access the desired table
player_summary_h <- webpage[[1]] %>%
  filter(Year != "") %>%
  select(-AgeDif)  %>% 
  filter(!grepl('A|A+|AA|AAA|Rk|Ind|Smr',Year))

player_front_h <- webpage[[1]] %>%
  filter(Lg == "FRON") %>%
  select(-AgeDif) %>%
  mutate_at(vars(7:29), ~replace_na(., 0))%>%
  adorn_totals() %>%
  mutate(BA = round(H/AB,3),
         OBP = round((H+BB+SH+SF+HBP+IBB)/PA ,3),
         SLG =  round( ((HR*4)+(`3B`*3)+(`2B`*2)+(H-HR-`3B`-`2B`))/AB    ,3),
         OPS = OBP + SLG
         )
  
player_info_h <- paste(player$POSITION, "|", "Bats/Throws:", player$BATS,"/", player$THROWS, "|", player$YR, "|", player$HEIGHT, player$WEIGHT)

rm(player_info_h)
# ---------------------------------------
player_roster <- rosters %>%
  filter(NAME %in% 'Bren Spillane')

url <- paste0("https://www.baseball-reference.com/register/player.fcgi?id=",  player_roster$bbref_id[1]) 

# Read the HTML content of the webpage
webpage_bbref <- read_html(url) %>% html_table(fill = TRUE)

table <- webpage_bbref[[1]] %>%
  filter(Year != "") %>%
  select(-AgeDif)

selector <- if (nchar(input$selectorInput) > 0) input$selectorInput else "table"

scraped_data <- webpage_bbref %>% html_nodes(selector)[[1]] %>% html_table()

# filter rosters for new savant style page
player_roster <- rosters %>%
  filter(NAME %in% input$HitterInput_szn_opp)

url_bbref <- paste0("https://www.baseball-reference.com/register/player.fcgi?id=",  player_roster$bbref_id[1]) 

# Read the HTML content of the webpage
webpage_bbref <- read_html(url_bbref) %>% html_table(fill = TRUE)

table <- webpage_bbref[[1]] %>%
  filter(Year != "") %>%
  select(-AgeDif)

tableFilter <- reactive({table})

datatable(tableFilter(), options = list(scrollX=TRUE,dom = 't', 
                                        autoWidth = TRUE,
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': '#f47b20', 'color': 'black'});",
                                          "}"),
                                        columnDefs = list(list(targets = 0, visible = FALSE),
                                                          list(className = 'dt-center', 
                                                               targets = 6:28),
                                                          list(className = 'dt-head-center', targets = 6:28 )  ))) %>%
  formatStyle(c(1,2,3,6,13), `border-left` = "solid 1px") %>% formatStyle(c(15), `border-right` = "solid 1px")



