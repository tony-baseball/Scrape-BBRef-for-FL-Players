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

# Scrape BBref Pro Baseball Register ----

# This url is the base of each url, but it is also the webpage where it contains the first 2 letters of every player registered on baseball-reference. For example, it starts with every player whose lastname begins with "A'", then "Aa", then "Ab", etc., for every last name on the pro register.
url <- "https://www.baseball-reference.com/register/player.fcgi"

# Read the HTML content of the page
page <- read_html(url)

# Extract all the links on the page
links <- page %>%
  html_nodes("a") %>%
  html_attr("href") 
links
# here, I filter all the links that have "initial" in the link, and then also remove everything before the ? in the link
initial <- sub(".+\\?", "?", links[grepl('initial', links)])

# create an empty data frame
bbref_players <- data.frame()

{ # starttime variable is just the time i begin the scrape. I like to see how long all my loops/scrapes take so i also have an endtime at theend
  starttime <- now()
  
  for (i in initial) { # for each entry in initial, run the following loop
    
    # create the proper url
    initial_url <- paste0(url, initial[1]) 
    
    # read the initial url
    initial_page <- curl::curl(initial_url) %>% 
      read_html()

    # obtain the text of each hyperlink on the page
    link_texts <- initial_page %>%
      html_nodes("a") %>%
      html_text()
    
    # obtain the url of each hyperlink on the page
    links <- initial_page %>%
      html_nodes("a") %>%
      html_attr("href")
links    
    # Combine link text and links into a data frame
    initial_df <- data.frame(LinkText = link_texts, Link = links) %>%
      # filter for rows that have "register/player.fcgi", which indicates it is a player
      filter(grepl('register/player\\.fcgi\\?id', Link)) %>%
      mutate(LastName = word(LinkText, 2, -1),
             FirstName = word(LinkText, 1, 1),)
    
    # add the players from this webpage to the existing players from previous pages
    bbref_players <- rbind(bbref_players, initial_df)
    
    # Introduce a delay of at least 5 seconds. BBRef does not allow more than 20 api calls per minute. Source here: https://www.sports-reference.com/bot-traffic.html
    Sys.sleep(5)
  } 
# bbref_players wound up with 452131 players as of 12/4/2023. 
  
# end of the loop  
endtime <- now()

# time the loop took. If i remember correctly, this took about 65 minutes; 718 different pages to scrape / (60 seconds in a minute / 5 seconds per api call) = 59.83 minutes. 
print(endtime - starttime)
}

# write.csv(bbref_players, "C:/Users/tonybaseball/OneDrive/bbrefplayers.csv")

bbref_players <- read.csv("C:/Users/tdmed/OneDrive/bbrefplayers.csv") %>%
  mutate(LastName_strip = gsub("[- | ']", "", LastName),
         FirstName_strip =  gsub("[- | ']", "", FirstName),
         LastName_strip = str_replace_all(LastName_strip, c("é" = "e", "á" = "a","Á" = "A", "ó" = "o", "í" = "i", "ú" = "u", "ü" = "u")),
         FirstName_strip = str_replace_all(FirstName_strip, c("é" = "e", "á" = "a","Á" = "A", "ó" = "o", "í" = "i", "ú" = "u", "ü" = "u")),
         bbref_id = sub(".+\\=", "",  Link)
         ) %>%
  select(-1)

bbref_id <- bbref_players 

# ----

rosters2 <- rosters %>% select(1:10, 13:15) %>%
  filter(!grepl("breaker",NAME)) %>%
  left_join(bbref_id, by = c("LastName" = "LastName_strip", "FirstName" = "FirstName_strip"))

write.xlsx(rosters2, "C:/Users/t.medina/OneDrive/frontierleague bbrefid.xlsx")
