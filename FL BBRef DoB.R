# Load the rvest library
library(plyr)
library(tidyverse)
library(janitor)
library(stringr)
library(baseballr)
library(Lahman)
library(rvest)
library(openxlsx)
library(rvest)
library(lubridate)
library(svMisc)
rost <- readxl::read_xlsx("C:/Users/t.medina/OneDrive/frontierleague bbrefid.xlsx")

# Specify the URL of the webpage you want to scrape
url <- paste("https://www.baseball-reference.com/register/player.fcgi?id=",id)

# Read the HTML content of the webpage
webpage <- read_html(url)

# Use CSS selectors to select the elements you want to scrape
# Replace "your_css_selector" with the actual CSS selector of the element you want to scrape
data <- webpage %>%
  html_nodes("#necro-birth") %>%
  html_text()

# Print the scraped data
mdy(gsub("\\s+", " ", trimws(data)))

dob_results <- as.Date(character(0))

# Loop through each row in 'rost'
for (i in 1:nrow(rost)) {
  # Construct the URL for the current 'id'
  url <- paste0("https://www.baseball-reference.com/register/player.fcgi?id=", rost$bbref_id[i])
  player_name <- rost$NAME[i]
  # Read the HTML content of the webpage
  webpage <- read_html(url)
  
  # Use CSS selectors to select the elements you want to scrape
  data <- webpage %>%
    html_nodes("#necro-birth") %>%
    html_text()
  
 # print(data)
  
  if(length(data) > 0){
  
    # Clean and format the date
  cleaned_date <- as.Date(mdy(gsub("\\s+", " ", trimws(data))))
  } else(cleaned_date <- as.Date(NA))
  
  # Append the result to the vector
  dob_results <- c(dob_results, cleaned_date)
  
  print(paste("Scrape dob for", player_name, i, "of", nrow(rost),"estimated", (712-i)/12, "mins remaining."))
  
  Sys.sleep(5)
}
dob_results
# Update the 'dob' column in 'rost'
rost$dob <- dob_results


write.csv(rost, "C:/Users/t.medina/OneDrive/frontierleague bbrefid_with dob.csv", row.names = F)
