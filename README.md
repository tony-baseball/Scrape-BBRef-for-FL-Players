# Scrape BBRef for FL Players
The point of this is to acquire career stats for Frontier League players. Specifically, I have my Frontier League Shiny app that scrapes FL stats from frontierleague.com, but it also has Yakkertech data for other metrics. 
I wanted to add a player page where it contains their career stats from BBRef, and pull in their date of birth if it exists on BBRef.

One thing to note:
BBRef does not allow more than 20 api calls per minute, so in the loop you need to introduce a sys.sleep(5). If you violate this rule your session will be in "jail" for an hour. 
Source here: https://www.sports-reference.com/bot-traffic.html

