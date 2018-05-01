# Steam scraper functions
library(httr)
library(rvest)
library(curl)
library(tseries)
library(jsonlite)
library(tidyverse)

# Gets list of games and their associated steamID's 
game_list <- "http://api.steampowered.com/ISteamApps/GetAppList/v0002/"%>% curl() %>% readLines() %>% fromJSON(flatten = TRUE) %>% as.data.frame()

# Funciton that gets list of items for sale, based on the Steam Community API
get_items_and_prices <- function(app.name, num_items = 100){
  # Check to see if game specified is in Steam's library. If not, return error
  if(app.name %in% game_list$applist.apps.name){
    appid = game_list$applist.apps.appid[which(game_list$applist.apps.name == app.name)]
  }
  else{
    stop("Your game isn't in Steam's Library. Check your spelling or try another game")
  }
  # Since Steam only allows 100 queries at a time, this for loop allows the user to get more than 100 items for a specific game, should they choose
  for(i in 0:(num_items/100 - 1)){
    url = paste("https://steamcommunity.com/market/search/render/?query=&start=", 100*i, "&count=100&norender=1&appid=", appid, sep = "")
    new_data = url %>% curl() %>% readLines() %>% fromJSON(flatten = TRUE) %>% as.data.frame() %>%  subset(select = c("results.name", "results.sell_listings","results.sell_price", "results.sell_price_text", "results.app_icon","results.app_name"))
    if(i == 0){
      out = new_data
    }
    else{
      out = rbind(out, new_data)
    }
  }
  out
}

# An unused, but helpful function that gets the name of the publisher based on the name of the game
get_game_info <- function(app.name){
  # Check to see if game specified is in Steam's library. If not, return error
  if(app.name %in% game_list$applist.apps.name){
    appid = game_list$applist.apps.appid[which(game_list$applist.apps.name == app.name)]
  }
  else{
    stop("Your game isn't in Steam's Library. Check your spelling or try another game")
  }
  # Uses Steam's store API to get the publisher name
  url = paste("https://store.steampowered.com/api/appdetails?appids=", appid, sep = "")
  out = url %>% curl() %>% readLines() %>% fromJSON(flatten = TRUE) %>% as_data_frame() 
  names(out) <- "V1"
  out$V1$data$publisher
}

# Using the get.hist.quote function from the tseries library, outputs a time series data set for the given stock symbol and period
get_financial_info <- function(symbol, days_to_plot){
  start_date = Sys.Date() - strtoi(days_to_plot)
  out = get.hist.quote(symbol, start = start_date, end = Sys.Date(), retclass = "ts")
  out = as.data.frame(out) %>% mutate(Date = seq(1, dim(out)[1]))
  out
}
#Saves the output of get_items_and_prices in a csv file for later use 
save_nice_item_data <- function(item_data){
  out = subset(item_data, select = -c(results.app_icon)) 
  colnames(out) <- c("Item name", "Number for sale", "Price of item (cents)", "Price of item (conventional format)", "Name of Game")
  write.csv(out, file = "Item_data_for_Game.csv")
}

#Saves the output of get_financial_info in a csv file for later use 
save_nice_stock_data <- function(stock_data, days = 365){
  days = seq(Sys.Date() - 364, Sys.Date(), by="days")
  mutate(stock_data, Date = days)
  out = na.omit(stock_data)
  out
}
