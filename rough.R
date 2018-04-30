# Steam scraper
library(httr)
library(rvest)
library(curl)
library(tseries)
library(jsonlite)
library(tidyverse)
game_list <- "http://api.steampowered.com/ISteamApps/GetAppList/v0002/"%>% curl() %>% readLines() %>% fromJSON(flatten = TRUE) %>% as.data.frame()

get_items_and_prices <- function(app.name, num_items = 100){
  if(app.name %in% game_list$applist.apps.name){
    appid = game_list$applist.apps.appid[which(game_list$applist.apps.name == app.name)]
  }
  else{
    stop("Your game isn't in Steam's Library. Check your spelling or try another game")
  }
  
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

get_game_info <- function(app.name){
  if(app.name %in% game_list$applist.apps.name){
    appid = game_list$applist.apps.appid[which(game_list$applist.apps.name == app.name)]
  }
  else{
    stop("Your game isn't in Steam's Library. Check your spelling or try another game")
  }
  url = paste("https://store.steampowered.com/api/appdetails?appids=", appid, sep = "")
  out = url %>% curl() %>% readLines() %>% fromJSON(flatten = TRUE) %>% as_data_frame() 
  names(out) <- "V1"
  out$V1$data$publisher
}

get_financial_info <- function(symbol, days_to_plot){
  start_date = Sys.Date() - strtoi(days_to_plot)
  out = get.hist.quote(symbol, start = start_date, end = Sys.Date(), retclass = "ts")
  out = as.data.frame(out) %>% mutate(Date = seq(1, dim(out)[1]))
  out
}
