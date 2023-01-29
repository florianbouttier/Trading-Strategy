rm(list = ls())
library(crypto2)
library(quantmod)
library(tidyverse)
library(lubridate)
library(data.table)
library(readxl)
library(purr)
setwd("C:/Users/flbouttier/OneDrive - eaholding.onmicrosoft.com/Bureau/PERSO/Trading bot")
crypto_list <- function (only_active = TRUE, add_untracked = FALSE) 
{
  active_url <- paste0("https://web-api.coinmarketcap.com/v1/cryptocurrency/map")
  active_coins <- jsonlite::fromJSON(active_url)
  coins <- active_coins$data %>% tibble::as_tibble() %>% dplyr::mutate(dplyr::across(10:11, 
                                                                                     as.Date))
  if (!only_active) {
    inactive_url <- paste0("https://web-api.coinmarketcap.com/v1/cryptocurrency/map?listing_status=inactive")
    inactive_coins <- jsonlite::fromJSON(inactive_url)
    coins <- dplyr::bind_rows(coins, inactive_coins$data %>% 
                                tibble::as_tibble() %>% dplyr::mutate(dplyr::across(8:9, 
                                                                                    as.Date))) %>% dplyr::arrange(id)
  }
  if (add_untracked) {
    untracked_url <- paste0("https://web-api.coinmarketcap.com/v1/cryptocurrency/map?listing_status=untracked")
    untracked_coins <- jsonlite::fromJSON(untracked_url)
    coins <- dplyr::bind_rows(coins, untracked_coins$data %>% 
                                tibble::as_tibble() %>% dplyr::mutate(dplyr::across(8:9, 
                                                                                    as.Date), is_active = -1)) %>% dplyr::arrange(id)
  }
  return(coins %>% dplyr::select(id:last_historical_data) %>% 
           dplyr::distinct() %>% dplyr::arrange(id))
}

#--------------------------------------Data-------------------------------------------------------------------------------------
NB_Best = 20

Historical_Snapshot = fread("./Data/Historical_Shapshot.csv")
Historical_Snapshot = Historical_Snapshot %>% 
  filter(Rank <= NB_Best)

stable_coin = c("USDT","USDC","BUSD","WBTC")

DL_Data <- function(symbols =  c("BTC","ETH"),start_date){
  coinlist = crypto_list(only_active = TRUE) %>% 
    filter(symbol %in% symbols)
  
  DataUSDT = crypto2::crypto_history(coin_list = coinlist,
                                     convert = "USD",
                                     start_date = start_date) 
  DataBTC = crypto2::crypto_history(coin_list = coinlist %>% filter(symbol!= "BTC"),
                                    start_date = start_date,
                                    convert = "BTC") 
  DataETH = crypto2::crypto_history(coin_list = coinlist %>% 
                                      filter(!(symbol %in% c("BTC","ETH"))),
                                    start_date = start_date,
                                    convert = "ETH") 
  
  Data = DataUSDT %>% 
    rbind(DataBTC) %>% 
    rbind(DataETH) %>% 
    dplyr::select(time_close,symbol,ref_cur,open,close,high,low,volume,market_cap) %>% 
    mutate(time_close = as.Date(time_close)) 
  
  Data = Data %>% 
    filter(! is.na(close))
  return(Data)
}


Data = fread("./Data/Data_30HistoricalSnapshot.csv") %>% 
  group_by(time_close,ref_cur,symbol) %>% 
  filter(market_cap == max(market_cap)) %>% 
  ungroup() 

Symbol_New_Data = Historical_Snapshot %>% filter(Annee == max(Annee)) %>% pull(Symbole)
Symbol_New_Data = Symbol_New_Data[!(Symbol_New_Data %in% stable_coin)]
New_Data = DL_Data(symbols = Symbol_New_Data,
                   start_date = max(Data$time_close))

New_Data = New_Data %>% 
  group_by(time_close,ref_cur,symbol) %>% 
  filter(market_cap == max(market_cap)) %>% 
  slice(1) %>% 
  ungroup()



All_Data = Data %>% 
  dplyr::bind_rows(New_Data) %>% 
  distinct()


fwrite(All_Data,"./Data_30HistoricalSnapshot.csv")
# "C:/Users/flori/OneDrive/Bureau/Finance/Bot trading/New Trading bot/Data/Data_30HistoricalSnapshot.csv"