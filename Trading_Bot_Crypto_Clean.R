rm(list = ls())
library(crypto2)
library(quantmod)
library(tidyverse)
library(TTR)
library(lubridate)
library(data.table)
library(readxl)
#----------------------------------------------Data-------------------------------------------------------------------------------------
Mapping_Buy_Alt1 <- read_excel("./Mapping_Achat.xlsx",sheet = "MappingvsRefCur1")
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
Data = DL_Data(symbols = c("BTC","ETH","BNB"),start_date = "2010-01-01")
#Computing the daily return
Data = Data %>% 
  group_by(symbol,ref_cur) %>% 
  arrange(time_close) %>% 
  mutate(DR = close/lag(close)) %>% 
  drop_na(DR) %>% ungroup()
#----------------------------------------------Functions---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
func_training_1d <- function(Data_Base = Data,fee,List_N,List_MA){
  Learning <- function(N,func_MovingAverage){
    df = Data_Base %>% mutate(DR = DR-1) %>% 
      group_by(symbol,ref_cur) %>%
      arrange(time_close) %>%
      mutate(EMA_Curve = func_MovingAverage(close,n = N))  %>%
      drop_na(EMA_Curve) %>%
      mutate(Buying_Decision = as.numeric(EMA_Curve < close),
             Selling_Signal = -as.numeric(low < lag(EMA_Curve))) %>%
      drop_na(Selling_Signal) %>%
      mutate(DR_Base = 1+lag(Buying_Decision)*DR,
             DR_Fees_Base = 1-fee*abs(Buying_Decision - lag(Buying_Decision)),
             DR_Base = DR_Base*DR_Fees_Base) %>%
      drop_na(DR_Base) %>%
      mutate(DR_Wallet = if_else(Selling_Signal == -1,lag(EMA_Curve)/lag(close),DR_Base),
             DR_Wallet = if_else(lag(Buying_Decision) == 0,1,DR_Wallet),
             Buying_Movement = ((lag(Buying_Decision) == 0) & (Buying_Decision == 1))|((Buying_Decision == 1) & (Selling_Signal == -1)),
             Buying_Movement = as.numeric(Buying_Movement),
             Sell_Movement = (Selling_Signal == -1) & (lag(Buying_Decision) == 1),
             Sell_Movement = as.numeric(Sell_Movement),
             DR_Fees = (1-fee*(Buying_Movement+Sell_Movement)),
             DR_Wallet = DR_Fees*DR_Wallet) %>%
      drop_na(DR_Wallet) %>%
      mutate(N_EMA = N,
             MovingAverage = if_else(isTRUE(all.equal(func_MovingAverage,EMA)),"EMA","SMA"),
             DR = 1+DR)%>%
      ungroup() %>%
      dplyr::select(time_close,symbol,ref_cur,
                    DR,DR_Base,DR_Wallet,EMA_Curve,
                    Buying_Decision,Selling_Signal,
                    N_EMA,MovingAverage) 
    print(paste("N =",N))
    return(df)
  }
  
  df_final = map_df(List_N,function(x){return(map_df(List_MA,Learning,N = x))})
  return(df_final)
  
}
func_trading_alt <- function(Test,Mapping,ref = "BTC",fee){
  Test_Symbol_USD = Test %>%
    filter(ref_cur == "USD",symbol != ref) %>%
    ungroup() %>% 
    dplyr::select(time_close,symbol,Buying_Decision,DR_Wallet) %>%
    rename("Buy_Symbol_VS_USD" = Buying_Decision,
           "DR_Symbol_VS_USD" = DR_Wallet)
  
  Test_Refcur_USD = Test %>%
    filter(ref_cur == "USD",symbol == ref) %>%
    ungroup() %>% 
    dplyr::select(time_close,Buying_Decision,DR_Wallet) %>%
    rename("Buy_Refcur_VS_USD" = Buying_Decision,
           "DR_Refcur_VS_USD" = DR_Wallet)
  
  Test_Symbol_Refcur = Test %>%
    filter(ref_cur == ref,symbol != ref) %>%
    ungroup() %>% 
    dplyr::select(time_close,symbol,Buying_Decision) %>%
    rename("Buy_Symbol_VS_Refcur" = Buying_Decision)
  
  Table_Buy  = Mapping %>% mutate(Combinaison_Buy = paste(`Symbol vs Ref cur`,
                                                              `Symbol vs USD`,
                                                              `Ref cur vs USD`,
                                                              sep = "-"))
  
  Test_ = Test_Symbol_USD %>% 
    left_join(Test_Refcur_USD,
              by = "time_close") %>% 
    left_join(Test_Symbol_Refcur,
              by = c("time_close", "symbol")) %>% 
    mutate(Combinaison_Buy = paste(Buy_Symbol_VS_Refcur,
                                     Buy_Symbol_VS_USD,
                                     Buy_Refcur_VS_USD,
                                     sep = "-")) %>% 
    left_join(Table_Buy,by = "Combinaison_Buy") %>%
    group_by(symbol) %>% 
    arrange(time_close) %>% 
    mutate(DR_Wallet_Final = if_else(lag(Position) == "Ref cur",
                                     DR_Refcur_VS_USD,
                                     if_else(lag(Position) == "Symbol",
                                             DR_Symbol_VS_USD,
                                             1))) %>% 
    mutate(DR_New_Fees = 1-fee*as.numeric( ((Position == "Symbol") & (lag(Position) == "Ref cur")) |  ((Position == "Ref cur") & (lag(Position) == "Symbol")))) %>% 
    mutate(DR_Wallet_Final = DR_Wallet_Final*DR_New_Fees) %>% 
    dplyr::select(time_close,symbol,Position,DR_Symbol_VS_USD,DR_Refcur_VS_USD,DR_Wallet_Final) %>% 
    left_join(Test %>%
                filter(ref_cur == "USD",symbol != ref) %>% 
                mutate("DR_Symbol_Raw" = DR) %>% 
                dplyr::select(time_close,symbol,DR_Symbol_Raw),
              by = c("time_close","symbol")) %>% 
    left_join(Test %>%
                filter(ref_cur == "USD",symbol == ref) %>% 
                mutate("DR_RefCur_Raw" = DR) %>% 
                dplyr::select(time_close,DR_RefCur_Raw),
              by = "time_close") %>% 
    ungroup()
  return(Test_)
}
Score <- function(x,alpha_positive,alpha_negative){
  x0 = x-1
  s = as.numeric(x0> 0)/(1+exp(-alpha_negative*x0)) + as.numeric(x0<= 0)/(1+exp(-alpha_positive*x0))
  #s = as.numeric(x0> 0)*log(abs(1+alpha_positive*x0)) + as.numeric(x0<= 0)*(-exp(-alpha_negative*x0)+1) 
  return(s)
}
Decreasing_Sum <- function(liste,alpha){
  listing = liste * exp(-alpha*(0:(length(liste)-1)))
  return(sum(listing))
}
func_all_curve_2d <- function(DF_Train = Full_Train,symb,ref,seq_MA,Mapping,fee,Summarise){
  #First Part. Learning ETHBTC
  Data_Base_Filter_ETH_BTC = DF_Train %>% 
    filter(symbol %in% symb) %>% 
    filter(ref_cur %in% c("USD",ref)) %>% 
    dplyr::bind_rows(DF_Train %>% 
                       filter(symbol == ref) %>% 
                       filter(ref_cur %in% c("USD",ref)))
  
  func_retour_trading_alt_ETHVSBTC <- function(N_symbol_USD,N_symbol_BTC,N_ref_USD){
    Data_EMASMA = Data_Base_Filter_ETH_BTC %>% 
      filter((symbol %in% symb & ref_cur == "USD" & N_EMA == N_symbol_USD) | 
               (symbol %in% ref & ref_cur == "USD" & N_EMA == N_ref_USD) | 
               (symbol %in% symb & ref_cur == ref & N_EMA == N_symbol_BTC)) 
    
    Retour = Data_EMASMA %>% 
      filter(MovingAverage == "EMA") %>% 
      func_trading_alt(Mapping = Mapping,fee = fee,ref = ref) %>%
      mutate(N_ETH_USD = N_symbol_USD,
             N_ETH_BTC = N_symbol_BTC,
             N_BTC_USD  = N_ref_USD,
             MovingAverage = "EMA") %>% 
      dplyr::bind_rows(Data_EMASMA %>% 
                         filter(MovingAverage == "SMA") %>% 
                         func_trading_alt(Mapping = Mapping,fee = fee,ref = ref)%>%
                         mutate(N_ETH_USD  = N_symbol_USD,
                                N_ETH_BTC  = N_symbol_BTC,
                                N_BTC_USD =  N_ref_USD,
                                MovingAverage = "SMA") ) %>% 
      drop_na(DR_Wallet_Final)
    print(paste(N_symbol_USD,N_symbol_BTC,N_ref_USD,sep = "-"))
    return(Retour)
  } 
  Full_Results_ETHBTC = map_df(seq_MA,function(x){return(map_df(seq_MA,function(y){return(map_df(seq_MA,func_retour_trading_alt_ETHVSBTC,N_symbol_USD = x,N_symbol_BTC = y))}))}) %>% 
    mutate(temporality = lubridate::floor_date(time_close,temp))
  if(Summarise){
    Full_Results_ETHBTC = Full_Results_ETHBTC %>% 
      group_by(temporality,symbol,N_ETH_USD,N_ETH_BTC,N_BTC_USD,MovingAverage) %>% 
      summarise(across(starts_with("DR"),prod)) %>% ungroup()
  }
  return(Full_Results_ETHBTC)
}
#Ploting the return of the trading method in 1 dimensions
ploting_training_1d <- function(Data,symb,ref,liste_idi,date_debut,date_fin){
  DF = Data %>%
    filter(symbol == symb,
           ref_cur == ref) %>% 
    mutate(idi = paste(N_EMA,MovingAverage,sep = "-")) %>%
    filter(idi %in% liste_idi,
           time_close >= as.Date(date_debut),
           time_close <= as.Date(date_fin)) %>%
    group_by(idi) %>%
    arrange(time_close) %>%
    mutate(Rendement_SJ = cumprod(DR),
           Rendement = cumprod(DR_Wallet)) %>% 
    dplyr::select(time_close,Rendement,Rendement_SJ,idi) %>% 
    pivot_wider(names_from = idi,values_from = Rendement) %>% 
    pivot_longer(-c(time_close),names_to = "Curve",values_to = "Price") 
  DF %>% 
    ggplot(aes(x = time_close,y = Price,color = Curve))+
    geom_line()+
    scale_color_brewer(palette = "Set1")
}
#Ploting the return of the trading method in 2 dimensions
ploting_training_2d <- function(Data,symb,ref,N_ref_USD,N_symbol_USD,N_symbol_BTC,MA,date_debut,date_fin,Mapping){
  
  Data_EMASMA = Data %>% filter(MovingAverage == MA) %>%  
    filter((symbol == symb & ref_cur == "USD" & N_EMA == N_symbol_USD) | 
             (symbol == ref & ref_cur == "USD" & N_EMA == N_ref_USD) | 
             (symbol == symb & ref_cur == ref & N_EMA == N_symbol_BTC)) 
  
  Data_EMASMA  %>% 
    func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = ref)  %>% 
    filter(as.Date(time_close) >= as.Date(date_debut),
           as.Date(time_close) <= as.Date(date_fin)) %>%
    arrange(time_close) %>% 
    pivot_longer(starts_with("DR"),names_to = "curve",values_to = "DR") %>%
    group_by(curve) %>% arrange(time_close) %>% 
    mutate(price = cumprod(DR)) %>% 
    ggplot(aes(x = time_close,y = price,color = curve))+
    geom_line()+scale_color_brewer(palette = "Set1")
}
#------------------------------------------------Application of one dimension training----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Full_Train <- func_training_1d(Data_Base = Data %>% 
                                 filter(time_close >= as.Date("2013-01-01")),
                               fee = 0.075/100,
                               List_N = 10:90,
                               List_MA = c(EMA,SMA)) 
temp = "6 months"
Full_Train <- Full_Train %>% 
  mutate(temporality = as.numeric(as.Date(lubridate::floor_date(time_close,unit = temp))))
#----------------------------------------------Determination all curves in 2 dimensions------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
list_coins = c("BNB","ETH")
All_Return_Against_BTC = func_all_curve_2d(DF_Train = Full_Train,
                                           symb = list_coins,
                                           ref = "BTC",
                                           seq_MA = seq(5,60,by = 5),
                                           Mapping = Mapping_Buy_Alt1,
                                           fee = 0.075/100,
                                           Summarise = TRUE)
All_Return_Against_ETH = func_all_curve_2d(DF_Train = Full_Train,
                                           symb = list_coins[list_coins != "ETH"],
                                           ref = "ETH",
                                           seq_MA = seq(5,60,by = 5),
                                           Mapping = Mapping_Buy_Alt1,
                                           fee = 0.075/100,
                                           Summarise = TRUE)

All_Return_By_Temporality = All_Return_Against_BTC %>% 
  mutate(ref_cur = "BTC") %>% 
  dplyr::bind_rows(All_Return_Against_ETH %>% mutate(ref_cur = "ETH")) %>% 
  mutate(Surperf =(DR_Wallet_Final/DR_Symbol_Raw+DR_Wallet_Final/DR_RefCur_Raw)/2) 

Determination_Futur_Parameters <- function(date_end_training,alpha_positive,alpha_negative,alpha){
  p = All_Return_By_Temporality %>% 
    filter(temporality < date_end_training) %>% 
    mutate(Surperf = Score(Surperf,
                           alpha_positive = alpha_positive,
                           alpha_negative = alpha_negative)) %>% 
    group_by(symbol,ref_cur,N_BTC_USD,N_ETH_USD,N_ETH_BTC,MovingAverage) %>%
    arrange(desc(temporality)) %>% 
    summarise(Surperf = Decreasing_Sum(Surperf,alpha),
              N = n(),
              .groups = "keep") %>% 
    ungroup() %>% 
    group_by(symbol,ref_cur) %>% 
    filter(Surperf == max(Surperf))  %>% 
    filter(N_BTC_USD == max(N_BTC_USD)) %>% 
    filter(N_ETH_USD == max(N_ETH_USD)) %>% 
    filter(N_ETH_BTC == max(N_ETH_BTC)) %>% 
    ungroup() %>% 
    mutate(temporality = as.character(date_end_training),
           alpha_positive = alpha_positive,
           alpha_negative = alpha_negative,
           alpha = alpha)
  print(paste(alpha_positive,alpha_negative,alpha),sep = "-")
  return(p)
}

Final_Parameters = map_df(All_Return_By_Temporality %>% 
                            filter(temporality != min(temporality)) %>% 
                            pull(temporality) %>% 
                            unique(),
                          Determination_Futur_Parameters,
                          alpha_positive = 5,
                          alpha_negative = 7,
                          alpha = 0.05)

Final_Tested_Curve = All_Return_By_Temporality %>%
  mutate(temporality = as.character(temporality)) %>% 
  inner_join(Final_Parameters,
             by = c("symbol","ref_cur","N_BTC_USD","N_ETH_USD","N_ETH_BTC","MovingAverage","temporality"))

Final_Tested_Curve  %>% 
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(temporality)) %>% 
  group_by(Year,wallet,symbol,ref_cur) %>% 
  summarise(Rendement = prod(DR)) %>% 
  ungroup() %>% 
  filter(!(wallet %in% c("DR_Symbol_VS_USD","DR_Refcur_VS_USD"))) %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = Year,y = wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text(size = 2)+
  scale_x_continuous(breaks = 2016:2023)+
  labs(title = "Return of our trading strategy",
       subtitle = "On ETH and BTC.\n The return of ETH per year is DR_Symbol_Raw,\n the return of BTC is DR_RefCur_Raw",
       fill = "Return")

