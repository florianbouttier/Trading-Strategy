rm(list = ls())
setwd("C:/Users/flbouttier/OneDrive - eaholding.onmicrosoft.com/Bureau/PERSO/Trading bot")
library(crypto2)
library(quantmod)
library(tidyverse)
library(TTR)
library(lubridate)
library(data.table)
library(readxl)
#----------------------------------------------Data-------------------------------------------------------------------------------------
Data = fread("./Data/Data_30HistoricalSnapshot.csv") 

Mapping_Achat_Alt1 <- read_excel("./Mapping_Achat.xlsx",
                                 sheet = "MappingvsRefCur1")
Mapping_Achat_Alt2 <- read_excel("./Mapping_Achat.xlsx",
                                 sheet = "MappingvsRefCur2")
Mapping_Achat  <- read_excel("./Mapping_Achat.xlsx",
                             sheet = "Mapping2")
Historical_Snapshot = fread("./Data/Historical_Shapshot.csv")

Data = Data %>% 
  group_by(symbol,ref_cur) %>% 
  arrange(time_close) %>% 
  mutate(DR = close/lag(close)) %>% 
  drop_na(DR) %>% ungroup() %>% 
  dplyr::filter(!symbol %in% c("DAI","UST","UNI","BUSD","USDC","WBTC","USDT"))

Historical_Snapshot = Historical_Snapshot %>% filter(Rank <= 20) %>% 
  dplyr::mutate(Year = Annee +1,symbol = Symbole) %>% 
  dplyr::select(Year,symbol)
#----------------------------------------------Fonctions---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
func_training_1d <- function(Data_Base = Data,fee,Liste_N,Liste_MA){
  
  Apprentissage <- function(N,func_MovingAverage){
    df = Data_Base %>% mutate(DR = DR-1) %>% 
      group_by(symbol,ref_cur) %>%
      arrange(time_close) %>%
      mutate(EMA_Curve = func_MovingAverage(close,n = N))  %>%
      drop_na(EMA_Curve) %>%
      mutate(Descision_Achat = as.numeric(EMA_Curve < close),
             Descision_Vente = -as.numeric(low < lag(EMA_Curve))) %>%
      drop_na(Descision_Vente) %>%
      mutate(DR_Base = 1+lag(Descision_Achat)*DR,
             DR_Fees_Base = 1-fee*abs(Descision_Achat - lag(Descision_Achat)),
             DR_Base = DR_Base*DR_Fees_Base) %>%
      drop_na(DR_Base) %>%
      mutate(DR_Wallet = if_else(Descision_Vente == -1,lag(EMA_Curve)/lag(close),DR_Base),
             DR_Wallet = if_else(lag(Descision_Achat) == 0,1,DR_Wallet),
             Mouvement_Achat = ((lag(Descision_Achat) == 0) & (Descision_Achat == 1))|((Descision_Achat == 1) & (Descision_Vente == -1)),
             Mouvement_Achat = as.numeric(Mouvement_Achat),
             Mouvement_Vente = (Descision_Vente == -1) & (lag(Descision_Achat) == 1),
             Mouvement_Vente = as.numeric(Mouvement_Vente),
             DR_Fees = (1-fee*(Mouvement_Achat+Mouvement_Vente)),
             DR_Wallet = DR_Fees*DR_Wallet) %>%
      drop_na(DR_Wallet) %>%
      mutate(N_EMA = N,
             MovingAverage = if_else(isTRUE(all.equal(func_MovingAverage,EMA)),"EMA","SMA"),
             DR = 1+DR)%>%
      ungroup() %>%
      # dplyr::select(time_close,symbol,ref_cur,close,
      #               DR,DR_Base,DR_Wallet,EMA_Curve,
      #               Descision_Achat,Descision_Vente,
      #               N_EMA,MovingAverage)  %>%
      dplyr::select(time_close,symbol,ref_cur,
                    DR,DR_Base,DR_Wallet,EMA_Curve,
                    Descision_Achat,Descision_Vente,
                    N_EMA,MovingAverage) 
    print(paste("N =",N))
    return(df)
  }
  
  df_final = map_df(Liste_N,function(x){return(map_df(Liste_MA,Apprentissage,N = x))})
  return(df_final)
  
}
func_trading_alt <- function(Test,Mapping,ref = "BTC",fee){
  Test_Symbol_USD = Test %>%
    filter(ref_cur == "USD",symbol != ref) %>%
    ungroup() %>% 
    dplyr::select(time_close,symbol,Descision_Achat,DR_Wallet) %>%
    rename("Achat_Symbol_VS_USD" = Descision_Achat,
           "DR_Symbol_VS_USD" = DR_Wallet)
  
  Test_Refcur_USD = Test %>%
    filter(ref_cur == "USD",symbol == ref) %>%
    ungroup() %>% 
    dplyr::select(time_close,Descision_Achat,DR_Wallet) %>%
    rename("Achat_Refcur_VS_USD" = Descision_Achat,
           "DR_Refcur_VS_USD" = DR_Wallet)
  
  Test_Symbol_Refcur = Test %>%
    filter(ref_cur == ref,symbol != ref) %>%
    ungroup() %>% 
    dplyr::select(time_close,symbol,Descision_Achat) %>%
    rename("Achat_Symbol_VS_Refcur" = Descision_Achat)
  
  Table_Achat  = Mapping %>% mutate(Combinaison_Achat = paste(`Symbol vs Ref cur`,
                                                              `Symbol vs USD`,
                                                              `Ref cur vs USD`,
                                                              sep = "-"))
  
  Test_ = Test_Symbol_USD %>% 
    left_join(Test_Refcur_USD,
              by = "time_close") %>% 
    left_join(Test_Symbol_Refcur,
              by = c("time_close", "symbol")) %>% 
    mutate(Combinaison_Achat = paste(Achat_Symbol_VS_Refcur,
                                     Achat_Symbol_VS_USD,
                                     Achat_Refcur_VS_USD,
                                     sep = "-")) %>% 
    left_join(Table_Achat,by = "Combinaison_Achat") %>%
    group_by(symbol) %>% 
    arrange(time_close) %>% 
    mutate(DR_Wallet_Final = if_else(lag(Position) == "Ref cur",
                                     DR_Refcur_VS_USD,
                                     if_else(lag(Position) == "Symbol",
                                             DR_Symbol_VS_USD,
                                             1))) %>% 
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
func_training_testing_2d <- function(DF_Train = Full_Train,symb,ref,seq_MA,func_aggregation,Years_Training,Mapping){
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
      func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = ref) %>%
      mutate(N_ETH_USD = N_symbol_USD,
             N_ETH_BTC = N_symbol_BTC,
             N_BTC_USD  = N_ref_USD,
             MovingAverage = "EMA") %>% 
      dplyr::bind_rows(Data_EMASMA %>% 
                         filter(MovingAverage == "SMA") %>% 
                         func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = ref)%>%
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
  learning_ETHVSBTC <- function(date_end_training){
    Full_Results_ETHBTC_ = Full_Results_ETHBTC %>%  
      filter(as.Date(temporality) < as.Date(date_end_training),
             as.Date(temporality) >= as.Date(date_end_training)-lubridate::years(Years_Training)) %>% 
      group_by(temporality,N_ETH_USD,N_ETH_BTC,N_BTC_USD,MovingAverage) %>% 
      summarise(Return_Wallet = prod(DR_Wallet_Final),
                Return_BTC = prod(DR_RefCur_Raw),
                Return_ETH = prod(DR_Symbol_Raw),
                .groups = "keep") %>% ungroup() %>% 
      group_by(N_ETH_USD,N_ETH_BTC,N_BTC_USD,MovingAverage) %>% 
      summarise(Return = func_aggregation(Return_Wallet-Return_BTC)+func_aggregation(Return_Wallet-Return_ETH),
                .groups = "keep") %>% 
      ungroup()  %>% 
      filter(Return == max(Return)) %>% 
      filter(N_BTC_USD == max(N_BTC_USD)) %>% 
      filter(N_ETH_USD == max(N_ETH_USD)) %>% 
      filter(N_ETH_BTC == max(N_ETH_BTC)) %>% 
      mutate(temporality = date_end_training)
    return(Full_Results_ETHBTC_)
  }
  Historical_Parameters_ETHBTC = map_df(c(as.Date(now()),Full_Results_ETHBTC %>% 
                                            pull(temporality) %>% 
                                            unique()),learning_ETHVSBTC)
  
  Full_Backtested_ETHBTC = Full_Results_ETHBTC %>% 
    inner_join(Historical_Parameters_ETHBTC,
               by = c("N_ETH_USD","N_ETH_BTC","N_BTC_USD","MovingAverage","temporality"))
  
  return(list(Historical_Parameters = Historical_Parameters_ETHBTC,
              Full_Backtested = Full_Backtested_ETHBTC))
}

func_all_curve_2d <- function(DF_Train = Full_Train,symb,ref,seq_MA,Mapping){
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
      func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = ref) %>%
      mutate(N_ETH_USD = N_symbol_USD,
             N_ETH_BTC = N_symbol_BTC,
             N_BTC_USD  = N_ref_USD,
             MovingAverage = "EMA") %>% 
      dplyr::bind_rows(Data_EMASMA %>% 
                         filter(MovingAverage == "SMA") %>% 
                         func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = ref)%>%
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
  return(Full_Results_ETHBTC)
}

func_trading_special_symbol_ref_V2 <- function(DF_Train = Full_Train,Historical_Data,Historical_Crypto,seq_MA,func_aggregation,Years_Training,Mapping,k_cluster = 3,q = 0.9){
  #First Part. Learning ETHBTC
  Data_Base_Filter_ETH_BTC = DF_Train %>% 
    filter(symbol == "ETH") %>% 
    filter(ref_cur %in% c("USD","BTC")) %>% 
    dplyr::bind_rows(DF_Train %>% 
                       filter(symbol == "BTC") %>% 
                       filter(ref_cur %in% c("USD","BTC")))
  
  func_retour_trading_alt_ETHVSBTC <- function(N_symbol_USD,N_symbol_BTC,N_ref_USD){
    Data_EMASMA = Data_Base_Filter_ETH_BTC %>% 
      filter((symbol == "ETH" & ref_cur == "USD" & N_EMA == N_symbol_USD) | 
               (symbol %in% "BTC" & ref_cur == "USD" & N_EMA == N_ref_USD) | 
               (symbol %in% "ETH" & ref_cur == "BTC" & N_EMA == N_symbol_BTC)) 
    
    Retour = Data_EMASMA %>% 
      filter(MovingAverage == "EMA") %>% 
      func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = "BTC") %>%
      mutate(N_ETH_USD = N_symbol_USD,
             N_ETH_BTC = N_symbol_BTC,
             N_BTC_USD  = N_ref_USD,
             MovingAverage = "EMA") %>% 
      dplyr::bind_rows(Data_EMASMA %>% 
                         filter(MovingAverage == "SMA") %>% 
                         func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = "BTC")%>%
                         mutate(N_ETH_USD  = N_symbol_USD,
                                N_ETH_BTC  = N_symbol_BTC,
                                N_BTC_USD =  N_ref_USD,
                                MovingAverage = "SMA") ) %>% 
      drop_na(DR_Wallet_Final)
    print(paste(N_symbol_USD,N_symbol_BTC,N_ref_USD,sep = "-"))
    return(Retour)
  } 
  Full_Results_ETHBTC = map_df(seq_MA,function(x){return(map_df(seq_MA,function(y){return(map_df(seq_MA,func_retour_trading_alt_ETHVSBTC,N_symbol_USD = x,N_symbol_BTC = y))}))}) %>% 
    mutate(temporality = lubridate::floor_date(time_close,temporality))
  learning_ETHVSBTC <- function(date_end_training){
    Full_Results_ETHBTC_ = Full_Results_ETHBTC %>%  
      filter(as.Date(temporality) < as.Date(date_end_training),
             as.Date(temporality) >= as.Date(date_end_training)-lubridate::years(Years_Training)) %>% 
      group_by(temporality,N_ETH_USD,N_ETH_BTC,N_BTC_USD,MovingAverage) %>% 
      summarise(Return_Wallet = prod(DR_Wallet_Final),
                Return_BTC = prod(DR_RefCur_Raw),
                Return_ETH = prod(DR_Symbol_Raw),
                .groups = "keep") %>% ungroup() %>% 
      group_by(N_ETH_USD,N_ETH_BTC,N_BTC_USD,MovingAverage) %>% 
      summarise(Return = func_aggregation(Return_Wallet-Return_BTC),
                .groups = "keep") %>% 
      ungroup()  %>% 
      filter(Return == max(Return)) %>% 
      filter(N_BTC_USD == max(N_BTC_USD)) %>% 
      filter(N_ETH_USD == max(N_ETH_USD)) %>% 
      filter(N_ETH_BTC == max(N_ETH_BTC)) %>% mutate(temporality = date_end_training)
    return(Full_Results_ETHBTC_)
  }
  Historical_Parameters_ETHBTC = map_df(c(as.Date(now()),Full_Results_ETHBTC %>% 
                                            pull(temporality) %>% 
                                            unique()),learning_ETHVSBTC)
  
  Full_Backtested_ETHBTC = Full_Results_ETHBTC %>% 
    inner_join(Historical_Parameters_ETHBTC,
               by = c("N_ETH_USD","N_ETH_BTC","N_BTC_USD","MovingAverage","temporality"))
  
  
  #Second part : Learning Alt
  
  Crypto_To_Keep = Historical_Crypto %>% 
    mutate(Data = 1) %>% 
    pivot_wider(names_from = symbol,values_from = Data) %>% 
    mutate_at(vars(-"Year"),function(x){return(replace(x,is.na(x),0))}) %>% 
    arrange(desc(Year)) %>% 
    mutate_at(vars(-"Year"),cumsum) %>% 
    pivot_longer(-Year,names_to = "symbol",values_to = "data_to_keep") %>% 
    filter(data_to_keep >0)
  Crypto_To_Keep = Crypto_To_Keep %>% 
    dplyr::bind_rows(Crypto_To_Keep %>% filter(Year == min(Year)) %>% mutate(Year = Year - 1)) %>% 
    dplyr::bind_rows(Crypto_To_Keep %>% filter(Year == min(Year)) %>% mutate(Year = Year - 2)) %>% 
    dplyr::bind_rows(Crypto_To_Keep %>% filter(Year == min(Year)) %>% mutate(Year = Year - 3))
  
  
  Historical_Data_Vol = Historical_Data %>%
    filter(ref_cur == "USD") %>% 
    mutate(temporality = lubridate::floor_date(time_close,temporality),
           Candle_Low = if_else(close < open,close/low,open/low)) %>% 
    group_by(temporality,symbol,ref_cur) %>% 
    summarise(Vol = log(sd(DR-1,na.rm = FALSE)),
              Candle_Low = quantile(Candle_Low,q,na.rm = TRUE),
              .groups = "keep") %>% 
    ungroup() %>% 
    filter(!is.na(Vol))
  
  DF_Train = DF_Train %>% 
    mutate(Year = year(as.Date(temporality))) %>% 
    inner_join(Crypto_To_Keep,by = c("Year","symbol"))
  
  Data_Base_Filter_BTC = DF_Train %>% 
    filter(!(symbol %in% c("BTC","ETH"))) %>% 
    filter(ref_cur %in% c("USD","BTC")) %>% 
    dplyr::bind_rows(DF_Train %>% 
                       filter(symbol == "BTC") %>% 
                       filter(ref_cur %in% c("USD")))
  
  Data_Base_Filter_ETH = DF_Train %>% 
    filter(!(symbol %in% c("BTC","ETH"))) %>% 
    filter(ref_cur %in% c("USD","ETH")) %>% 
    dplyr::bind_rows(DF_Train %>% 
                       filter(symbol == "ETH") %>% 
                       filter(ref_cur %in% c("USD")))
  
  
  Data_BTC_USD = Data_Base_Filter_BTC %>% 
    filter(symbol == "BTC") %>% 
    inner_join(Full_Backtested_ETHBTC %>% 
                 mutate(N_EMA = N_BTC_USD) %>% 
                 dplyr::select(time_close,N_EMA,MovingAverage),
               by = c("time_close","N_EMA","MovingAverage"))
  
  Data_ETH_USD = Data_Base_Filter_ETH %>% 
    filter(symbol == "ETH") %>% 
    inner_join(Full_Backtested_ETHBTC %>% 
                 mutate(N_EMA = N_ETH_USD) %>% 
                 dplyr::select(time_close,N_EMA,MovingAverage),
               by = c("time_close","N_EMA","MovingAverage"))
  
  Data_Base_Filter_BTC = Data_Base_Filter_BTC %>% filter(symbol != "BTC")
  Data_Base_Filter_ETH = Data_Base_Filter_ETH %>% filter(symbol != "ETH")
  func_retour_trading_alt <- function(N_symbol_USD,N_symbol_BTC,N_symbol_ETH){
    
    Data_EMASMA_BTC = Data_Base_Filter_BTC  %>%  
      filter(((ref_cur == "USD") & (N_EMA == N_symbol_USD)) | 
               ((ref_cur == "BTC") & (N_EMA == N_symbol_BTC))) 
    
    Data_EMASMA_ETH = Data_Base_Filter_ETH %>%  
      filter(((ref_cur == "USD") & (N_EMA == N_symbol_USD)) | 
               ((ref_cur == "ETH") & (N_EMA == N_symbol_ETH)))
    
    
    Retour_BTC = Data_EMASMA_BTC %>% 
      filter(MovingAverage == "EMA") %>% 
      dplyr::bind_rows(Data_BTC_USD) %>% 
      func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = "BTC") %>%
      mutate(N_symbol_USD = N_symbol_USD,
             N_symbol_BTC = N_symbol_BTC,
             MovingAverage = "EMA") %>% 
      dplyr::bind_rows(Data_EMASMA_BTC %>% 
                         filter(MovingAverage == "SMA") %>% 
                         dplyr::bind_rows(Data_BTC_USD) %>% 
                         func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = "BTC")%>%
                         mutate(N_symbol_USD = N_symbol_USD,
                                N_symbol_BTC = N_symbol_BTC,
                                MovingAverage = "SMA")) %>% 
      drop_na(DR_Wallet_Final) 
    
    Retour_ETH = Data_EMASMA_ETH %>% 
      filter(MovingAverage == "EMA") %>% 
      dplyr::bind_rows(Data_ETH_USD) %>% 
      func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = "ETH") %>% 
      mutate(MovingAverage = "EMA") %>% 
      dplyr::bind_rows(Data_EMASMA_ETH %>% 
                         filter(MovingAverage == "SMA") %>% 
                         dplyr::bind_rows(Data_ETH_USD) %>% 
                         func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = "ETH")%>%
                         mutate(MovingAverage = "SMA")) %>% 
      drop_na(DR_Wallet_Final) 
    
    Retour_Final = Retour_BTC %>% 
      dplyr::select(time_close,symbol,MovingAverage,DR_Symbol_VS_USD,DR_Refcur_VS_USD,DR_Wallet_Final,DR_Symbol_Raw,DR_RefCur_Raw) %>% 
      dplyr::rename("DR_BTC_VS_USD" = "DR_Refcur_VS_USD",
                    "DR_BTC_Raw" = "DR_RefCur_Raw") %>% 
      inner_join(Retour_ETH %>% 
                   dplyr::select(time_close,symbol,MovingAverage,DR_Symbol_VS_USD,DR_Refcur_VS_USD,DR_Wallet_Final,DR_Symbol_Raw,DR_RefCur_Raw) %>% 
                   dplyr::rename("DR_ETH_VS_USD" = "DR_Refcur_VS_USD",
                                 "DR_ETH_Raw" = "DR_RefCur_Raw"),
                 by = c("time_close","symbol","MovingAverage"),
                 suffix = c("_BTC","_ETH")) %>% 
      inner_join(Full_Backtested_ETHBTC %>% dplyr::select(time_close,Position),by = "time_close") %>% 
      group_by(symbol,MovingAverage) %>% 
      arrange(time_close) %>% 
      mutate(DR_Wallet_Final = if_else(lag(Position) == "Symbol",DR_Wallet_Final_ETH,DR_Wallet_Final_BTC)) %>% 
      ungroup() %>%  
      mutate(temporality = as.Date(lubridate::floor_date(time_close,temporality))) %>% 
      group_by(temporality,symbol,MovingAverage) %>% 
      drop_na(DR_Wallet_Final) %>% 
      dplyr::summarise(across(starts_with("DR"),prod)) %>% 
      mutate(N_symbol_USD = N_symbol_USD,
             N_symbol_BTC = N_symbol_BTC,
             N_symbol_ETH = N_symbol_ETH) %>% ungroup()
    
    T2 = Sys.time()
    T2-T1
    print(paste(N_symbol_USD,N_symbol_BTC,N_symbol_ETH,sep = "-"))
    return(Retour_Final)
  } 
  
  Full_Results_Alt = map_df(seq_MA,function(x){return(map_df(seq_MA,function(y){return(map_df(seq_MA,func_retour_trading_alt,N_symbol_USD = x,N_symbol_BTC = y))}))})
  
  Historical_Data_Vol = Historical_Data_Vol %>% 
    filter(!(symbol %in% c("BTC","ETH")))
  historical_parameter_alt <- function(date_end_training){
    Final_Results_Boucle = Full_Results_Alt %>%  
      filter(as.Date(temporality) < as.Date(date_end_training),
             as.Date(temporality) >= as.Date(date_end_training)-lubridate::years(Years_Training)) %>%
      filter(symbol %in% (Historical_Crypto %>% filter(Year == year(as.Date(date_end_training))) %>%   pull(symbol) %>% unique()))
    
    Historical_Data_Vol_Boucle = Historical_Data_Vol %>%  
      filter(as.Date(temporality) < as.Date(date_end_training),
             as.Date(temporality) >= as.Date(date_end_training)-lubridate::years(Years_Training)) %>%
      filter(symbol %in% (Historical_Crypto %>% filter(Year == year(as.Date(date_end_training))) %>%   pull(symbol) %>% unique())) %>% 
      group_by(symbol) %>% 
      summarise(Vol = mean(Vol),
                Candle_Low = mean(Candle_Low),.groups = "keep")%>% ungroup() %>% 
      mutate(Vol = (Vol-mean(Vol))/sd(Vol),
             Candle_Low = (Candle_Low-mean(Candle_Low))/sd(Candle_Low)) 
    
    if(nrow(Historical_Data_Vol_Boucle) > 0){
      Clusturing = Historical_Data_Vol_Boucle %>%   
        dplyr::select(Vol,Candle_Low) %>% 
        kmeans(centers = k_cluster)
      Historical_Data_Vol_Boucle = Historical_Data_Vol_Boucle %>% mutate(Cluster = Clusturing$cluster) 
    }
    
    if (nrow(Historical_Data_Vol_Boucle) == 0){
      Historical_Data_Vol_Boucle = data.frame(symbol = c(),Vol = c(),Candle_low = c())}
    Final_Results_Boucle_ = Final_Results_Boucle  %>% 
      inner_join(Historical_Data_Vol_Boucle  %>% 
                   dplyr::select(-Vol,-Candle_Low),
                 by = c("symbol")) 
    Best_Para_Per_Cluster = Final_Results_Boucle_ %>% 
      group_by(Cluster,N_symbol_USD,N_symbol_BTC,N_symbol_ETH,MovingAverage) %>% 
      summarise(Return = func_aggregation(DR_Wallet_Final-DR_BTC_Raw),
                N = n(),
                .groups = "keep") %>% 
      ungroup() %>% 
      group_by(Cluster) %>% 
      filter(Return == max(Return)) %>% 
      filter(N_symbol_USD == max(N_symbol_USD)) %>% 
      filter(N_symbol_BTC == max(N_symbol_BTC)) %>% 
      filter(N_symbol_ETH == max(N_symbol_ETH))
    
    Historical_Data_Vol_Boucle = Historical_Data_Vol_Boucle  %>% 
      dplyr::select(-Vol,-Candle_Low) %>% 
      left_join(Best_Para_Per_Cluster,
                by = "Cluster") %>% mutate(temporality = date_end_training)
    
    
    return(Historical_Data_Vol_Boucle) 
    
    print(as.Date(date_end_training))
  }
  
  Historical_Parameters_Alt = map_df(c(as.Date(unique(Full_Results_Alt %>% filter(temporality >= as.Date("2016-01-01")) %>% pull(temporality))),as.Date(now())),historical_parameter_alt)
  
  Backtested_Result_Alt = Full_Results_Alt %>% mutate(temporality = as.character(temporality)) %>% 
    inner_join(Historical_Parameters_Alt %>% mutate(temporality = as.character(temporality)),
               by = c("temporality","symbol","MovingAverage","N_symbol_USD","N_symbol_BTC","N_symbol_ETH")) 
  
  
  return(list(Backtested_Result_Alt = Backtested_Result_Alt,
              Historical_Parameters_Alt = Historical_Parameters_Alt,
              Backtested_Result_ETHBTC = Full_Backtested_ETHBTC,
              Historical_Parameters_ETHBTC = Historical_Parameters_ETHBTC))
  
}

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

Mean_Beating_SJ <- function(x){return(mean(as.numeric(x > 0))+median(x)/1000+mean(x)/1000000)}

decreasing_curve <- function(liste){
  Coeff = exp(-0.25*(1:length(liste)))
  Coeff = Coeff/sum(Coeff)
  liste = sum(liste*Coeff)
  return(liste)
}
#----------------------------------------------Parameters------------------------------------------------------------------------------------------------------------------------------------
temp = "6 months"
#----------------------------------------------FullTrain------------------------------------------------------------------------------------------------------------------------------------
memory.limit(size = 3*memory.limit())
Full_Train <- func_training_1d(Data_Base = Data %>% 
                                 filter(time_close >= as.Date("2013-01-01")) %>% 
                                 filter(symbol != "LUNA"),
                               fee = 0.075/100,
                               Liste_N = 10:60,
                               Liste_MA = c(EMA,SMA)) %>% 
  mutate(DR_Wallet = if_else(ref_cur != "USD",DR_Base,DR_Wallet))

Full_Train <- Full_Train %>% 
  mutate(temporality = as.numeric(as.Date(lubridate::floor_date(time_close,unit = temp))))
#----------------------------------------------AllCurve------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
All_Return_Against_BTC = func_all_curve_2d(DF_Train = Full_Train,
                                           symb = c("XRP","ETH","ETC","LTC"),
                                           ref = "BTC",
                                           seq_MA = seq(5,60,by = 2),
                                           Mapping = Mapping_Achat_Alt1)
All_Return_Against_ETH = func_all_curve_2d(DF_Train = Full_Train,
                                           symb = c("XRP","ETC","LTC"),
                                           ref = "ETH",
                                           seq_MA = seq(5,60,by = 2),
                                           Mapping = Mapping_Achat_Alt1)

All_Return = All_Return_Against_BTC %>% 
  mutate(ref_cur = "BTC") %>% 
  dplyr::bind_rows(All_Return_Against_ETH %>% mutate(ref_cur = "ETH")) 

All_Return_By_Temp = All_Return %>% 
  group_by(temporality,symbol,ref_cur,N_ETH_USD,N_ETH_BTC,N_BTC_USD,MovingAverage) %>% 
  summarise(across(starts_with("DR"),prod)) %>% ungroup()%>% 
  mutate(Surperf =(DR_Wallet_Final/DR_Symbol_Raw+DR_Wallet_Final/DR_RefCur_Raw)/2) 

Score <- function(x,alpha_positive,alpha_negative){
  x0 = x-1
  s = as.numeric(x0> 0)/(1+exp(-alpha_negative*x0)) + as.numeric(x0<= 0)/(1+exp(-alpha_positive*x0))
  return(s)
}
Determination_Futur_Parameters <- function(date_end_training,alpha_positive,alpha_negative){
  p = All_Return_By_Temp %>% 
    filter(temporality < date_end_training) %>% 
    mutate(Surperf = Score(Surperf,
                           alpha_positive = alpha_positive,
                           alpha_negative = alpha_negative)) %>% 
    group_by(symbol,ref_cur,N_BTC_USD,N_ETH_USD,N_ETH_BTC,MovingAverage) %>% 
    summarise(Surperf = sum(Surperf),
              .groups = "keep") %>% 
    ungroup() %>% 
    group_by(symbol,ref_cur) %>% 
    filter(Surperf == max(Surperf))  %>% 
    filter(N_BTC_USD == max(N_BTC_USD)) %>% 
    filter(N_ETH_USD == max(N_ETH_USD)) %>% 
    filter(N_ETH_BTC == max(N_ETH_BTC)) %>% 
    ungroup() %>% mutate(temporality = as.character(date_end_training),
                         alpha_positive = alpha_positive,
                         alpha_negative = alpha_negative)
  print(paste(alpha_positive,alpha_negative,date_end_training),sep = "-")
  return(p)
}

Scoring = map_df(All_Return_By_Temp %>% 
    filter(temporality != min(temporality)) %>% 
    pull(temporality) %>% 
    unique(),
  Determination_Futur_Parameters,
  alpha_positive = 2.1,
  alpha_negative = 7.1)

Final_Curve = All_Return %>%
  mutate(temporality = as.character(temporality)) %>% 
  inner_join(Final_Parameters,
             by = c("symbol","ref_cur","N_BTC_USD","N_ETH_USD","N_ETH_BTC","MovingAverage","temporality"))

Final_Curve %>% 
  filter(alpha_positive == 0.1,alpha_negative == 7.1) %>% 
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(time_close)) %>% 
  group_by(Year,wallet,symbol,ref_cur) %>% 
  summarise(Rendement = prod(DR)) %>% 
  ungroup() %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = Year,y = wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()+
  facet_wrap(symbol~ref_cur)



Final_Parameters = map_df(seq(0.1,10.1,1),
                          function(x)return({map_df(seq(0.1,10.1,1),
                                                    function(y){
                                                      return(map_df(
                                                        All_Return_By_Temp %>% 
                                                          filter(temporality != min(temporality)) %>% 
                                                          pull(temporality) %>% 
                                                          unique(),
                                                        Determination_Futur_Parameters,
                                                        alpha_positive = x,
                                                        alpha_negative = y)
                                                      )
                                                    })
                          }
                          ))

Final_Curve = All_Return  %>% 
  filter(symbol == "LTC",ref_cur == "BTC") %>%
  dplyr::select(-symbol,-ref_cur) %>% 
  mutate(temporality = as.character(temporality)) %>% 
  inner_join(Final_Parameters %>% 
               filter(symbol == "LTC",ref_cur == "BTC") %>% 
               dplyr::select(-symbol,-ref_cur),
             by = c("N_BTC_USD","N_ETH_USD","N_ETH_BTC","MovingAverage","temporality"))

Global_Perf = Final_Curve %>%mutate(Y = year(time_close)) %>%  
  group_by(alpha_positive,alpha_negative,Y) %>% 
  summarise(across(starts_with("DR"),prod)) %>% ungroup() %>% 
  mutate(Beating = (DR_Wallet_Final > DR_Symbol_Raw) & (DR_Wallet_Final > DR_RefCur_Raw)) 

Global_Perf %>% group_by(alpha_positive,alpha_negative) %>% summarise(Good = mean(Beating)) %>% View()

Final_Curve %>% 
  filter(alpha_positive == 0.1,alpha_negative == 7.1) %>% 
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(time_close)) %>% 
  group_by(Year,wallet) %>% 
  summarise(Rendement = prod(DR)) %>% ungroup() %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = Year,y = wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()


All_Return_By_Temp %>% 
  mutate(Key = paste(symbol,ref_cur,N_ETH_USD,N_ETH_BTC,N_BTC_USD,MovingAverage,sep = "-")) %>% 
  filter(Key == "ETC-ETH-20-20-20-EMA") %>% 
  ggplot(aes(x = temporality,y = DR_Wallet_Final,color = Key))+
  geom_line()+geom_point()+scale_y_log10()



#----------------------------------------------Test----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
T1 = Sys.time()
B = func_training_testing_2d(DF_Train = Full_Train,
                             symb = "ETH",
                             ref = "BTC",
                             seq_MA = seq(5,60,by = 5),
                             func_aggregation = Mean_Beating_SJ,
                             Years_Training = 5,
                             Mapping = Mapping_Achat_Alt1)

T2 = Sys.time()

B[[2]] %>%  
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(time_close)) %>% 
  group_by(Year,wallet) %>% 
  summarise(Rendement = prod(DR)) %>% ungroup() %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = Year,y = wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()

B[[2]] %>%  
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(time_close)) %>% 
  filter(Year >= 2020) %>% 
  group_by(wallet) %>% 
  arrange(time_close) %>% 
  mutate(Rendement = cumprod(DR)) %>% ungroup() %>% 
  ggplot(aes(x = time_close,y = Rendement,color = wallet)) +
  geom_line()


B[[2]] %>% ungroup() %>% 
  group_by(temporality) %>%
  summarise(across(starts_with("DR"),prod)) %>%
  pivot_longer(-temporality,names_to = "Wallet",values_to = "Rendement") %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = temporality,y = Wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()


T1 = Sys.time()
C = func_training_testing_2d_V2(Data_Base = Data,
                                DF_Train = Full_Train,
                                symb = "ETH",
                                ref = "BTC",
                                fee = 0.1/100,
                                Liste_N = seq(5,60,by = 5),
                                Liste_MA = c(SMA,EMA),
                                func_aggregation = Mean_Beating_SJ,
                                Years_Training = 3)

T2 = Sys.time()

C[[2]] %>%  
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(time_close)) %>% 
  group_by(Year,wallet) %>% 
  summarise(Rendement = prod(DR)) %>% ungroup() %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = Year,y = wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()

B[[2]] %>%  
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(time_close)) %>% 
  filter(Year >= 2020) %>% 
  group_by(wallet) %>% 
  arrange(time_close) %>% 
  mutate(Rendement = cumprod(DR)) %>% ungroup() %>% 
  ggplot(aes(x = time_close,y = Rendement,color = wallet)) +
  geom_line()


B[[2]] %>% ungroup() %>% 
  group_by(temporality) %>%
  summarise(across(starts_with("DR"),prod)) %>%
  pivot_longer(-temporality,names_to = "Wallet",values_to = "Rendement") %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = temporality,y = Wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()
#----------------------------------------------Test 3dl----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
T1 = Sys.time()
B = func_trading_special_symbol_ref_V2(DF_Train = Full_Train,
                                       Historical_Data = Data,
                                       Historical_Crypto = Historical_Snapshot,
                                       seq_MA = seq(5,60,by = 20),
                                       func_aggregation = Mean_Beating_SJ,
                                       Years_Training = 3,
                                       Mapping = Mapping_Achat_Alt1,
                                       k_cluster = 3,
                                       q = 0.9)

T2 = Sys.time()

B[[1]] %>% ungroup() %>%
  group_by(temporality) %>%
  summarise(across(starts_with("DR"),mean)) %>%
  pivot_longer(-temporality,names_to = "Wallet",values_to = "Rendement") %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = temporality,y = Wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()

B[[1]] %>% ungroup() %>%
  mutate(Year = year(as.Date(temporality))) %>% 
  group_by(Year,symbol) %>% 
  summarise(across(starts_with("DR"),prod)) %>% ungroup() %>% 
  group_by(Year) %>%
  summarise(across(starts_with("DR"),mean)) %>%
  pivot_longer(-Year,names_to = "Wallet",values_to = "Rendement") %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = Year,y = Wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()


B[[3]] %>%  
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(time_close)) %>% 
  group_by(Year,wallet) %>% 
  summarise(Rendement = prod(DR)) %>% ungroup() %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = Year,y = wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()

B[[3]] %>%  
  pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
  mutate(Year = year(time_close)) %>% 
  filter(Year >= 2020) %>% 
  group_by(wallet) %>% 
  arrange(time_close) %>% 
  mutate(Rendement = cumprod(DR)) %>% ungroup() %>% 
  ggplot(aes(x = time_close,y = Rendement,color = wallet)) +
  geom_line()


B[[3]] %>% ungroup() %>% 
  group_by(temporality) %>%
  summarise(across(starts_with("DR"),prod)) %>%
  pivot_longer(-temporality,names_to = "Wallet",values_to = "Rendement") %>% 
  mutate(Label = round(Rendement,2)) %>% 
  ggplot(aes(x = temporality,y = Wallet,fill = Rendement,label = Label))+
  geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()


B[[2]] %>% View()
B[[4]] %>% View()

#------------------------------------------------OLD-----------------------------------------------------------------------------------------------------------------------------------------------
func_training_testing_2d_V2 <- function(Data_Base = Data,DF_Train = Full_Train,symb,ref,fee,Liste_N,Liste_MA,func_aggregation,Years_Training){
  
  Apprentissage <- function(N_REF_USD,N_SYMB_REF,func_MovingAverage){
    func_MovingAverage_char =  if_else(isTRUE(all.equal(func_MovingAverage,EMA)),"EMA","SMA")
    DF_Train_ =  DF_Train %>% 
      filter((symbol == symb) & (ref_cur == ref)&(N_EMA == N_SYMB_REF)& (MovingAverage == func_MovingAverage_char)) %>%
      dplyr::bind_rows(DF_Train %>% 
                         filter((symbol == ref) & (ref_cur == "USD")&(N_EMA == N_REF_USD)& (MovingAverage == func_MovingAverage_char)))%>% 
      left_join(Data_Base %>% dplyr::select(time_close,symbol,ref_cur,open,close,high,low), 
                by = c("time_close","symbol","ref_cur")) %>% 
      dplyr::select(-temporality)
    
    EMA_Curve_symb_USD = DF_Train_ %>% 
      filter((symbol == symb) & (ref_cur == ref)&(N_EMA == N_SYMB_REF)& (MovingAverage == func_MovingAverage_char)) %>%
      dplyr::select(time_close,EMA_Curve) %>% rename(StopLoss = "EMA_Curve") %>% 
      dplyr::bind_rows(DF_Train_ %>% 
                         filter((symbol == ref) & (ref_cur == "USD")&(N_EMA == N_REF_USD)& (MovingAverage == func_MovingAverage_char))%>%
                         dplyr::select(time_close,close) %>% 
                         rename(StopLoss = "close")) %>% 
      group_by(time_close) %>% 
      summarise(N = n(),EMA_Curve = prod(StopLoss)) %>% 
      filter(N > 1) %>% 
      dplyr::select(-N)
    
    df = Data_Base %>% 
      filter(symbol == symb,ref_cur == "USD") %>% 
      left_join(EMA_Curve_symb_USD,by = "time_close") %>% 
      mutate(DR = DR-1) %>% 
      group_by(symbol,ref_cur) %>%
      mutate(Descision_Achat = as.numeric(EMA_Curve < close),
             Descision_Vente = -as.numeric(low < lag(EMA_Curve))) %>%
      drop_na(Descision_Vente) %>%
      mutate(DR_Base = 1+lag(Descision_Achat)*DR,
             DR_Fees_Base = 1-fee*abs(Descision_Achat - lag(Descision_Achat)),
             DR_Base = DR_Base*DR_Fees_Base) %>%
      drop_na(DR_Base) %>%
      mutate(DR_Wallet = if_else(Descision_Vente == -1,lag(EMA_Curve)/lag(close),DR_Base),
             DR_Wallet = if_else(lag(Descision_Achat) == 0,1,DR_Wallet),
             Mouvement_Achat = ((lag(Descision_Achat) == 0) & (Descision_Achat == 1))|((Descision_Achat == 1) & (Descision_Vente == -1)),
             Mouvement_Achat = as.numeric(Mouvement_Achat),
             Mouvement_Vente = (Descision_Vente == -1) & (lag(Descision_Achat) == 1),
             Mouvement_Vente = as.numeric(Mouvement_Vente),
             DR_Fees = (1-fee*(Mouvement_Achat+Mouvement_Vente)),
             DR_Wallet = DR_Fees*DR_Wallet) %>%
      drop_na(DR_Wallet) %>%
      mutate(DR = 1+DR) %>%
      ungroup() %>%
      dplyr::select(time_close,symbol,ref_cur,DR,DR_Base,DR_Wallet,EMA_Curve,
                    Descision_Achat,Descision_Vente,open,close,high,low)
    print(paste(N_REF_USD,N_SYMB_REF,func_MovingAverage_char),sep = "-")
    return(DF_Train_ %>% mutate(N_EMA = as.character(N_EMA)) %>% 
             dplyr::bind_rows(df) %>% 
             func_trading_alt(Mapping = Mapping_Achat_Alt1, 
                              ref = ref,fee = 0) %>%
             mutate(N_EMA = paste(N_REF_USD,N_SYMB_REF,sep = "-"),
                    MovingAverage = func_MovingAverage_char,"EMA","SMA"))
  }
  
  All_Apprentissage = map_df(Liste_N,
                             function(x){
                               return(map_df(Liste_N,
                                             function(y){
                                               return(map_df(Liste_MA,
                                                             Apprentissage,
                                                             N_REF_USD = x,
                                                             N_SYMB_REF = y))
                                             }))
                             }) %>% 
    mutate(temporality = as.numeric(as.Date(lubridate::floor_date(time_close,unit = temp))))
  learning_ETHVSBTC <- function(date_end_training){
    All_Apprentissage_ = All_Apprentissage %>%  
      filter(as.Date(temporality) < as.Date(date_end_training),
             as.Date(temporality) >= as.Date(date_end_training)-lubridate::years(Years_Training)) %>% 
      group_by(temporality,N_EMA,MovingAverage) %>% 
      summarise(Return_Wallet = prod(DR_Wallet_Final),
                Return_BTC = prod(DR_RefCur_Raw),
                Return_ETH = prod(DR_Symbol_Raw),
                .groups = "keep") %>% ungroup() %>% 
      group_by(N_EMA,MovingAverage) %>% 
      summarise(Return = func_aggregation(Return_Wallet-Return_BTC)+func_aggregation(Return_Wallet-Return_ETH),
                .groups = "keep") %>% 
      ungroup()  %>% 
      filter(Return == max(Return)) %>% 
      slice(1) %>% 
      mutate(temporality = date_end_training)
    return(All_Apprentissage_)
  }
  Historical_Parameters_ETHBTC = map_df(c(as.Date(now()),All_Apprentissage %>% 
                                            pull(temporality) %>% 
                                            unique()),learning_ETHVSBTC)
  
  Full_Backtested_ETHBTC = All_Apprentissage %>% 
    mutate(temporality = as.character(as.Date(temporality))) %>% 
    inner_join(Historical_Parameters_ETHBTC%>% 
                 mutate(temporality = as.character(temporality)),
               by = c("N_EMA","MovingAverage","temporality"))
  return(list(Historical_Parameters = Historical_Parameters_ETHBTC,
              Full_Backtested = Full_Backtested_ETHBTC))
}
# #----------------------------------------------Test all methods----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Method_2dl = Full_Train_filter  %>% 
#   func_trading_alt_2dl(Data_Base = Data,fee = 0,Liste_N_symbol = seq(10,60,by = 5),Liste_N_ref = seq(10,60,by = 5),ref = ref_coins) 
# 
# Method_2dl = Method_2dl %>% 
#   mutate(temporality = lubridate::floor_date(time_close,unit = temporality),
#          symbol_learning = if_else(symbol %in% c("ETH","BTC"),symbol,"Alt"),
#          ref_cur = ref_coins,DR_Wallet = DR_Wallet_Final,DR = DR_Wallet_Ref) %>% ungroup() %>% 
#   dplyr::select(time_close,symbol,symbol_learning,ref_cur,N_EMA,DR_Wallet,DR,temporality,MovingAverage)
# 
# Backtest_Method_2dl = Method_2dl %>% func_full_training(func_aggregation = Mean_Beating_SJ,
#                                                         Years_Training = 3)
# HP_Method_2dl = Backtest_Method_2dl[[1]]
# HR_Method_2dl = Backtest_Method_2dl[[2]]
# 
# 
# 
# Method_3dl <- Full_Train %>% func_trading_special_symbol_ref(s = c(symbol_coin),
#                                                              r = ref_coins,
#                                                              seq_MA = seq(10,60,by = 5),
#                                                              full_croisee = TRUE,
#                                                              Mapping = Mapping_Achat_Alt1) %>% 
#   mutate(temporality = lubridate::floor_date(time_close,unit = temporality),
#          symbol_learning = if_else(symbol %in% c("ETH","BTC"),symbol,"Alt"),
#          ref_cur = ref_coins,N_EMA = paste(N_symbol_BTC,N_symbol_USD,N_ref_USD,sep = "-"),
#          DR_Wallet = DR_Wallet_Final,DR = DR_RefCur_Raw) %>% ungroup() %>% 
#   dplyr::select(time_close,symbol,symbol_learning,ref_cur,N_EMA,DR_Wallet,DR,temporality,MovingAverage)
# 
# 
# Backtest_Method_3dl = Method_3dl %>% func_full_training(func_aggregation = Mean_Beating_SJ,
#                                                         Years_Training = 3)
# HP_Method_3dl = Backtest_Method_3dl[[1]]
# HR_Method_3dl = Backtest_Method_3dl[[2]]
# 
# HR_Method_3dl %>% mutate(Y = year(time_close)) %>% 
#   group_by(Y) %>% summarise(Rendement = prod(DR_Wallet),
#                             Rendement_SJ = prod(DR)) %>% 
#   view()
# 
# Method_1dl <- Full_Train_filter %>%
#   filter(ref_cur %in% c(ref_coins,"USD")) %>% 
#   mutate(symbol_learning = if_else(symbol %in% c("ETH","BTC"),symbol,"Alt")) %>%
#   func_full_training_V2(Historical_Data = Data,
#                         func_aggregation = Mean_Beating_SJ,
#                         Years_Training = 3,
#                         k_cluster = 10,
#                         spec_ETHBTC = TRUE)
# 
# Method_1dl <- Method_1dl[[1]] %>% 
#   mutate(temporality = as.Date(temporality))
# 
# # HR_Method_1dl = Full_Train_filter %>% 
# #   mutate(temporality = as.Date(temporality)) %>%
# #   mutate(symbol_learning = if_else(symbol %in% c("ETH","BTC"),symbol,"Alt")) %>% 
# #   inner_join(HP_Method_1dl  %>% 
# #               rename(temporality = Date_Training),
# #             by = c("symbol_learning","ref_cur","N_EMA","MovingAverage","temporality")) %>% 
# #   func_trading_alt(Mapping = Mapping_Achat_Alt1,ref = ref_coins)
# 
# HR_Method_1dl = Full_Train_filter %>% 
#   mutate(temporality = as.Date(temporality)) %>% 
#   inner_join(Method_1dl,
#              by = c("symbol","ref_cur","N_EMA","MovingAverage","temporality")) %>% 
#   func_trading_alt(Mapping = Mapping_Achat_Alt1,ref = ref_coins)
# #----------------------------------------------Analysis_Results----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
# All_Results = HR_Method_1dl %>% ungroup() %>% drop_na(DR_Wallet_Final) %>% 
#   rename("DR_Wallet_1dl" = DR_Wallet_Final) %>% dplyr::select(-Position) %>%  
#   inner_join(HR_Method_2dl %>% 
#                dplyr::select(time_close,symbol,DR_Wallet) %>% 
#                rename("DR_Wallet_2dl" = "DR_Wallet"),
#              by = c("time_close","symbol")) %>% 
#   inner_join(HR_Method_3dl %>% 
#                dplyr::select(time_close,symbol,DR_Wallet) %>% 
#                rename("DR_Wallet_3dl" = "DR_Wallet"),
#              by = c("time_close","symbol")) %>% 
#   pivot_longer(c(-time_close,-symbol),names_to = "wallet",values_to = "DR")
# 
# All_Results_Per_Year = All_Results %>% 
#   mutate(Year = year(time_close)) %>% 
#   group_by(Year,symbol,wallet) %>% 
#   summarise(Rendement = prod(DR))
# 
# All_Results_Per_Year %>% 
#   mutate(l = round(Rendement,3)) %>% 
#   ggplot(aes(x = wallet,y = Year,fill = Rendement,label = l))+
#   geom_tile()+geom_text()+scale_fill_viridis_c()
# 
# 
# 
# A = HR_Method_1dl %>% drop_na(DR_Wallet_Final) %>% dplyr::select(-Position) %>%   
#   pivot_longer(starts_with("DR"),names_to = "wallet",values_to = "DR") %>% 
#   group_by(time_close,wallet) %>% 
#   summarise(DR = mean(DR)) %>% ungroup()
# 
# A %>% mutate(Year = year(time_close)) %>% 
#   group_by(Year,wallet) %>% 
#   summarise(Rendement = prod(DR))%>% 
#   mutate(l = round(Rendement,3)) %>% 
#   ggplot(aes(x = wallet,y = Year,fill = Rendement,label = l))+
#   geom_tile()+geom_text()+scale_fill_viridis_c(trans = "log")
# 
# #----------------------------------------------Resultat 1dl ETHBTC------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Full_Train_filter = Full_Train %>% mutate(Year = year(time_close)) %>% 
#   inner_join(Historical_Snapshot %>% filter(Rank <= 20) %>% 
#                mutate(symbol = Symbole,
#                       Year = Annee+1) %>% 
#                dplyr::select(Year,symbol),by = c("Year","symbol")) %>% 
#   filter(symbol != "BTG")
# Retour_ETHBTC_1dl <- function(Train,Historical_Data,func_aggregation,Years_Training,k_cluster,spec_ETHBTC){
#   Method_1dl_BTC <- Train %>%
#     filter(ref_cur %in% c("BTC","USD")) %>% 
#     func_full_training_V2(Historical_Data = Historical_Data,
#                           func_aggregation = func_aggregation,
#                           Years_Training = Years_Training,
#                           k_cluster = k_cluster,
#                           spec_ETHBTC = spec_ETHBTC)
#   
#   Method_1dl_BTC <- Method_1dl_BTC[[1]] %>% 
#     mutate(temporality = as.Date(temporality))
#   
#   HR_Method_1dl_BTC = Train %>% 
#     mutate(temporality = as.Date(temporality)) %>% 
#     inner_join(Method_1dl_BTC,
#                by = c("symbol","ref_cur","N_EMA","MovingAverage","temporality")) %>% 
#     func_trading_alt(Mapping = Mapping_Achat_Alt1,ref = "BTC")
#   
#   Method_1dl_ETH <- Train %>%
#     filter(ref_cur %in% c("ETH","USD")) %>% 
#     func_full_training_V2(Historical_Data = Historical_Data,
#                           func_aggregation = func_aggregation,
#                           Years_Training = Years_Training,
#                           k_cluster = k_cluster,
#                           spec_ETHBTC = spec_ETHBTC)
#   
#   Method_1dl_ETH <- Method_1dl_ETH[[1]] %>% 
#     mutate(temporality = as.Date(temporality))
#   
#   HR_Method_1dl_ETH = Train %>% 
#     mutate(temporality = as.Date(temporality)) %>% 
#     inner_join(Method_1dl_ETH,
#                by = c("symbol","ref_cur","N_EMA","MovingAverage","temporality")) %>% 
#     func_trading_alt(Mapping = Mapping_Achat_Alt1,ref = "ETH")
#   
#   
#   HR_Method_1dl = HR_Method_1dl_BTC %>%
#     filter(symbol != "ETH") %>% dplyr::select(-Position) %>% 
#     drop_na(DR_Wallet_Final)%>% 
#     left_join(HR_Method_1dl_ETH %>%
#                 filter(symbol != "BTC") %>% dplyr::select(-Position) %>% 
#                 drop_na(DR_Wallet_Final),
#               by = c("time_close","symbol"),
#               suffix = c("_VS_BTC","_VS_ETH")) %>% 
#     left_join(HR_Method_1dl_BTC %>% filter(symbol == "ETH") %>% dplyr::select(time_close,Position),
#               by = "time_close") %>%  
#     group_by(symbol) %>% 
#     mutate(DR_Wallet_Final = if_else(lag(Position) == "Ref cur",
#                                      DR_Wallet_Final_VS_BTC,
#                                      if_else(lag(Position) == "Symbol",
#                                              DR_Wallet_Final_VS_ETH,
#                                              1))) %>% 
#     drop_na(DR_Wallet_Final) %>% 
#     ungroup()
#   
#   Method_1dl = Method_1dl_BTC %>% mutate(ref = "BTC") %>% 
#     dplyr::bind_rows(Method_1dl_ETH %>% mutate(ref = "ETH"))
#   return(list(HR_Method_1dl,
#               Method_1dl))
#   
# }
# Retour_ETHBTC_1dl_V2 <- function(Train,Historical_Data,func_aggregation,Years_Training,k_cluster,spec_ETHBTC){
#   Method_1dl_BTC <- Train %>%
#     filter(ref_cur %in% c("BTC","USD")) %>% 
#     func_full_training_V3(Historical_Data = Historical_Data,
#                           Historical_Crypto =  Historical_Snapshot %>% 
#                             filter(Rank <= 20) %>% 
#                             mutate(symbol = Symbole,Year = Annee) %>% 
#                             dplyr::select(symbol,Year) %>% 
#                             mutate(Year = Year+1),
#                           func_aggregation = func_aggregation,
#                           Years_Training = Years_Training,
#                           k_cluster = k_cluster,
#                           spec_ETHBTC = spec_ETHBTC)
#   
#   Method_1dl_BTC <- Method_1dl_BTC[[1]] %>% 
#     mutate(temporality = as.Date(temporality))
#   
#   HR_Method_1dl_BTC = Train %>% 
#     mutate(temporality = as.Date(temporality)) %>% 
#     inner_join(Method_1dl_BTC,
#                by = c("symbol","ref_cur","N_EMA","MovingAverage","temporality")) %>% 
#     func_trading_alt(Mapping = Mapping_Achat_Alt1,ref = "BTC")
#   
#   Method_1dl_ETH <- Train %>%
#     filter(ref_cur %in% c("ETH","USD")) %>% 
#     func_full_training_V3(Historical_Data = Historical_Data,
#                           Historical_Crypto =  Historical_Snapshot %>% 
#                             filter(Rank <= 20) %>% 
#                             mutate(symbol = Symbole,Year = Annee) %>% 
#                             dplyr::select(symbol,Year)%>% 
#                             mutate(Year = Year+1),
#                           func_aggregation = func_aggregation,
#                           Years_Training = Years_Training,
#                           k_cluster = k_cluster,
#                           spec_ETHBTC = spec_ETHBTC)
#   
#   Method_1dl_ETH <- Method_1dl_ETH[[1]] %>% 
#     mutate(temporality = as.Date(temporality))
#   
#   HR_Method_1dl_ETH = Train %>% 
#     mutate(temporality = as.Date(temporality)) %>% 
#     inner_join(Method_1dl_ETH,
#                by = c("symbol","ref_cur","N_EMA","MovingAverage","temporality")) %>% 
#     func_trading_alt(Mapping = Mapping_Achat_Alt1,ref = "ETH")
#   
#   
#   HR_Method_1dl = HR_Method_1dl_BTC %>%
#     filter(symbol != "ETH") %>% dplyr::select(-Position) %>% 
#     drop_na(DR_Wallet_Final)%>% 
#     left_join(HR_Method_1dl_ETH %>%
#                 filter(symbol != "BTC") %>% dplyr::select(-Position) %>% 
#                 drop_na(DR_Wallet_Final),
#               by = c("time_close","symbol"),
#               suffix = c("_VS_BTC","_VS_ETH")) %>% 
#     left_join(HR_Method_1dl_BTC %>% filter(symbol == "ETH") %>% dplyr::select(time_close,Position),
#               by = "time_close") %>%  
#     group_by(symbol) %>% 
#     mutate(DR_Wallet_Final = if_else(lag(Position) == "Ref cur",
#                                      DR_Wallet_Final_VS_BTC,
#                                      if_else(lag(Position) == "Symbol",
#                                              DR_Wallet_Final_VS_ETH,
#                                              1))) %>% 
#     drop_na(DR_Wallet_Final) %>% 
#     ungroup()
#   
#   Method_1dl = Method_1dl_BTC %>% mutate(ref = "BTC") %>% 
#     dplyr::bind_rows(Method_1dl_ETH %>% mutate(ref = "ETH"))
#   return(list(HR_Method_1dl,
#               Method_1dl))
#   
# }
# Test_3_True = Retour_ETHBTC_1dl_V2(Train = Full_Train,
#                                    Historical_Data = Data,
#                                    func_aggregation = Mean_Beating_SJ,
#                                    Years_Training = 3,
#                                    k_cluster = 3,
#                                    spec_ETHBTC = TRUE)
# 
# Test_3_False = Retour_ETHBTC_1dl_V2(Train = Full_Train,
#                                     Historical_Data = Data,
#                                     func_aggregation = Mean_Beating_SJ,
#                                     Years_Training = 3,
#                                     k_cluster = 3,
#                                     spec_ETHBTC = FALSE)
# 
# Test_2_True = Retour_ETHBTC_1dl_V2(Train = Full_Train,
#                                    Historical_Data = Data,
#                                    func_aggregation = Mean_Beating_SJ,
#                                    Years_Training = 3,
#                                    k_cluster = 2,
#                                    spec_ETHBTC = TRUE)
# 
# 
# 
# Test_2_False = Retour_ETHBTC_1dl_V2(Train = Full_Train,
#                                     Historical_Data = Data,
#                                     func_aggregation = Mean_Beating_SJ,
#                                     Years_Training = 3,
#                                     k_cluster = 2,
#                                     spec_ETHBTC = FALSE)
# 
# 
# Test_4_True = Retour_ETHBTC_1dl_V2(Train = Full_Train,
#                                    Historical_Data = Data,
#                                    func_aggregation = Mean_Beating_SJ,
#                                    Years_Training = 3,
#                                    k_cluster = 4,
#                                    spec_ETHBTC = TRUE)
# 
# Test_4_False = Retour_ETHBTC_1dl_V2(Train = Full_Train,
#                                     Historical_Data = Data,
#                                     func_aggregation = Mean_Beating_SJ,
#                                     Years_Training = 3,
#                                     k_cluster = 4,
#                                     spec_ETHBTC = FALSE)
# 
# 
# Test = Test_2_True[[2]]
# Test %>% 
#   dplyr::select(-Position)  %>%  
#   pivot_longer(c(-time_close,-symbol),names_to = "wallet",values_to = "DR") %>% 
#   group_by(time_close,wallet) %>%
#   summarise(DR = mean(DR)) %>% 
#   ungroup() %>% mutate(Year = year(time_close)) %>% 
#   group_by(Year,wallet) %>% 
#   summarise(Rendement = prod(DR)) %>% 
#   mutate(labe = round(Rendement,3)) %>% 
#   ggplot(aes(x = Year, y = wallet, fill = Rendement,label = labe))+
#   geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()
# 
# Test %>% 
#   dplyr::select(-Position)  %>%  
#   pivot_longer(c(-time_close,-symbol),names_to = "wallet",values_to = "DR") %>% mutate(Year = year(time_close)) %>%
#   group_by(Year,symbol,wallet) %>% 
#   summarise(Rendement = prod(DR)) %>%
#   ungroup() %>% 
#   group_by(Year,wallet) %>% 
#   summarise(Rendement = mean(Rendement)) %>% 
#   mutate(labe = round(Rendement,3)) %>% 
#   ggplot(aes(x = Year, y = wallet, fill = Rendement,label = labe))+
#   geom_tile()+scale_fill_viridis_c(trans = "log")+geom_text()
# 
# 
# Test %>% 
#   dplyr::select(-Position)  %>%  
#   pivot_longer(c(-time_close,-symbol),names_to = "wallet",values_to = "DR") %>% 
#   filter(year(time_close) == 2021) %>% 
#   group_by(wallet,symbol) %>%
#   summarise(Rendement = prod(DR)) %>% View
# 
# #----------------------------------------------Test 3dl------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# func_return_3dl <- function(DF_Train,Historical_Data = Data,ref = "BTC",list_MA){
#   Historical_Data_Vol = Historical_Data %>% 
#     mutate(temporality = lubridate::floor_date(time_close,temporality),
#            Candle_Low = if_else(close < open,close/low,open/low)) %>% 
#     group_by(temporality,symbol,ref_cur) %>% 
#     summarise(Vol = log(sd(DR-1,na.rm = FALSE)),
#               Candle_Low = quantile(Candle_Low,0.9,na.rm = TRUE),
#               .groups = "keep") %>% 
#     filter(Candle_Low < 10) %>% ungroup() %>% 
#     filter(!is.na(Vol))
#   
#   DF_Train_Filter = DF_Train %>% 
#     filter(ref_cur %in% c("USD",ref)) %>% 
#     dplyr::bind_rows(Data_Base %>% 
#                        filter(symbol == ref) %>% 
#                        filter(ref_cur %in% c("USD",ref)))
#   
#   func_retour_trading_alt <- function(N_symbol_USD,N_symbol_BTC,N_ref_USD){
#     Data_EMASMA = DF_Train_Filter %>% 
#       filter((symbol != ref & ref_cur == "USD" & N_EMA == N_symbol_USD) | 
#                (symbol %in% ref & ref_cur == "USD" & N_EMA == N_ref_USD) | 
#                (symbol != ref & ref_cur == ref & N_EMA == N_symbol_BTC))  %>% 
#       filter(MovingAverage == "EMA") %>% 
#       func_trading_alt(Mapping = Mapping,fee = 0.1/100,ref = ref) %>%
#       mutate(N_symbol_USD = N_symbol_USD,
#              N_symbol_BTC = N_symbol_BTC,
#              N_ref_USD  = N_ref_USD,
#              MovingAverage = "EMA") %>% 
#       dplyr::bind_rows(Data_EMASMA %>% 
#                          filter(MovingAverage == "SMA") %>% 
#                          func_trading_alt(Mapping = Mapping,fee = 0.1/100)%>%
#                          mutate(N_symbol_USD = N_symbol_USD,
#                                 N_symbol_BTC = N_symbol_BTC,
#                                 N_ref_USD  = N_ref_USD,
#                                 MovingAverage = "SMA") ) %>% 
#       drop_na(DR_Wallet_Final)
#     print(paste(N_symbol_USD,N_symbol_BTC,N_ref_USD,sep = "-"))
#     return(Data_EMASMA)
#   } 
#   Return = func_retour_trading_alt(10,12,11) %>% 
#     mutate(temporality = lubridate::floor_date(time_close,temporality))
#   
#   Summarised_Return <- function(date_end_training){
#     Return_ = Return %>%
#       filter(as.Date(temporality) < as.Date(date_end_training),
#              as.Date(temporality) >= as.Date(date_end_training)-lubridate::years(Years_Training))
#     
#     
#     Historical_Data_Vol_ = Historical_Data_Vol %>%  
#       filter(as.Date(temporality) < as.Date(date_end_training),
#              as.Date(temporality) >= as.Date(date_end_training)-lubridate::years(Years_Training)) %>%
#       group_by(symbol) %>% 
#       summarise(Vol = mean(Vol),
#                 Candle_Low = mean(Candle_Low),.groups = "keep")%>% ungroup() %>% 
#       mutate(Vol = (Vol-mean(Vol))/sd(Vol),
#              Candle_Low = (Candle_Low-mean(Candle_Low))/sd(Candle_Low)) 
#     
#     Clusturing = Historical_Data_Vol_ %>%   
#       filter(symbol %in% (Return_ %>% pull(symbol) %>% unique())) %>% 
#       dplyr::select(Vol,Candle_Low) %>% 
#       kmeans(centers = k_cluster)
#     
#     closest.cluster <- function(x) {
#       cluster.dist <- apply(Clusturing$centers, 1, function(y) sqrt(sum((x-y)^2)))
#       return(which.min(cluster.dist)[1])
#     }
#     
#     Historical_Data_Vol_ = Historical_Data_Vol_ %>% mutate(Cluster = apply(Historical_Data_Vol_ %>% dplyr::select(Vol,Candle_Low), 1, closest.cluster)) %>% 
#       mutate(spec_ETHBTC = spec_ETHBTC) %>% 
#       mutate(Cluster = if_else(spec_ETHBTC,if_else(symbol %in% c("ETH","BTC"),paste(symbol,sep = "-"),as.character(Cluster)),as.character(Cluster)))
#     
#     Return__ = Return_  %>%
#       inner_join(Historical_Data_Vol_ %>% dplyr::select(-Vol,-Candle_Low,-spe),by = c("symbol")) %>% 
#       group_by(Cluster,N_EMA,MovingAverage) %>%
#       arrange(desc(as.Date(temporality))) %>% 
#       summarise(Return = func_aggregation(Return-Return_SJ),.groups = "keep") %>%
#       ungroup()  %>% 
#       group_by(Cluster)  %>% 
#       filter(Return == max(Return)) %>% 
#       slice(1) %>% 
#       mutate(temporality = date_end_training)
#     
#     Test = Test %>% 
#       dplyr::select(-Vol,-Candle_Low) %>% 
#       left_join(DF_Train_Summarised_,by = "Cluster") %>% 
#       drop_na(N_EMA)
#     print(as.character(as.Date(date_end_training)))
#     return(Test)
#   }
# }
# 
# 

# #----------------------------------------------OLD----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Res = Full_Train  %>% 
#   func_trading_alt_2dl(Data_Base = Data,fee = 0,Liste_N_symbol = seq(10,50,by = 5),Liste_N_ref = seq(10,50,by = 5),ref = "BTC") 
# 
# Res_ = Res %>% 
#   mutate(temporality = lubridate::floor_date(time_close,unit = temporality),
#          symbol_learning = if_else(symbol %in% c("ETH","BTC"),symbol,"Alt"),
#          ref_cur = ref,DR_Wallet = DR_Wallet_Final,DR = DR_Wallet_Ref) %>% ungroup() %>% 
#   dplyr::select(time_close,symbol,symbol_learning,ref_cur,N_EMA,DR_Wallet,DR,temporality,MovingAverage)
# 
# Backtest_Full_Croisee_2dl = Res_ %>% func_full_training(func_aggregation = Mean_Beating_SJ,
#                                                                    Years_Training = 3)
# Full_Croisee_HP_2dl = Backtest_Full_Croisee_2dl[[1]]
# Full_Croisee_HR_2dl = Backtest_Full_Croisee_2dl[[2]]
# 
# Full_Croisee_HR_2dl %>% filter(symbol == "ETH") %>%  
#    left_join(Data %>% filter(ref_cur == "USD") %>%  dplyr::select(time_close,symbol,close) %>% 
#                group_by(symbol) %>% 
#                arrange(time_close) %>% 
#                mutate(DR_S = close/lag(close)) %>% 
#                dplyr::select(-close)) %>% 
#   group_by(time_close) %>% 
#   summarise(DR = mean(DR),
#             DR_Wallet = mean(DR_Wallet),
#             DR_S = mean(DR_S))  %>% 
#   ungroup() %>% 
#   arrange(time_close) %>% 
#   pivot_longer(-time_close,names_to = "wallet",values_to = "DR") -> a
# 
# a %>% 
#   filter(time_close >= as.Date("2021-01-01")) %>% 
#   group_by(wallet) %>% 
#   arrange(time_close) %>% 
#   mutate(Price = cumprod(DR)) %>% 
#   ggplot(aes(x = time_close,y = Price,color = wallet))+geom_line()
#   
#   
#   
# 
# 
# 
# 
# 
# 
# Res %>% 
#   filter(symbol == "ETH",MovingAverage == "SMA",N_EMA == c("30-30")) %>% View() 
#   filter(time_close >= as.Date("2020-01-01")) %>% 
#   pivot_longer(c(DR_Wallet_Ref,DR_Wallet_Symbol,DR_Wallet_Final),names_to = "wallet",values_to = "DR") %>% 
#   group_by(wallet) %>% 
#   arrange(time_close) %>% 
#   mutate(Price = cumprod(DR)) %>% 
#   ggplot(aes(x = time_close,y = Price,color = wallet))+geom_line()
# 
# Res %>% 
#   filter(symbol == "ETH") %>% 
#   filter(time_close >= as.Date("2020-01-01")) %>% 
#   pivot_longer(c(DR_Wallet_Ref,DR_Wallet_Symbol,DR_Wallet_Final),names_to = "wallet",values_to = "DR") %>% 
#   group_by(wallet,MovingAverage,N_EMA) %>% 
#   summarise(Rendement = prod(DR)) %>% View()
# #----------------------------------------------Apprentissage croisée----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Result_Full_Croisee <- func_trading_special_symbol_ref(Data_Base = Full_Train,
#                                                        s = c("ETH"),
#                                                        r = ref,
#                                                        seq_MA = seq(10,60,by = 5),
#                                                        full_croisee = TRUE,
#                                                        Mapping = Mapping_Achat_Alt1) %>% 
#   mutate(temporality = lubridate::floor_date(time_close,unit = temporality),
#          symbol_learning = if_else(symbol %in% c("ETH","BTC"),symbol,"Alt"),ref_cur = ref,N_EMA = paste(N_symbol_BTC,N_symbol_USD,N_ref_USD,sep = "-"),
#          DR_Wallet = DR_Wallet_Final,DR = DR_RefCur_Raw) %>% ungroup() %>% 
#   dplyr::select(time_close,symbol,symbol_learning,ref_cur,N_EMA,DR_Wallet,DR,temporality,MovingAverage)
# 
# 
# Backtest_Full_Croisee = Result_Full_Croisee %>% func_full_training(func_aggregation = Mean_Beating_SJ,
#                                           Years_Training = 3)
# Full_Croisee_HP = Backtest_Full_Croisee[[1]]
# Full_Croisee_HR = Backtest_Full_Croisee[[2]]
# #----------------------------------------------test----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Best_Test = Full_Train %>% filter(symbol == s,ref_cur == "USD",N_EMA == 30,MovingAverage == "SMA") %>% 
#   dplyr::bind_rows(Full_Train %>% filter(symbol == r,ref_cur == "USD",N_EMA == 40,MovingAverage == "SMA")) %>% 
#   dplyr::bind_rows(Full_Train %>% filter(symbol == s,ref_cur == r,N_EMA == 35,MovingAverage == "SMA")) %>% 
#   func_trading_alt(Mapping = Mapping_Achat_Alt1,ref = r,fee = 0.1/100) 
# 
# Best_Test %>% 
#   pivot_longer(c(-time_close,-symbol,-Position),names_to = "Wallet",values_to = "DR") %>% 
#   filter(time_close >= "2021-01-01") %>% 
#   group_by(Wallet) %>% 
#   arrange(time_close) %>% 
#   mutate(Price = cumprod(DR)) %>% 
#   ggplot(aes(x = time_close,y = Price,color = Wallet))+
#   geom_line()+scale_color_brewer(palette = "Set1")
# #----------------------------------------------Full Training----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Full_Trained_Curve <-   Full_Train %>%
#   mutate(symbol_learning = symbol) %>%
#   #mutate(symbol_learning = if_else(symbol %in% c("ETH","BTC"),symbol,"Alt")) %>%
#   func_full_training(func_aggregation = Mean_Beating_SJ,
#                      Years_Training = 3) 
# 
# s =  crypto
# r = ref
# Full_Trained_Strategy <- Full_Trained_Curve[[2]] %>% 
#   filter(symbol %in% c(crypto,ref)) %>% 
#   func_trading_alt(Mapping = Mapping_Achat_Alt2,
#                    ref = ref,
#                    fee = 0.01/100) %>%  
#   drop_na(DR_Wallet_Final) %>% 
#   left_join(Full_Croisee_HR %>% dplyr::select(time_close,DR_Wallet) %>% rename(DR_Wallet_FullCroisee = DR_Wallet),by = "time_close") %>%
# #  left_join(Full_Half_HR %>% dplyr::select(time_close,DR_Wallet) %>% rename(DR_Wallet_HalfCroisee = DR_Wallet),by = "time_close") %>%
#   drop_na(DR_Wallet_FullCroisee)  %>%  
#   pivot_longer(dplyr::starts_with("DR"),names_to = "Curve",values_to = "DR") 
# 
# date_debut = "2021-01-01"
# date_fin = "2023-01-01"
# Full_Trained_Strategy %>% 
#   filter(time_close <= as.Date(date_fin),
#          time_close >= as.Date(date_debut)) %>% 
#   group_by(Curve) %>% 
#   arrange(time_close) %>% 
#   mutate(Price = cumprod(DR)) %>% ungroup() %>%  
#   ggplot(aes(x = time_close,y = Price,color = Curve))+
#   geom_line()+scale_color_brewer(palette = "Set1")+scale_y_log10()
# 
# 
# Historical_Parameters <- Full_Trained_Curve[[1]] %>% 
#   mutate(Date_Training = as.Date(Date_Training))
# 
# Return_Per_Year = Full_Trained_Strategy %>% 
#   mutate(Year = lubridate::floor_date(time_close,unit = "3 months")) %>% 
#   group_by(Year,Curve) %>% 
#   summarise(Return = prod(DR)) %>% 
#   pivot_wider(names_from = "Curve",values_from = "Return")
# #--------------------------------------Training VS ETH And BTC------------------------------------------------------------
# Training_Against_ETHBTC = map_df(unique(Full_Train$symbol),func_trading_VS_ETH_BTC,fee = 0,Test = Full_Trained_Curve[[2]])
# 
# s = "ATOM"
# date_debut = "2021-01-01"
# date_fin = "2022-05-01"
# Training_Against_ETHBTC %>% 
#   filter(symbol == s,
#          time_close >= as.Date(date_debut),
#          time_close <= as.Date(date_fin)) %>% 
#   pivot_longer(starts_with("DR"),names_to = "Wallet",values_to = "DR") %>%
#   drop_na(DR) %>% 
#   group_by(symbol,Wallet) %>% 
#   arrange(time_close) %>% 
#   mutate(Price = cumprod(DR)) %>% 
#   ggplot(aes(x = time_close,y = Price,color = Wallet))+
#   geom_line()+scale_y_log10()+scale_color_brewer(palette = "Set1")
# 
# 
# s = "ATOM"
# date_debut = "2021-01-01"
# date_fin = "2022-05-01"
# Training_Against_ETHBTC %>% 
#   filter(symbol == s,
#          time_close >= as.Date(date_debut),
#          time_close <= as.Date(date_fin)) %>% 
#   pivot_longer(starts_with("DR"),names_to = "Wallet",values_to = "DR") %>% 
#   filter(Wallet == "DR") %>% 
#   mutate(Appentissage = "Joint") %>%
#   dplyr::bind_rows(Training_Against_ETHBTC_ %>% 
#                      filter(symbol == s,
#                             time_close >= as.Date(date_debut),
#                             time_close <= as.Date(date_fin)) %>% 
#                      pivot_longer(starts_with("DR"),names_to = "Wallet",values_to = "DR") %>% 
#                      filter(Wallet == "DR") %>% 
#                      mutate(Appentissage = "Solo")) %>% 
#   drop_na(DR) %>% 
#   group_by(symbol,Appentissage) %>% 
#   arrange(time_close) %>% 
#   mutate(Price = cumprod(DR)) %>% 
#   ggplot(aes(x = time_close,y = Price,color = Appentissage))+
#   geom_line()+scale_y_log10()+scale_color_brewer(palette = "Set1")
#   
