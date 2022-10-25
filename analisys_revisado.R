library(HierPortfolios)
library(readxl)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(RiskPortfolios)

# function to save files quickly
save_file <- function(file_name, file_path, file_content) {
  file_path <- paste0(file_path, file_name)
  return(write.csv(file_content, file_path, row.names = FALSE))
}

#path = "C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\ibrx_mensal.xlsx"
#path_out = "C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\"

df <- read_xlsx("/Users/ctruciosm/Downloads/bdaq_portfolio_allocation-main/data/ibrx_mensal.xlsx", na = "-")
colnames(df) <- str_replace(colnames(df), "Retorno\r\ndo fechamento\r\nem 1 mês\r\nEm moeda orig\r\najust p/ prov\r\n", "")

# transforming the 'Data' column to a date format
df <- df %>%
  mutate(Data = str_replace(Data, "Jan", "01"), 
         Data = str_replace(Data, "Fev", "02"),
         Data = str_replace(Data, "Mar", "03"), 
         Data = str_replace(Data, "Abr", "04"),
         Data = str_replace(Data, "Mai", "05"), 
         Data = str_replace(Data, "Jun", "06"),
         Data = str_replace(Data, "Jul", "07"), 
         Data = str_replace(Data, "Ago", "08"),
         Data = str_replace(Data, "Set", "09"), 
         Data = str_replace(Data, "Out", "10"),
         Data = str_replace(Data, "Nov", "11"), 
         Data = str_replace(Data, "Dez", "12")) %>% 
  mutate(Data = lubridate::my(Data)) %>% 
  dplyr::filter(Data >= '2000-01-01', Data <= '2022-06-01')

# replacing NA values with 0
df <- df %>% select_if(~ !any(is.na(.))) # 27 - 1 assets


n = nrow(df)
p = ncol(df)  # cotamos quantas colunas temos
w = matrix(NA, p - 1, p - 1) # p - 1 pois a primeira coluna são apenas datas
w_mv = matrix(NA, p - 1, p - 1) # p - 1 pois a primeira coluna são apenas datas

# Defino tamanho da janela (window size) e o tamanho do periodo fora da amostra (Out-of-Sample)
WS = 60  # Utiliamos 5 anos para estimar os pesos das carteiras
OoS = n - WS
Rport = matrix(NA, ncol = 3, nrow = OoS)  # 3 para incluir variância minima

# 5 years rolling window
for (i in 1:OoS) {
    df_rolling = df[i:(i + WS - 1), 2:p]
    df_cov = cov(df_rolling)
    w[i,] =  t(HRP_Portfolio(df_cov, graph = FALSE))
    w_mv[i,] =  optimalPortfolio(Sigma = df_cov, control = list(type = 'minvol', constraint = 'lo'))
    Rport[i, 1] = mean(as.numeric(df[WS + i, 2:p]))   # Carteria realizada de pesos iguais 
    Rport[i, 2] = sum(w[i, ] * df[WS + i, 2:p])       # Carteira realizada (utilizando o HRP)
    Rport[i, 3] = sum(w_mv[i, ] * df[WS + i, 2:p])       # Carteira realizada (utilizando o HRP)
}

colnames(Rport) <- c("Equal Weights", "HRP", "MV") # nome das colunas 
Rport <- cbind(df[-c(1:WS), 1], Rport)             # Tinha um erro no codigo

ts.plot(apply(Rport, 2, cumsum), lty = 1:3, col = c("red", "blue", "black"))

#save_file("pesos_HRP.csv", path_out, w)
#save_file("pesos_MV.csv", path_out, w_mv)
#save_file("Rport.csv", path_out, Rport)

