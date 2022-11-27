library(HierPortfolios)
library(readxl)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)
library(RiskPortfolios)
library(tidyverse)

# function to save files quickly
save_file <- function(file_name, file_path, file_content) {
  file_path <- paste0(file_path, file_name)
  return(write.csv(file_content, file_path, row.names = FALSE))
}

# function to rolling window
rolling_window_HRP <- function(data, window_size) {
  
  number_of_rows = nrow(data) # number of rows
  number_of_columns = ncol(data) # number of columns
  weights = matrix(NA, number_of_rows - 1, number_of_columns - 1) # matrix to save weights
  wheighs_mv = matrix(NA, number_of_rows - 1, number_of_columns - 1) # matrix to save weights for MV
  out_of_sample = number_of_rows - window_size # out of sample size
  Rport = matrix(NA, ncol = 3, nrow = out_of_sample) # matrix to save returns, 3 columns to store date and returns comparing with HRP and MV
  
  for (i in 1:out_of_sample) {
    df_rolling = data[i:(i + window_size - 1), 2:number_of_columns]
    df_cov = cov(df_rolling)
    weights[i,] =  t(HRP_Portfolio(df_cov, graph = FALSE))
    wheighs_mv[i,] =  optimalPortfolio(Sigma = df_cov, control = list(type = 'minvol', constraint = 'lo'))
    Rport[i, 1] = mean(as.numeric(data[window_size + i, 2:number_of_columns]))  # realized return for the period
    Rport[i, 2] = sum(weights[i, ] * data[window_size + i, 2:number_of_columns])  # realized cumulative return for the period for HRP
    Rport[i, 3] = sum(wheighs_mv[i, ] * data[window_size + i, 2:number_of_columns]) # realized cumulative return for the period for MV
  }
  
  colnames(Rport) <- c("Equal Weights", "HRP", "MV")  # column names
  Rport <- cbind(data[-c(1:window_size), 1], Rport) # add date column
  
  return(Rport)
}

path = "C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\ibrx_mensal.xlsx"
path_out = "C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\"

df <- read_xlsx("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\ibrx_mensal.xlsx", na = "-")
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

Rport = rolling_window_HRP(df, 60)
ts.plot(apply(Rport, 2, cumsum), lty = 1:3, col = c("red", "blue", "black"))
save_file("Rport.csv", path_out, Rport)

ggplot(data = Rport,
  mapping = aes(x = Data, y = HRP)) %>%
  geom_line(color = "blue") %>%
  geom_line(aes(x = Data, y = 'MV'), color = "red") %>%
  geom_line(aes(x = Data, y = 'Equal Weights'), color = "black") %>%
  labs(title = "Cumulative Returns", x = "Date", y = "Cumulative Returns") %>%
  theme_minimal()

# print(Rport)


não t te ouvindo um minuto