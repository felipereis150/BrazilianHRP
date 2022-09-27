library("HierPortfolios")
library("readxl")
library("httr")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("readr")

# function to save files quickly
save_file <- function(file_name, file_path, file_content) {
  file_path <- paste0(file_path, file_name)
  return(write.csv(file_content, file_path, row.names = FALSE))
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
df <- df %>%
  select_if(~ !any(is.na(.)))

# Troquei os nomes para o codigo ficar mais limpo
n = nrow(df)
p = ncol(df) - 1
w = matrix(NA, nrow = nrow(df)-1, ncol = ncol(df)-1) #tirando a primeira coluna

# Defino tamanho da janela (window size) e o tamanho do periodo fora da amostra (Out-of-Sample)
WS = 60
OoS = n - WS

Rport = matrix(NA, ncol = 2, nrow = OoS)  # 2 pq temos 2 estratégias: Pesos iguais e HRP.
# 5 years rolling window
for (i in 1:OoS) {   # Pq apenas 60? Vc tem uma serie de tamanho T e usará um tamanho de janela 60, ou seja, no teu periodo fora da amostra vc terá T-60 pontos.
    df_rolling = df[i:(i+WS-1), 2:p]
    df_cov = cov(df_rolling)
    w[i,] =  t(HRP_Portfolio(df_cov, graph = FALSE))
    Rport[i, 1] = mean(df[i+InS, 2:p])          # Carteria de pesos iguais realizada
    Rport[i, 2] = sum(w[i, ] * df[i+InS, 2:p])  # Carteira realizada (utilizando o HRP)
}
row.names(w) <- colnames(df)
save_file("pesos.csv", path_out, w)
save_file("Rport.csv", path_out, Rport)
  
  
