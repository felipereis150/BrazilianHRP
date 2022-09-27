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
colnames(df)
str(df)

glimpse(df)

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

periodo_de_amostra = nrow(df)
pesos_da_amostra = matrix(NA,nrow = nrow(df)-1, ncol = ncol(df)-1) #tirando a primeira coluna

# 6 months rolling window
for (i in 1:60) {
    df_rolling = df[i:(i+60-1), 2:ncol(df)]
    df_cov = cov(df_rolling)
    #print (cov(df_cov)) #checando...
    pesos_da_amostra[i,] =  t(HRP_Portfolio(df_cov, graph = FALSE))
    glimpse(df_rolling)
}

  row.names(pesos_da_amostra) <- colnames(periodo_de_amostra)
  save_file("periodo_de_amostra.csv", path_out, periodo_de_amostra)
  
  
save_file("df_rolling.csv", path_out, df_rolling)
save_file("df.csv", path_out, df)
save_file("pesos_da_amostra.csv", path_out, pesos_da_amostra)

# 2 years rolling window

for (i in 1:60) {
    df_rolling = df[i:(i+60-1), 2:ncol(df)]
    df_cov = cov(df_rolling)
    #print (cov(df_cov)) #checando...
    pesos_da_amostra[i,] =  t(HRP_Portfolio(df_cov, graph = FALSE))
}

  row.names(pesos_da_amostra) <- colnames(periodo_de_amostra)
  save_file("periodo_de_amostra.csv", path_out, periodo_de_amostra)
  
  
save_file("df_rolling.csv", path_out, df_rolling)
save_file("df.csv", path_out, df)
save_file("pesos_da_amostra.csv", path_out, pesos_da_amostra)

# 5 years rolling window

for (i in 1:60) {
    df_rolling = df[i:(i+60-1), 2:ncol(df)]
    df_cov = cov(df_rolling)
    #print (cov(df_cov)) #checando...
    pesos_da_amostra[i,] =  t(HRP_Portfolio(df_cov, graph = FALSE))
}

  row.names(pesos_da_amostra) <- colnames(periodo_de_amostra)
  save_file("periodo_de_amostra.csv", path_out, periodo_de_amostra)
  
  
save_file("df_rolling.csv", path_out, df_rolling)
save_file("df.csv", path_out, df)
save_file("pesos_da_amostra.csv", path_out, pesos_da_amostra)

# 10 years rolling window

for (i in 1:60) {
    df_rolling = df[i:(i+60-1), 2:ncol(df)]
    df_cov = cov(df_rolling)
    #print (cov(df_cov)) #checando...
    pesos_da_amostra[i,] =  t(HRP_Portfolio(df_cov, graph = FALSE))
}

  row.names(pesos_da_amostra) <- colnames(periodo_de_amostra)
  save_file("periodo_de_amostra.csv", path_out, periodo_de_amostra)
  
  
save_file("df_rolling.csv", path_out, df_rolling)
save_file("df.csv", path_out, df)
save_file("pesos_da_amostra.csv", path_out, pesos_da_amostra)