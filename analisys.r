# importing packages
install.packages("HierPortfolios")
install.packages("readxl")
install.packages("httr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")

library("HierPortfolios")
library("readxl")
library("httr")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")

# reading data file
df <- read_xlsx("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\ibrx_mensal.xlsx", na = "-")
path_out <- "C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\"

# creating a dataframe with only the necessary columns and renaming the columns
colnames(df) <- str_replace(colnames(df), "Retorno\r\ndo fechamento\r\nem 1 mês\r\nEm moeda orig\r\najust p/ prov\r\n", "")
colnames(df)


# trasnforming the 'Data' column to a date format
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
glimpse(df)

# creating df to test clustering with 5 years window
five_years_window <- df %>%
  dplyr::filter(Data >= '2017-06-01', Data <= '2022-06-01')
write.csv(five_years_window, file.path(path_out, "five_years_window.csv"), row.names=FALSE)
five_years_window <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\five_years_window.csv")[ ,2:ncol(df)]


# plotting the data
parent_portfolio <- HRP_Portfolio(cov(five_years_window), graph = FALSE)
glimpse(parent_portfolio)

i = 1
janela_estimacao <- 60
periodo_fora_da_amostra <- (nrow(five_years_window) - janela_estimacao) - 1

for (i in 1:periodo_fora_da_amostra) {
  amostra_fora_da_amostra <- five_years_window[i:(i+janela_estimacao-1), ]
  write.csv(amostra_fora_da_amostra, file.path(path_out, "amostra_fora_da_amostra.csv"), row.names=FALSE)
  amostra_fora_da_amostra <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\amostra_fora_da_amostra.csv")
  amostra_fora_da_amostra <- amostra_fora_da_amostra %>%
    select_if(~ !any(is.na(.)))
  pesos = HRP_Portfolio(cov(amostra_fora_da_amostra), graph = FALSE)
  print(pesos)
  write.csv(pesos, file.path(path_out, "pesos.csv"), row.names=FALSE)
  }
