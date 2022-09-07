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

i = 1
janela_estimacao <- 60  ## (60 meses = 5 anos)
periodo_fora_da_amostra <- (nrow(df) - janela_estimacao) - 1
pesos = matrix(NA, ncol = periodo_fora_da_amostra, nrow = ncol(df) - 1) # -1 é para não contar a coluna que tem as datas

for (i in 1:periodo_fora_da_amostra) {
  janela <- df[i:(i + janela_estimacao - 1), -1]
  pesos[, i] = HRP_Portfolio(cov(janela), graph = FALSE)  # salvar como matriz de dados
  retornos_da_carteira_one_step_ahead = pesos[, i]*df[i + janela_estimacao - 1 + 1, -1]  #salvo a carteira realizada, se der erro, tente com t(pesos[,i])
}

# Salvar em cada iteração demora muito, melhor salvar apenas no final
row.names(pesos) = colnames(df[,-1])  # pronto, já tem os nomes dos ativos na matriz de pesos

write.csv(pesos, file.path(path_out, "pesos.csv"), row.names=FALSE)
write.csv(retornos_da_carteira_one_step_ahead, file.path(path_out, "retornos_da_carteira_one_step_ahead.csv"), row.names=FALSE)
