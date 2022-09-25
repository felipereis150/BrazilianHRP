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

# function to save files quickly
save_file <- function(file_name, file_path, file_content) {
  file_path <- paste0(file_path, file_name)
  return(write.csv(file_content, file_path, row.names = FALSE))
}

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

i = 1
periodo_de_amostra <- df %>% 
  dplyr::filter(Data >= '2017-06-01', Data <= '2022-06-01') %>% # 5 years window
  select(-Data) %>%  # removing the date column
  as.matrix()  # transforming the dataframe to a matrix

for (i in 1:periodo_de_amostra) {
  periodo_de_amostra[i] = HRP_Portfolio(cov(periodo_de_amostra), graph = FALSE)
}
#row.names(periodo_de_amostra[i]) <- colnames(periodo_de_amostra)
save_file("periodo_de_amostra.csv", path_out, periodo_de_amostra)


# for (i in 1:periodo_de_amostra) {
#   pesos[i] = HRP_Portfolio(cov(periodo_de_amostra), graph = FALSE)
#   retornos_da_carteira_one_step_ahead = pesos[i]*df[i + pesos - 1 + 1, -1]
# }

# save_file("pesos.csv", path_out, pesos)
# save_file("df.csv", path_out, df)
# save_file("retornos_da_carteira_one_step_ahead.csv", path_out, retornos_da_carteira_one_step_ahead)
