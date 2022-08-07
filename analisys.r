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
df[is.na(df)] <- 0
glimpse(df)

# creating csv file to test clustering
write.csv(df, file.path(path_out, "assets.csv"), row.names=FALSE)
df2 <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\assets.csv")[ ,2:ncol(df)]
glimpse(df2)

# plotting the data
parent_portfolio <- HRP_Portfolio(cov(df2), graph = TRUE)

# work in progress - split the dataframe into multiplie dataframes
one_year_history <- df %>% 
    dplyr::filter(Data >= '2021-06-01', Data <= '2022-06-01')
one_year_history[grepl(0, unlist(one_year_history[1,]))]
glimpse(one_year_history)

five_years_history <- df %>% 
    dplyr::filter(Data >= '2017-06-01', Data <= '2022-06-01')
five_years_history[grepl(as.numeric(0.0, unlist(five_years_history[1,])))]
glimpse(five_years_history)

