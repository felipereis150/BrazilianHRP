# importing packages
install.packages("HierPortfolios")
install.packages("readxl")
install.packages("httr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("runner")

library("HierPortfolios")
library("readxl")
library("httr")
library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("runner")

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


# working in progress - rollinw window
# df3 <- runner(
#   x = df
#   1:15
#   k = 5
#   f = sum
#   by = "month"
# )
# glimpse(df3)


# creating csv file to test clustering with 1/2 years window
half_year_window <- df %>%
  dplyr::filter(Data >= '2022-01-01', Data <= '2022-06-01')
write.csv(half_year_window, file.path(path_out, "half_year_window.csv"), row.names=FALSE)
half_year_window <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\half_year_window.csv")[ ,2:ncol(df)]
half_year_window[is.na(half_year_window)] <- 0
glimpse(half_year_window)

# creating csv file to test clustering with 1 years window
one_year_window <- df %>%
  dplyr::filter(Data >= '2021-06-01', Data <= '2022-06-01')
write.csv(one_year_window, file.path(path_out, "one_year_window.csv"), row.names=FALSE)
one_year_window <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\one_year_window.csv")[ ,2:ncol(df)]
one_year_window[is.na(one_year_window)] <- 0
glimpse(one_year_window)

# creating csv file to test clustering with 2 years window
two_years_window <- df %>%
  dplyr::filter(Data >= '2020-06-01', Data <= '2022-06-01')
write.csv(two_years_window, file.path(path_out, "two_years_window.csv"), row.names=FALSE)
two_years_window <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\two_years_window.csv")[ ,2:ncol(df)]
two_years_window[is.na(two_years_window)] <- 0
glimpse(two_years_window)

# creating csv file to test clustering with 5 years window
five_years_window <- df %>%
  dplyr::filter(Data >= '2017-06-01', Data <= '2022-06-01')
five_years_window[is.na(five_years_window)] <- 0
write.csv(five_years_window, file.path(path_out, "five_years_window.csv"), row.names=FALSE)
five_years_window <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\five_years_window.csv")[ ,2:ncol(df)]
glimpse(five_years_window)

# creating csv file to test clustering with 10 years window
ten_years_window <- df %>%
  dplyr::filter(Data >= '2012-06-01', Data <= '2022-06-01')
ten_years_window[is.na(ten_years_window)] <- 0
write.csv(ten_years_window, file.path(path_out, "ten_years_window.csv"), row.names=FALSE)
ten_years_window <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\ten_years_window.csv")[ ,2:ncol(df)]
glimpse(ten_years_window)

# creating csv file to test clustering with 20 years window
write.csv(df, file.path(path_out, "twenty_years_window.csv"), row.names=FALSE)
twenty_years_window <- read.csv("C:\\Users\\felip\\OneDrive\\Área de Trabalho\\BDAQ\\data\\twenty_years_window.csv")[ ,2:ncol(df)]
glimpse(twenty_years_window)

# plotting the data
parent_portfolio <- HRP_Portfolio(cov(five_years_window), graph = TRUE)

# # work in progress - split the dataframe into multiplie dataframes
# one_year_history <- df %>% 
#     dplyr::filter(Data >= '2021-06-01', Data <= '2022-06-01')
# one_year_history[grepl(0, unlist(one_year_history[1,]))]
# glimpse(one_year_history)

# five_years_history <- df %>% 
#     dplyr::filter(Data >= '2017-06-01', Data <= '2022-06-01')
# five_years_history[grepl(as.numeric(0.0, unlist(five_years_history[1,])))]
# glimpse(five_years_history)