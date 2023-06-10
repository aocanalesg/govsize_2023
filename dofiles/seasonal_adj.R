#Working directory 
setwd ("C:/Users/Axel Canales/Documents/GitHub/govsize_2023/dofiles")

##########################################
#libraries
library(readxl)
#install.packages("readxl")
#install.packages("seasonal")
library(seasonal)
#install.packages("TSstudio")
library(TSstudio)
#install.packages("writexl")
library(writexl)

#########################################
#importing data from excel to R

raw_data<- read_excel("pre_seasonal_adj.xls")
raw_data

#Declare variables a time series
raw_data <- ts(data = raw_data,
               start = c(2006,1),
               frequency = 4,
               names = c("gdp_pc","gov_gdp","gov_con_gdp","pub_inv_gdp","priv_inv_gdp","tr_op"))
ts_plot(raw_data)

#Seasonal Adjustment

seasonal_adj <- seas(x = raw_data)
#series(seasonal_adj,c("forecast.forecasts","s12"))
seasonal_adj <- final(seasonal_adj)


ts_plot(seasonal_adj)

#Convertion from time series object to dataframe 

seasonal_adj <- data.frame(sa=as.matrix(seasonal_adj),date=time(seasonal_adj))

#Export from R to stata

write_xlsx(seasonal_adj,"C:/Users/Axel Canales/Documents/GitHub/govsize_2023/dofiles/seasonal_adj.xlsx")