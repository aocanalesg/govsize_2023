#Investigacion: Impacto del tamano del Gobierno en el crecimiento Economico 
#Fecha: Julio 4, 2023

#CLEAR
rm(list = ls())

#Working directory
setwd('C:/Users/MatildeCerdaRuiz/Documents/GitHub/govsize_2023/dofiles')
#Packages to install/load 
install.packages("googlesheets4")
install.packages("timeSeries")
install.packages("zoo")
install.packages("xts")
install.packages("seasonal")
install.packages("TSstudio")
install.packages("ggpubr")
install.packages("patchwork")
install.packages("tidyverse")
install.packages("fpp2")
library(fpp2)
library(tidyverse)
library(zoo)
library(xts)
library(dplyr)
library(googlesheets4)
library(lubridate)
library(seasonal)
library(TSstudio)
library(patchwork)

#Import data from Drive (Euler)

raw_data <- read_sheet("https://docs.google.com/spreadsheets/d/15_lA3MjsOMDQinHgw2A93T7tTmHdqEpOQGHSFFtkpIU/edit?usp=sharing",
           sheet = "RAW_DATA",
           col_names = TRUE,
           range = "A1:H65"
           )


#Data cleaning (Euler)

#rename
raw_data <- raw_data %>%
  rename( 
    "date" = "DATE",
    "gdp" = "RAW_GDP",
    "gov_con" = "RAW_GOV_CON",
    "pub_inv" = "RAW_PUB_INV",
    "priv_inv" = "RAW_PRIV_INV",
    "x" = "RAW_X",
    "m" = "RAW_M",
    "pop" = "RAW_POP"
  )

#Declare vector of names 
var_names_bcn <- c("date", "gdp", "gov_con", "pub_inv", "priv_inv", "x", "m", "pop")


#Rescale variables from BCN to millions of cords
raw_data <- raw_data %>%
  mutate(
    gdp = gdp*10^6,
    gov_con =gov_con*10^6,
    pub_inv= pub_inv*10^6,
    priv_inv=priv_inv*10^6,
    x=x*10^6,
    m=m*10^6
         )


#Quiebre estructural debido a cambios metodologicos 
# Para recuperar la tendencia reflejada a partir de 2013, se restar al cambio porcentual de la serie
# en el punto de quiebre, la tasa de crecimiento trimestral de los datos previo al quiebre en el trimestre correspondiente.

raw_data <- raw_data %>%
  mutate(growth_rate_pop = ifelse(!is.na(pop),(pop - lag(pop))/lag(pop),0))



#growth_rate_pop <- 0
#for (val in raw_data$pop) {
  #if(val %% 2 == 0)  growth_rate_pop = (raw_data$pop/raw_data$pop)-1
#}
#print(count)


#Variables as share of PIB per capita
raw_data <- raw_data %>%
  mutate(
    gdp_pc = gdp/pop,
    gov_gdp = (gov_con + pub_inv)/gdp,
    gov_con_gdp = gov_con/gdp,
    pub_inv_gdp= pub_inv/gdp,
    priv_inv_gdp = priv_inv/gdp,
    tr_op = (x+m)/gdp,
  )


#create dummies 
raw_data <- raw_data %>%
  mutate(
    d_2008 = ifelse(date >= "2008-7-1" & date <= "2009-4-1" ,1,0)
  )

raw_data <- raw_data %>%
  mutate(
    d_2018 = ifelse(date >= "2017-10-1",1,0)
  )


#Time series set
ts_vars <- ts(data = cbind(raw_data[,1],raw_data[,9:16]),
             start = c(2006,1),
             frequency = 4
)

df <- as.data.frame(ts_vars)
df$date<-as.Date(df$date)

ts_plot(ts_vars[,1])
ts_plot(ts_vars[,2])
ts_plot(ts_vars[,3])
ts_plot(ts_vars[,4])
ts_plot(ts_vars[,5])
ts_plot(ts_vars[,6])

ggplot(df, aes(x = date, y = df[,2])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

#Desestacionalizacion (Done)


seasonal_adj <- seas(x = ts_vars[,2:7])
#series(seasonal_adj,c("forecast.forecasts","s12"))
seasonal_adj <- final(seasonal_adj)

#Basic graph
plot1 <- ts_plot(seasonal_adj[,1])
plot2 <- ts_plot(seasonal_adj[,2])
plot3 <- ts_plot(seasonal_adj[,3])
plot4 <- ts_plot(seasonal_adj[,4])
plot5 <- ts_plot(seasonal_adj[,5])
plot6 <- ts_plot(seasonal_adj[,6])

#Graph with ggplot



combined_plot <- ggarrange(plot1,
                           plot2,
                           plot3,
                           plot4,
                           plot5,
                           plot6,
                           nrow = 2,
                           ncol = 3) #nrow & ncol depend on how you want to 
#organize your plots

plot1 + plot2 + plot3 + plot4
#Estacionariedad (Tony Stark)




#Incluye montar la tabla y exportarla a codigo latex
#Criterio de seleccion de rezagos Gasto Agregado (Tony Stark)



#Criterio de seleccion de rezagos Inversion Fija Publica (Tony Stark)



#Prueba de presedencia temporal Gasto Agregado  (Tony Stark)




#Prueba de presedencia temporal Inversion Fija Publica (Tony Stark)




#Prueba de cointegracion (Euler)


#Ecuacion de largo plazo Gasto Agregado (Tony Stark)




#Ecuacion de largo plazo Inversion Fija (Euler)




#Estimacion del tamano optiomo (Euler)




#Bootstrap (Together but Tony Stark leading)










