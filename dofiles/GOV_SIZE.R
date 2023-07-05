#Investigacion: Impacto del tamano del Gobierno en el crecimiento Economico 
#Fecha: Julio 4, 2023

#CLEAR
rm(list = ls())

#Workind directory
setwd('/Users/axelcanales/Documents/GitHub/govsize_2023')
#Packages
#install.packages("googlesheets4")
#install.packages("timeSeries")
#install.packages("zoo")
#install.packages("xts")
library(xts)
library(dplyr)
library(googlesheets4)
library(lubridate)
library(zoo)

#Importar data (Euler)

raw_data <- read_sheet("https://docs.google.com/spreadsheets/d/15_lA3MjsOMDQinHgw2A93T7tTmHdqEpOQGHSFFtkpIU/edit?usp=sharing",
           sheet = "RAW_DATA2",
           col_names = TRUE,
           range = "A1:H65"
           )


#Limpieza datos (Euler)

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

var_names_bcn <- c("date", "gdp", "gov_con", "pub_inv", "priv_inv", "x", "m", "pop")




#rescale variables from BCN to millions of cords
raw_data <- raw_data %>%
  mutate(
    gdp = gdp*10^6,
    gov_con =gov_con*10^6,
    pub_inv= pub_inv*10^6,
    priv_inv=priv_inv*10^6,
    x=x*10^6,
    m=m*10^6
         )


#Backast (Tony Stark)

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
    d_2008 = ifelse(date >= "2008-10-1" & date <= "2009-1-1" ,1,0)
  )


#Time series set
ts_vars <- ts(data = raw_data,
              start = c(2006,1),
              frequency = 4
)

#ts_vars[,1] <- as.yearqtr(ts_vars[,1],           # Convert dates to quarterly
 #          format = "%Y-%m-%d")




#Desestacionalizacion (Done)



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










