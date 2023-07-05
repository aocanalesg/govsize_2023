#Investigacion: Impacto del tamano del Gobierno en el crecimiento Economico 
#Fecha: Julio 4, 2023

#CLEAR
rm(list = ls())

#Working directory
setwd('/Users/axelcanales/Documents/GitHub/govsize_2023')
#Packages to install/load 
#install.packages("googlesheets4")
#install.packages("timeSeries")
#install.packages("zoo")
#install.packages("xts")
#install.packages("seasonal")
#install.packages("TSstudio")
#install.packages("ggpubr")
#install.packages("patchwork")
library(dplyr)
library(googlesheets4)
library(lubridate)
library(seasonal)
library(TSstudio)
library(ggpubr)
library(patchwork)

#Import data from Drive (Euler)

raw_data <- read_sheet("https://docs.google.com/spreadsheets/d/15_lA3MjsOMDQinHgw2A93T7tTmHdqEpOQGHSFFtkpIU/edit?usp=sharing",
           sheet = "RAW_DATA2",
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
    d_2008 = ifelse(date >= "2008-7-1" & date <= "2009-4-1" ,1,0)
  )

raw_data <- raw_data %>%
  mutate(
    d_2018 = ifelse(date >= "2017-10-1",1,0)
  )


#Time series set
ts_vars <- ts(data = raw_data[,9:14],
             start = c(2006,1),
             frequency = 4
)

df <- as.data.frame(raw_data)
df$date<-as.Date(df$date,  "%m/%d/%y")


raw_plot1 <- ggplot(df, aes(x = date, y = gdp_pc)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot2 <- ggplot(df, aes(x = date, y = df[,3])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot3 <- ggplot(df, aes(x = date, y = df[,4])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot4 <- ggplot(df, aes(x = date, y = df[,5])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot5 <- ggplot(df, aes(x = date, y = df[,6])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot6 <- ggplot(df, aes(x = date, y = df[,7])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

combined_plot <- ggarrange(raw_plot1,
                          raw_plot2,
                          raw_plot3,
                          raw_plot4,
                          raw_plot5,
                          raw_plot6,
                          nrow = 2,
                          ncol = 3) #nrow & ncol depend on how you want to #organize your plots

combined_plot
#Desestacionalizacion (Done)


seasonal_adj <- seas(x = ts_vars)
#series(seasonal_adj,c("forecast.forecasts","s12"))
seasonal_adj <- final(seasonal_adj)

#Basic graph
plot1 <- ts_plot(seasonal_adj[,1])
plot2 <- ts_plot(seasonal_adj[,2])
plot3 <- ts_plot(seasonal_adj[,3])
plot4 <- ts_plot(seasonal_adj[,4])
plot5 <- ts_plot(seasonal_adj[,5])
plot6 <- ts_plot(seasonal_adj[,6])


plot1
plot2
plot3
plot4
plot5
plot6
#ggplot_graph

df_seas <- as.data.frame(seasonal_adj)
df_seas<- cbind(df$date, df_seas[,])

df_seas <- df_seas %>% 
  rename("date" = "df$date",
         "gdp_pc_s"="gdp_pc",
         "gov_gdp_s"="gov_gdp",
         "gov_con_gdp_s"="gov_con_gdp",
         "pub_inv_gdp_s"="pub_inv_gdp",
         "priv_inv_gdp_s"="priv_inv_gdp",
         "tr_op_s"="tr_op"
         )

df_seas$date<-as.Date(df_seas$date,  "%m/%d/%y")

#Graph with ggplot

seas_plot1 <- ggplot(df_seas, aes(x = date, y = df_seas[,2])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

seas_plot2 <- ggplot(df_seas, aes(x = date, y = df_seas[,3])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

seas_plot3 <- ggplot(df_seas, aes(x = date, y = df_seas[,4])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

seas_plot4 <- ggplot(df_seas, aes(x = date, y = df_seas[,5])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

seas_plot5 <- ggplot(df_seas, aes(x = date, y = df_seas[,6])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

seas_plot6 <- ggplot(df_seas, aes(x = date, y = df_seas[,7])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

combined_plot_seas <- ggarrange(seas_plot1,
                                seas_plot2,
                                seas_plot3,
                                seas_plot4,
                                seas_plot5,
                                seas_plot6,
                           nrow = 2,
                           ncol = 3) #nrow & ncol depend on how you want to #organize your plots

combined_plot_seas



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










