#Investigacion: Impacto del tamano del Gobierno en el crecimiento Economico 
#Fecha: Julio 4, 2023

#CLEAR
rm(list = ls())

#Working directory
setwd('C:/Users/MatildeCerdaRuiz/Documents/GitHub/govsize_2023')
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
install.packages("googlesheets4")
install.packages("timeSeries")
install.packages("zoo")
install.packages("xts")
install.packages("seasonal")
install.packages("TSstudio")
install.packages("ggpubr")
install.packages("patchwork")
install.packages("urca")
install.packages("cointReg")
install.packages("xtable")
install.packages("aTSA")
install.packages("broom")

library(tidyverse)
library(xtable)#para tablas de latex
library(cointReg)#para FMOLS
library(urca)#para test de Johansen
library(dplyr)
library(googlesheets4)#para importar de G. Drive
library(lubridate)
library(seasonal)
library(TSstudio)
library(patchwork)
library(zoo)
library(seasonal)#Para desestacionalizar
library(TSstudio)#PAra desestacionalizar
library(ggpubr)
library(patchwork) # para combinar graficos
library(aTSA)
library(broom)

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


### Grafica de la serie poblacion
raw_pop <- ggplot(raw_data, aes(x = as.Date(date), y = pop)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")
raw_pop


### Debido a cambio estructural en la serie de poblacion, se procedio a reescalar la serie. 
#Para reflejar la tendencia reflejada a partir de 2013, es restar al cambio porcentual de la serie
#punto de quiebre, la tasa de crecimiento trimestral de los datos previo al quiebre en el trimestre 
# correspondiente. 

raw_data <- raw_data %>%
  mutate(
    growth_pop = ifelse(date >= "2012-10-01" & date<= "2021-04-01"  , pop/lag(pop)-1,0)
  )

for (x in 62:64) {
 raw_data[x,9] = (raw_data[x-1,9] +raw_data[x-2,9] +raw_data[x-3,9] +raw_data[x-4,9])/4
}

for (x in 28:1) {
  raw_data[x,9] = (raw_data[x+1,9] +raw_data[x+2,9] +raw_data[x+3,9] +raw_data[x+4,9])/4
}

for (x in 26:1) { 
  raw_data[x, 8] = raw_data[x+1, 8]/(1+raw_data[x+1,9])
}

for (x in 62:64) {
  raw_data[x,8] = raw_data[x-1,8]*(1+ raw_data[x,9])
}
##Grafico de poblacion reescalada 

tr_pop <- ggplot(raw_data, aes(x = as.Date(date), y = pop)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")
tr_pop
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
ts_vars <- ts(data = raw_data[,10:15],
             start = c(2006,1),
             frequency = 4
)

df <- as.data.frame(raw_data)
df$date<-as.Date(df$date,  "%m/%d/%y")


raw_plot1 <- ggplot(df, aes(x = date, y = gdp_pc)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot2 <- ggplot(df, aes(x = date, y = df[,10])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot3 <- ggplot(df, aes(x = date, y = df[,11])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot4 <- ggplot(df, aes(x = date, y = df[,12])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot5 <- ggplot(df, aes(x = date, y = df[,13])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot6 <- ggplot(df, aes(x = date, y = df[,14])) +
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

#ggplot_graph

df_seas <- as.data.frame(seasonal_adj)
df_seas <- cbind(df$date, df_seas[,])

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

#Log tranasformation
df_seas <- df_seas %>% 
  mutate(
         log_gdp_pc_s= log(gdp_pc_s),
         log_gov_gdp_s=log(gov_gdp_s),
         log_gov_con_gdp_s=log(gov_con_gdp_s),
         log_pub_inv_gdp_s=log(pub_inv_gdp_s),
         log_priv_inv_gdp_s=log(priv_inv_gdp_s),
         log_tr_op_s=log(tr_op_s)
  )

#Graph with ggplot

seas_plot1 <- ggplot(df_seas, aes(x = date, y = df_seas[,2])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")+
  theme_classic()+
  ggtitle("Producto Interno Bruto Per Capita")+
  labs(x="",y="")

seas_plot2 <- ggplot(df_seas, aes(x = date, y = df_seas[,3])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")+
  theme_classic()+
  ggtitle("Gasto de Gobierno agregado")+
  labs(x="",y="")

seas_plot3 <- ggplot(df_seas, aes(x = date, y = df_seas[,4])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")+
  theme_classic()+
  ggtitle("Gasto de Gobierno corriente")+
  labs(x="",y="")

seas_plot4 <- ggplot(df_seas, aes(x = date, y = df_seas[,5])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")+
  theme_classic()+
  ggtitle("Inverion fija publica")+
  labs(x="",y="")

seas_plot5 <- ggplot(df_seas, aes(x = date, y = df_seas[,6])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")+
  theme_classic()+
  ggtitle("Inversion fija privada")+
  labs(x="",y="")

seas_plot6 <- ggplot(df_seas, aes(x = date, y = df_seas[,7])) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")+
  theme_classic()+
  ggtitle("Apertura comercial")+
  labs(x="",y="")

combined_plot_seas <- ggarrange(seas_plot1,
                                seas_plot2,
                                seas_plot3,
                                seas_plot4,
                                seas_plot5,
                                seas_plot6,
                           nrow = 2,
                           ncol = 3) #nrow & ncol depend on how you want to #organize your plots

combined_plot_seas

##############Estacionariedad (Tony Stark)####################

### Test de Raiz Unitaria Phillips-Perron para serie en niveles 

#Creando tablas por variable para guardar resultado de pp test

variables <- df_seas[,8:13]
save <- matrix(nrow=18, ncol=3)
for (i in 1:ncol(variables)) {
col <- variables[,i]
save[[i]] <- tidy(pp.test(col))
}

### Test de Raiz Unitaria Phillips-Perron para serie en diferencias


#Incluye montar la tabla y exportarla a codigo latex


#Criterio de seleccion de rezagos Gasto Agregado (Tony Stark)



#Criterio de seleccion de rezagos Inversion Fija Publica (Tony Stark)



#Prueba de precedencia temporal Gasto Agregado  (Tony Stark)




#Prueba de presedencia temporal Inversion Fija Publica (Tony Stark)




#Prueba de cointegracion de Johansen (Euler)

jotest=ca.jo(df_seas[,2:6], type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

#Tabla


jotest_table <- rbind.data.frame(c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 col.names)=
  
  )
print(xtable(jotest_table, type="latex"))
#tablas a Latex


#Ecuacion de largo plazo Gasto Agregado (Tony Stark)




### Ecuacion de largo plazo Inversion Fija (Euler)

##### Preparing data base for regression

#inclusion of dummies


df_seas <- data.frame(df_seas,df[,15:16])


#inclusion of quadratic
df_seas <- df_seas %>%
  mutate(
    log_pub_inv_gdp_s_2= log(pub_inv_gdp_s)*log(pub_inv_gdp_s),
)
    

#inclusion of cubic

df_seas <- df_seas %>%
  mutate(
    log_pub_inv_gdp_s_3=  log_pub_inv_gdp_s_2*log(pub_inv_gdp_s),
  )

######### Estimates

####### lineal model

#####  MCO
lin_2_mco <- lm(log(gdp_pc_s) ~ log(pub_inv_gdp_s) + log(priv_inv_gdp_s) + log(tr_op_s) + d_2008 + d_2018, df_seas)
summary(lin_2_mco)

#####  FMOLS

lin_2_fmols <- cointReg(method = c("FM"), df_seas[,8], df_seas[,11:15])
print(lin_2_fmols)

#####  Canonical Cointegration Regression

#Pausa

####### Quadratic model

#####  MCO

quad_2_mco <- lm(log(gdp_pc_s) ~ log(pub_inv_gdp_s) + log(priv_inv_gdp_s) + log_pub_inv_gdp_s_2 + log(tr_op_s) + d_2008 + d_2018, df_seas)
summary(quad_2_mco)



#####  FMOLS

quad_2_fmols <- cointReg(method = c("FM"), df_seas[,8], df_seas[,11:16])
print(quad_2_fmols)

#####  Canonical Cointegration Regression

#Estimacion del tamano optiomo (Euler)




#Bootstrap (Together but Tony Stark leading)










