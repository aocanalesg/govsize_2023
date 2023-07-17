#Investigacion: Impacto del tamano del Gobierno en el crecimiento Economico 
#Fecha: Julio 4, 2023

#CLEAR
rm(list = ls())

#Working directory
#Matilde working directory: 'C:/Users/MatildeCerdaRuiz/Documents/GitHub/govsize_2023'
#Axel working directory: '/Users/axelcanales/Documents/GitHub/govsize_2023'

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
install.packages("dplyr")
install.packages("vars")
install.packages('forecast')
install.packages('vars')
install.packages('lmtest')
install.packages('tseries')
install.packages('broom')

library(lmtest) #Test de causalidad de granger 
library(vars) #para selection de criterio de var 
library(forecast)#for lag selection VAR
library(tidyverse)#manipulation de datos en general
library(xtable)#para tablas de latex
library(cointReg)#para FMOLS
library(urca)#para test de Johansen
library(dplyr)#manipulacion de datos en general
library(googlesheets4)#para importar de G. Drive
library(TSstudio)
library(patchwork)#para combinar graficos en una imagen
library(zoo)#funciones de series de tiempo
library(seasonal)#Para desestacionalizar
library(TSstudio)#PAra desestacionalizar
library(ggpubr)
library(aTSA)
library(broom)
library(dplyr)
library(vars)
library(xtable)
library(tseries)
library(broom) #Para convertir los objetos htest (de los test estadisticos) en dataframe

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
##Graph of re-scaled population

tr_pop <- ggplot(raw_data, aes(x = as.Date(date), y = pop)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")+ 
  theme_classic()+
  ggtitle("Población")+
  labs(x="",y="")
tr_pop
#Creating variables as share of GDP per capita

raw_data <- raw_data %>%
  mutate(
    gdp_pc = gdp/pop,
    gov_gdp = (gov_con + pub_inv)/gdp,
    gov_con_gdp = gov_con/gdp,
    pub_inv_gdp= pub_inv/gdp,
    priv_inv_gdp = priv_inv/gdp,
    tr_op = (x+m)/gdp,
  )

#create growth gdp per capita

raw_data <- raw_data %>%
  mutate(
    growth_gdp_pc = gdp_pc/lag(gdp_pc)-1
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
ts_vars <- ts(data = raw_data[,10:ncol(raw_data)],
             start = c(2006,1),
             frequency = 4
)

#creating variable for date
df <- as.data.frame(raw_data)
df$date<-as.Date(df$date,  "%m/%d/%y")


raw_plot1 <- ggplot(df, aes(x = date, y = df$gdp_pc)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot2 <- ggplot(df, aes(x = date, y = df$gov_gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot3 <- ggplot(df, aes(x = date, y = df$gov_con_gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot4 <- ggplot(df, aes(x = date, y = df$pub_inv_gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot5 <- ggplot(df, aes(x = date, y = df$priv_inv_gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot6 <- ggplot(df, aes(x = date, y =  df$tr_op)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

combined_plot_raw <- ggarrange(raw_plot1,
                          raw_plot2,
                          raw_plot3,
                          raw_plot4,
                          raw_plot5,
                          raw_plot6,
                          nrow = 2,
                          ncol = 3) #nrow & ncol depend on how you want to #organize your plots

combined_plot_raw


#Desestacionalizacion (Done)

seasonal_adj <- seas(x = ts_vars[,1:7])
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

#Log tranasformation to GDP_pc

df_seas <- df_seas %>% 
  mutate(
         log_gdp_pc_s= log(gdp_pc_s)
  )

#Graph of seasonal adjusted variables with ggplot

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

##############                                      ##############     

##############    Estacionariedad (Tony Stark).    ####################

##############                                      ############## 

### Test de Raiz Unitaria ADF para serie en niveles
#Crea un dataframe con variables desestacionalizadas y en logaritmos
variables <- df_seas[,8:13] #Genera una matriz con variables desestacionalizadas y en logaritmos
variables_diff <- apply(variables, 2, diff)  #Genera una matriz con las variables en diferencias

#save_adf <- list() #Genera una lista para guardar los resultados de los test de raiz unitaria
#for (i in 1:ncol(variables)) { #Es un loop para realizar el test PP a cada variable guardada en save
 # col <- variables[,i]
  #save_adf[[i]] <- tidy(ur.df(col,type = c("none","drift","trend")))
#}
names(save_adf) <- colnames(variables) 
#adfTest(variables$log_gdp_pc_s, type = c("nc", "c", "ct"))
# "nc" for a regression with no intercept (constant) nor time trend, and "c" for
#a regression with an intercept (constant) but no time trend, "ct" for a regression
#with an intercept (constant) and a time trend.


### Test de Raiz Unitaria Phillips-Perron para serie en niveles 
##Variables en log-niveles

save_pp <- list() #Genera una lista para guardar los resultados de los test de raiz unitaria
for (i in 1:ncol(variables)) { #Es un loop para realizar el test PP a cada variable guardada en save
col <- variables[,i]
save_pp[[i]] <- tidy(pp.test(col))
}
names(save_pp) <- colnames(variables) 

### Test de Raiz Unitaria Phillips-Perron para serie en diferencias

save_pp_diff <- list() #Genera una lista para guardar los resultados de los test de raiz unitaria
for (i in 1:ncol(variables_diff)) { #Es un loop para realizar el test PP a cada variable guardada en save
  col <- variables_diff[,i]
  save_pp_diff[[i]] <- tidy(pp.test(col))
}

names(save_pp_diff) <- colnames(variables) #Asigna un nombre a cada elemento de la lista de acuerdo al nombre de las variables 

#Crear un dataframe vacio que sera la tabla de salidas para el analisis de raices unitarias
u_root <- data.frame(matrix(NA,    
                          nrow = 6,
                          ncol = 6))

#Comenzar llenado de la tabla de raices unitarias 
#Variables en niveles 
# GDP_PC: Nivel con tres especificaciones 
u_root[1,1] <- as.data.frame.list(save[[1]])[1,3]
u_root[1,2] <- as.data.frame.list(save[[1]])[2,3]
u_root[1,3] <- as.data.frame.list(save[[1]])[3,3]
# GOV_GDP: Nivel con tres especificaciones 
u_root[2,1] <- as.data.frame.list(save[[2]])[1,3]
u_root[2,2] <- as.data.frame.list(save[[2]])[2,3]
u_root[2,3] <- as.data.frame.list(save[[2]])[3,3]
# Consumo_GDP: Nivel con tres especificaciones 
u_root[3,1] <- as.data.frame.list(save[[3]])[1,3]
u_root[3,2] <- as.data.frame.list(save[[3]])[2,3]
u_root[3,3] <- as.data.frame.list(save[[3]])[3,3]
# inv_publica: Nivel con tres especificaciones 
u_root[4,1] <- as.data.frame.list(save[[4]])[1,3]
u_root[4,2] <- as.data.frame.list(save[[4]])[2,3]
u_root[4,3] <- as.data.frame.list(save[[4]])[3,3]
# inv_privada: Nivel con tres especificaciones 
u_root[5,1] <- as.data.frame.list(save[[5]])[1,3]
u_root[5,2] <- as.data.frame.list(save[[5]])[2,3]
u_root[5,3] <- as.data.frame.list(save[[5]])[3,3]
# apertura_comercial: Nivel con tres especificaciones 
u_root[6,1] <- as.data.frame.list(save[[6]])[1,3]
u_root[6,2] <- as.data.frame.list(save[[6]])[2,3]
u_root[6,3] <- as.data.frame.list(save[[6]])[3,3]

#Variables en diferencias 
# GDP_PC: Nivel con tres especificaciones 
u_root[1,4] <- as.data.frame.list(save_diff[[1]])[1,3]
u_root[1,5] <- as.data.frame.list(save_diff[[1]])[2,3]
u_root[1,6] <- as.data.frame.list(save_diff[[1]])[3,3]
# GOV_GDP: Nivel con tres especificaciones 
u_root[2,4] <- as.data.frame.list(save_diff[[2]])[1,3]
u_root[2,5] <- as.data.frame.list(save_diff[[2]])[2,3]
u_root[2,6] <- as.data.frame.list(save_diff[[2]])[3,3]
# Consumo_GDP: Nivel con tres especificaciones 
u_root[3,4] <- as.data.frame.list(save_diff[[3]])[1,3]
u_root[3,5] <- as.data.frame.list(save_diff[[3]])[2,3]
u_root[3,6] <- as.data.frame.list(save_diff[[3]])[3,3]
# inv_publica: Nivel con tres especificaciones 
u_root[4,4] <- as.data.frame.list(save_diff[[4]])[1,3]
u_root[4,5] <- as.data.frame.list(save_diff[[4]])[2,3]
u_root[4,6] <- as.data.frame.list(save_diff[[4]])[3,3]
# inv_privada: Nivel con tres especificaciones 
u_root[5,4] <- as.data.frame.list(save_diff[[5]])[1,3]
u_root[5,5] <- as.data.frame.list(save_diff[[5]])[2,3]
u_root[5,6] <- as.data.frame.list(save_diff[[5]])[3,3]
# apertura_comercial: Nivel con tres especificaciones 
u_root[6,4] <- as.data.frame.list(save_diff[[6]])[1,3]
u_root[6,5] <- as.data.frame.list(save_diff[[6]])[2,3]
u_root[6,6] <- as.data.frame.list(save_diff[[6]])[3,3]
series = c( "PIB per ́capita","Gasto de Gobierno Agregado","Gasto de Gobierno Corriente","Inversion fija Publica","Inversion Fija Privada","Apertura Comercial")
u_root$series = series
u_root <- u_root %>% relocate(series)
# Redondeando los valores en la tabla
 u_root <- u_root %>% 
   mutate(across(where(is.numeric), round, digits=2))
# Exportar codigo de raices unitarias a latex

 addtorow <- list()
 addtorow$pos <- list(0)
 addtorow$command <- c(" \\toprule
\\headrow & \\multicolumn{3}{c}{Variable en nivel} &
            \\multicolumn{3}{c}{Variable en diferencias}\\\\
  \\midrule
\\headrow Serie trimestral &
 \\multicolumn{1}{c}{N.} &
  \\multicolumn{1}{c}{I.} &
  \\multicolumn{1}{c}{I. y T.} &
  \\multicolumn{1}{c}{N.} &
  \\multicolumn{1}{c}{I.} &
  \\multicolumn{1}{c}{I. y T.} \\\\
  \\bottomrule")
 print(xtable(u_root), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )

#Criterio de seleccion de rezagos Gasto Agregado (Tony Stark)
 var_gob <-  variables[,1:2]
 lag_selection_gov <- VARselect(var_gob, lag.max = 5, type = c("const", "trend", "both", "none"),
                                                 season = NULL, exogen = NULL)
 df_lag_selection_gov <- as.data.frame(VARselect(var_gob, lag.max = 7, type = c("const", "trend", "both", "none"),
           season = NULL, exogen = NULL)[[2]])
 Criterio = c("AIC","HQ","SC","FPE")
 df_lag_selection_gov$Criterio = Criterio
 df_lag_selection_gov <-df_lag_selection_gov %>% relocate(Criterio)
 # Redondeando los valores en la tabla
 df_lag_selection_gov <- df_lag_selection_gov %>% 
   mutate(across(where(is.numeric), round, digits=2))
 
 addtorow <- list()
 addtorow$pos <- list(0)
 addtorow$command <- c(" \\toprule
\\headrow & \\multicolumn{5}{c}{Criterio de seleccion de rezagos} \\\\
  \\midrule
\\headrow Criterio &
 \\multicolumn{1}{c}{1} &
  \\multicolumn{1}{c}{2} &
  \\multicolumn{1}{c}{3} &
  \\multicolumn{1}{c}{4} &
  \\multicolumn{1}{c}{5} \\\\
  \\bottomrule")
 print(xtable(df_lag_selection_gov), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )

#Criterio de seleccion de rezagos Inversion Fija Publica (Tony Stark)
 var_inv <- variables %>%  select(log_gdp_pc_s, log_pub_inv_gdp_s)
 lag_selection_inv <- VARselect(var_inv, lag.max = 5, type = c("const", "trend", "both", "none"),
                                season = NULL, exogen = NULL)
 df_lag_selection_inv <- as.data.frame(VARselect(var_inv, lag.max = 5, type = c("const", "trend", "both", "none"),
                                                 season = NULL, exogen = NULL)[[2]])
 df_lag_selection_inv$Criterio = Criterio
 df_lag_selection_inv <-df_lag_selection_inv %>% relocate(Criterio)
 # Redondeando los valores en la tabla
 df_lag_selection_inv <- df_lag_selection_inv %>% 
   mutate(across(where(is.numeric), round, digits=2))
 
 addtorow <- list()
 addtorow$pos <- list(0)
 addtorow$command <- c(" \\toprule
\\headrow & \\multicolumn{5}{c}{Criterio de seleccion de rezagos} \\\\
  \\midrule
\\headrow Criterio &
 \\multicolumn{1}{c}{1} &
  \\multicolumn{1}{c}{2} &
  \\multicolumn{1}{c}{3} &
  \\multicolumn{1}{c}{4} &
  \\multicolumn{1}{c}{5} \\\\
  \\bottomrule")
 print(xtable(df_lag_selection_inv), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )

#Prueba de precedencia temporal Gasto Agregado  (Tony Stark)
 granger_gov_pib <- list()
 granger_pib_gov <- list()
 for (i in 1:4) { 
granger_gov_pib[[i]] <- grangertest(variables[,2],variables[,1], order = i) #Gasto de Gobierno causa a PIB
granger_pib_gov[[i]] <- grangertest(variables[,1],variables[,2], order = i) #PIB causa a Gasto de Gobierno
 }
#Prueba de precedencia temporal Inversion Fija Publica (Tony Stark)
 
 granger_inv_pib <- list()
 granger_pib_inv <- list()
 for (i in 1:4) { 
   granger_inv_pib[[i]] <- grangertest(variables[,4],variables[,1], order = i) #Gasto de Gobierno causa a PIB
   granger_pib_inv[[i]] <- grangertest(variables[,1],variables[,4], order = i) #PIB causa a Gasto de Gobierno
 }

 #Generar cuadro de precedencia temporal de granger

prueba_granger <- data.frame(matrix(NA, nrow = 4, ncol = 5))
prueba_granger[1,1] <- c("Gasto publico agregado no causa a PIB per capita")
prueba_granger[2,1] <- c("PIB per capita no causa a Gasto publico agregado")
prueba_granger[3,1] <- c("Inversion fija publica no causa a PIB per capita")
prueba_granger[4,1] <- c("PIB per capita no causa a Inversion fija publica")
###Llenado del cuadro de test de procedencia de Granger
for (i in 1:4) { 
prueba_granger[1,i+1] <- granger_gov_pib[[i]][2,4]
prueba_granger[2,i+1] <- granger_pib_gov[[i]][2,4]
prueba_granger[3,i+1] <- granger_inv_pib[[i]][2,4]
prueba_granger[4,i+1] <- granger_pib_inv[[i]][2,4]
}
## Creacion de titulos y subtitulos para exportar cuadro a latex 
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- c(" \\toprule
\\headrow & \\multicolumn{4}{c}{Numero de rezagos} \\\\
  \\midrule
\\headrow Hipotesis Nula &
 \\multicolumn{1}{c}{1} &
  \\multicolumn{1}{c}{2} &
  \\multicolumn{1}{c}{3} &
  \\multicolumn{1}{c}{4} \\\\
  \\bottomrule")
print(xtable(prueba_granger), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )


#Prueba de cointegracion de Johansen (Euler)
df_modelo1 <- as.data.frame(c(df_seas[,2:3],df_seas[,6:7]))
colnames(df_modelo1)<- c("gdp_pc_s", "gov_gdp_s", "priv_inv_gdp_s","tr_op_s")                        

df_modelo2 <- as.data.frame(cbind.data.frame(df_seas[,2],df_seas[,5:7]))
colnames(df_modelo2)<- c("gdp_pc_s", "pub_inv_gdp_s", "priv_inv_gdp_s","tr_op_s")     

#Lag selection criteria

lagselect <- VARselect(df_modelo1, lag.max=7, type = 'cons')
lagselect2 <- VARselect(df_modelo2, lag.max=7, type = 'cons')
lagselect$selection
lagselect2$selection

jotest1=ca.jo(df_modelo1, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1)
jotest2=ca.jo(df_modelo2, type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest2)

#Tabla

jotest_table <- rbind.data.frame(c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba"),
                                 c("Variable", "Tipo de prueba")
  )
view(jotest_table)
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










