#Investigacion: Impacto del tamano del Gobierno en el crecimiento Economico 
#Fecha: Julio 4, 2023

#CLEAR
rm(list = ls())

#Working directory
#Matilde working directory: 'C:/Users/MatildeCerdaRuiz/Documents/GitHub/govsize_2023'
#Axel working directory: '/Users/axelcanales/Documents/GitHub/govsize_2023'

setwd('C:/Users/MatildeCerdaRuiz/Documents/GitHub/govsize_2023')
path <- getwd()

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
install.packages("stargazer")
install.packages("writexl")

library(writexl)#para exportar el excel
library(vars)
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
library(patchwork) # para combinar graficos
library(aTSA)
library(broom)
library(dplyr)
library(stargazer)
library(vars)

#Import data from Drive (Euler)

raw_data <- read_sheet("https://docs.google.com/spreadsheets/d/15_lA3MjsOMDQinHgw2A93T7tTmHdqEpOQGHSFFtkpIU/edit?usp=sharing",
           sheet = "RAW_DATA",
           col_names = TRUE,
           range = "A1:H65"
           )

raw_data2 <- read_sheet("https://docs.google.com/spreadsheets/d/15_lA3MjsOMDQinHgw2A93T7tTmHdqEpOQGHSFFtkpIU/edit?usp=sharing",
                       sheet = "RAW_DATA2",
                       col_names = TRUE,
                       range = "A1:H65"
)

#bcn_data_junio_2023 <- read_sheet("https://docs.google.com/spreadsheets/d/11j04635-SOfd4rqdz2snU6SemmZaW5bb/edit?usp=sharing&ouid=116574696867256574492&rtpof=true&sd=true",
#                                  sheet = "Gasto",
#                                  col_names = TRUE,
#                                  range = "A35:CB59"
#)

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
  theme(plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 11, face = "italic"),
        plot.caption.position = "plot",
        )+
  ggtitle("Población")+
  labs(x="",
       y="",
    title = "Población total", 
       subtitle = "Habitantes", 
       caption = "Fuente: Elaboración y cálculos propios con base en datos de INIDE")
tr_pop

ggsave("population_con_titulo.png", width=10, height =7 , units= c("cm"), dpi=500)



tr_pop_2 <- ggplot(raw_data, aes(x = as.Date(date), y = pop)) +
  geom_line() +
  scale_x_date(date_breaks = "years" , date_labels = "%b %Y")+ 
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 14, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 11, face = "italic"),
        plot.caption.position = "plot",
    axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Población")+
  labs(x="",
       y="")
   #    title = "", 
    #   subtitle = "", 
     #  caption = "")
tr_pop_2

ggsave("population_sin_titulo.png", width=10, height =7 , units= c("cm"), dpi=500)


#validacion, DELETE LATER

validacion_pop <- data_frame(raw_data$date, raw_data$pop, raw_data2$RAW_POP)

validacion_pop <- validacion_pop %>%
mutate(
  diff = raw_data$pop-raw_data2$RAW_POP
)


validacion_pop <- ggplot(validacion_pop, aes(as.Date(raw_data$date))) +
  geom_line(aes(x = as.Date(raw_data$date), y=raw_data$pop)) +
  geom_line(aes(x = as.Date(raw_data$date), y=raw_data2$RAW_POP)) 
 
validacion_pop

#Creating variables as share of GDP per capita

raw_data <- raw_data %>%
  mutate(
    gdp_pc = log(gdp/pop),
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

#Graph of seasonal adjusted variables with ggplot

seas_plot1 <- ggplot(df_seas, aes(x = date, y = df_seas[,2])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Producto Interno Bruto Per Capita")+
  labs(x="",y="",title = "Producto Interno Bruto Per Capita", 
       subtitle = "Cordobas constantes por habitante", )

seas_plot2 <- ggplot(df_seas, aes(x = date, y = df_seas[,3])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(x="",y="",title = "Gasto de Goberno agregado", 
       subtitle = "Porcentaje respecto al PIB p.c.", )

seas_plot3 <- ggplot(df_seas, aes(x = date, y = df_seas[,4])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Producto Interno Bruto Per Capita")+
  labs(x="",y="",title = "Gasto de Goberno corriente", 
       subtitle = "Porcentaje respecto al PIB p.c.", )

seas_plot4 <- ggplot(df_seas, aes(x = date, y = df_seas[,5])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Producto Interno Bruto Per Capita")+
  labs(x="",y="",title = "Inversión fija pública", 
       subtitle = "Porcentaje respecto al PIB p.c.", )

seas_plot5 <- ggplot(df_seas, aes(x = date, y = df_seas[,6])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Inversion Fija Privada")+
  labs(x="",y="",title = "Inversión fija privada", 
       subtitle = "Porcentaje respecto al PIB p.c.", )

seas_plot6 <- ggplot(df_seas, aes(x = date, y = df_seas[,7])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 10, face = "bold"),
        plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Apertura Comercial")+
  labs(x="",y="",title = "Apertura comercial", 
       subtitle = "Porcentaje respecto al PIB p.c.", )

combined_plot_seas <- ggarrange(seas_plot1,
                                seas_plot2,
                                seas_plot3,
                                seas_plot4,
                                seas_plot5,
                                seas_plot6,
                           nrow = 2,
                           ncol = 3) #nrow & ncol depend on how you want to #organize your plots

combined_plot_seas
ggsave("variables_sin_titulo.png", width=24, height =14 , units= c("cm"), dpi=500)
#########
#-- Scatterplots 
#########


seas_plot7 <- ggplot(df_seas, aes(x =df_seas[,3])) +
  geom_point(aes (y=df_seas[,2]), shape=16)+
  geom_smooth(aes(y=df_seas[,2]), method="lm", formula = y ~ x + I(x^2), se=FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  #      plot.caption = element_text(hjust = 0),
  #      plot.title.position = "plot",
  #      plot.title = element_text(color = "black", size = 10, face = "bold"),
  #      plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
  #      plot.caption.position = "plot",
  #      axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(x="",y="")
  #     title = "Titulo", 
   #    subtitle = "Subtitulo", )
seas_plot7
ggsave("gdp_vs_aggregate_exp.png", width=24, height =14 , units= c("cm"), dpi=500)


seas_plot8 <- ggplot(df_seas, aes(x =df_seas[,5])) +
  geom_point(aes (y=df_seas[,2]), shape=16)+
  geom_smooth(aes(y=df_seas[,2]), method="lm", formula = y ~ x + I(x^2), se=FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
        #      plot.caption = element_text(hjust = 0),
        #      plot.title.position = "plot",
        #      plot.title = element_text(color = "black", size = 10, face = "bold"),
        #      plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        #      plot.caption.position = "plot",
        #      axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(x="",y="")
seas_plot8 
ggsave("gdp_vs_public_inv.png", width=24, height =14 , units= c("cm"), dpi=500)



exploratory_analysis_tex <- stargazer(df_seas[,2:7])
stargazer(df_seas[,2:7], type='text')

##############                                      ##############     

##############    Estacionariedad (Tony Stark).    ####################

##############                                      ##############   

### Test de Raiz Unitaria ADF para serie en niveles 
##Variables en log-niveles
variables <- df_seas[,8:13] #Crea un dataframe con variables desestacionalizadas y en logaritmos

### Test de Raiz Unitaria Phillips-Perron para serie en niveles 
##Variables en log-niveles
save <- list() #Genera una lista para guardar los resultados de los test de raiz unitaria
for (i in 1:ncol(variables)) { #Es un loop para realizar el test PP a cada variable guardada en save
col <- variables[,i]
save[[i]] <- tidy(pp.test(col))
}
names(save) <- colnames(variables) 

### Test de Raiz Unitaria Phillips-Perron para serie en diferencias
variables_diff <- apply(variables, 2, diff)
save_diff <- list() #Genera una lista para guardar los resultados de los test de raiz unitaria
for (i in 1:ncol(variables_diff)) { #Es un loop para realizar el test PP a cada variable guardada en save
  col <- variables_diff[,i]
  save_diff[[i]] <- tidy(pp.test(col))
}

names(save_diff) <- colnames(variables) #Asigna un nombre a cada elemento de la lista de acuerdo al nombre de las variables 

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
 df_lag_selection_gov <- as.data.frame(VARselect(var_gob, lag.max = 5, type = c("const", "trend", "both", "none"),
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


 #Prueba de presedencia temporal De Gasto Gobierno Agregado (Tony Stark)
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

 #########                                                 #########
 
 #########  Prueba de cointegracion de Johansen (Euler)
 
 #########                                                 #########
 
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






#########                                                 #########

#########  Ecuacion de largo plazo Gasto Agregado (Tony Stark)

#########                                                 #########






#########                                                 #########

######### Ecuacion de largo plazo - Var. dep.: Inversion Fija (Euler) 

#########                                               #########  

#inclusion of dummies to the variables object

df_modelo1 <- data.frame(df_modelo1, df[,17:18])

df_modelo2 <- data.frame(df_modelo2, df[,17:18]) 

### Exporting data for Eviews 
write_xlsx(raw_data, paste(path,"raw_data.xlsx", sep="/"))
write_xlsx(df_seas, paste(path,"df_seas.xlsx", sep="/"))
write_xlsx(df_modelo1, paste(path,"df_modelo1.xlsx", sep="/"))
write_xlsx(df_modelo2, paste(path,"df_modelo2.xlsx", sep="/"))

#creation of quadratic term for gov. expenditure variables
df_modelo1 <- df_modelo2 %>%
  mutate(
    gov_gdp_s_2= gov_gdp_s*gov_gdp_s
  )


df_modelo2 <- df_modelo2 %>%
  mutate(
    pub_inv_gdp_s_2= pub_inv_gdp_s*pub_inv_gdp_s
)
    

#inclusion of cubic

df_modelo2 <- df_modelo2 %>%
  mutate(
    pub_inv_gdp_s_3=  pub_inv_gdp_s_2*pub_inv_gdp_s,
  )

df_modelo1 <- df_modelo1 %>%
  mutate(
    gov_gdp_s_3=  gov_gdp_s_2*gov_gdp_s,
  )

# export the data for Eviews processing

write_xlsx(df_modelo2, paste(path,"df_modelo2.xlsx", sep="/"))

######### Estimates

####### linear model

#####  MCO
lin_2_mco <- lm(log(gdp_pc_s) ~ pub_inv_gdp_s + priv_inv_gdp_s + tr_op_s + d_2008 + d_2018, df_modelo2)
summary(lin_2_mco)
stargazer(lin_2_mco, type="text")

#####  FMOLS

lin_2_fmols <- cointReg(method = c("FM"), df_modelo2[,1], df_modelo2[,2:7])
print(lin_2_fmols)
tidy(lin_2_fmols)

res = sapply(c("FM", "D", "IM"), cointReg, x = df_modelo2[,1], y = df_modelo2[,2:7],)
do.call(cbind, lapply(res, "[[", "theta"))

test.fm = cointRegFM(x = df_modelo2[,1], y = df_modelo2[,2:7])
      print(test.fm, digits = 4)

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










