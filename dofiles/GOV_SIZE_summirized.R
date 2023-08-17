#Investigacion: Impacto del tamano del Gobierno en el crecimiento Economico 
#Fecha: Julio 4, 2023

#CLEAR
rm(list = ls())

#Working directory
#Matilde working directory: 'C:/Users/MatildeCerdaRuiz/Documents/GitHub/govsize_2023'
#Axel working directory: '/Users/axelcanales/Documents/GitHub/govsize_2023'
#Axel working directory WINDOWS: 'C:\Users\Axel Canales\Documents\GitHub\govsize_2023'

setwd('C:/Users/Axel Canales/Documents/GitHub/govsize_2023')
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
install.packages("fUnitRoots")

library(patchwork)#para combinar graficos en una imagen

library(ggplot2)
library(fUnitRoots)
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

library(zoo)#funciones de series de tiempo
library(seasonal)#Para desestacionalizar
library(TSstudio)#PAra desestacionalizar
#library(ggpubr)
library(aTSA) #pptest
library(broom)
library(dplyr)
library(stargazer)
library(vars)

#Import data from Drive

raw_data <- read_sheet("https://docs.google.com/spreadsheets/d/15_lA3MjsOMDQinHgw2A93T7tTmHdqEpOQGHSFFtkpIU/edit?usp=sharing",
           sheet = "RAW_DATA3",
           col_names = TRUE,
           range = "A1:H69"
           )


#Data procesing

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

for (x in 50:68) {
 raw_data[x,9] = (raw_data[x-1,9] +raw_data[x-2,9] +raw_data[x-3,9] +raw_data[x-4,9])/4
}

for (x in 28:1) {
  raw_data[x,9] = (raw_data[x+1,9] +raw_data[x+2,9] +raw_data[x+3,9] +raw_data[x+4,9])/4
}

for (x in 26:1) { 
  raw_data[x, 8] = raw_data[x+1, 8]/(1+raw_data[x+1,9])
}

for (x in 50:68) {
  raw_data[x,8] = raw_data[x-1,8]*(1+ raw_data[x,9])
}
##Graph of re-scaled population (with title)

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

#Exporting graph
ggsave("population_con_titulo.png", width=10, height =7 , units= c("cm"), dpi=500)

#Graph (without title)
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

#Exporting graph
ggsave("population_sin_titulo.png", width=10, height =7 , units= c("cm"), dpi=500)



#Creating variables as share of GDP per capita

raw_data <- raw_data %>%
  mutate(
    log_gdp_pc = log(gdp/pop),
    gov_gdp = (gov_con + pub_inv)/gdp,
    gov_con_gdp = gov_con/gdp,
    pub_inv_gdp= pub_inv/gdp,
    priv_inv_gdp = priv_inv/gdp,
    tr_op = (x+m)/gdp,
  )

#create growth gdp per capita

raw_data <- raw_data %>%
  mutate(
    growth_gdp_pc = log_gdp_pc/lag(log_gdp_pc)-1
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


raw_plot1 <- ggplot(df, aes(x = date, y = log_gdp_pc)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot2 <- ggplot(df, aes(x = date, y = gov_gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot3 <- ggplot(df, aes(x = date, y = gov_con_gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot4 <- ggplot(df, aes(x = date, y = pub_inv_gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot5 <- ggplot(df, aes(x = date, y = priv_inv_gdp)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot6 <- ggplot(df, aes(x = date, y = tr_op)) +
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
seasonal_adj <- final(seasonal_adj)

#ggplot_graph

df_seas <- as.data.frame(seasonal_adj)
df_seas <- cbind(df$date, df_seas[,])
df_seas <- df_seas %>%
  rename( 
    "date" = "df$date",
  )

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
  labs(x="",y="",title = "Consumo de Gobierno", 
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
variables <- df_seas[,2:7] #Crea un dataframe con variables desestacionalizadas y en logaritmos
u_root <- data.frame(matrix(NA, nrow = 6, ncol = 12)) #Crear un dataframe vacio que sera la tabla de salidas para el analisis de raices unitarias


for (i in 1:ncol(variables)) { #Es un loop para realizar el test PP a cada variable guardada en save
     col <- variables[,i]
     u_root[i,1] <-  adfTest(col, type = c("nc"))@test$p.value
     u_root[i,2] <-  adfTest(col, type = c("c"))@test$p.value
     u_root[i,3] <-  adfTest(col, type = c("ct"))@test$p.value
     u_root[i,4] <-  adfTest(diff(col), type = c("nc"))@test$p.value
     u_root[i,5] <-  adfTest(diff(col), type = c("c"))@test$p.value
     u_root[i,6] <-  adfTest(diff(col), type = c("ct"))@test$p.value
     u_root[i,7] <-  pp.test(col)[1,3]
     u_root[i,8] <-  pp.test(col)[2,3]
     u_root[i,9] <-  pp.test(col)[3,3]
     u_root[i,10] <-  pp.test(diff(col))[1,3]
     u_root[i,11] <-  pp.test(diff(col))[2,3]
     u_root[i,12] <-  pp.test(diff(col))[3,3]
}

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
\\headrow & \\multicolumn{6}{c}{ADF} &
            \\multicolumn{6}{c}{PP}\\\\
  \\midrule
\\headrow & \\multicolumn{3}{c}{Variable en nivel} &
            \\multicolumn{3}{c}{Variable en diferencias} &
            \\multicolumn{3}{c}{Variable en nivel} &
            \\multicolumn{3}{c}{Variable en diferencias}\\\\
  \\midrule
\\headrow Serie trimestral &
 \\multicolumn{1}{c}{N.} &
  \\multicolumn{1}{c}{I.} &
  \\multicolumn{1}{c}{I. y T.} &
  \\multicolumn{1}{c}{N.} &
  \\multicolumn{1}{c}{I.} &
  \\multicolumn{1}{c}{I. y T.}&
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

#########  Ecuacion de largo plazo Gasto Agregado (Tony Stark)

#########                                                 #########






#########                                                 #########

######### Ecuacion de largo plazo - Var. dep.: Inversion Fija (Euler) 

#########                                               #########  

#inclusion of dummies to the variables object

df_modelo1 <- as.data.frame(c(df_seas[,2:3],df_seas[,6:7]))
colnames(df_modelo1)<- c("gdp_pc_s", "gov_gdp_s", "priv_inv_gdp_s","tr_op_s")                        

df_modelo2 <- as.data.frame(cbind.data.frame(df_seas[,2],df_seas[,5:7]))
colnames(df_modelo2)<- c("gdp_pc_s", "pub_inv_gdp_s", "priv_inv_gdp_s","tr_op_s")     




df_modelo1 <- data.frame(df_modelo1, df[,17:18])

df_modelo2 <- data.frame(df_modelo2, df[,17:18]) 

### Exporting data for Eviews 
path<-getwd()

#exporting the raw data
write_xlsx(raw_data, paste(path,"raw_data.xlsx", sep="/"))
write.csv(raw_data, paste(path,"raw_data.csv", sep="/"))

#exporting the seasonal adjusted datra¿¿a
write_xlsx(df_seas, paste(path,"df_seas.xlsx", sep="/"))
write.csv(df_seas, paste(path,"df_seas.csv", sep="/"))


#########                                                 #########

#########  Prueba de cointegracion de Johansen (Euler)

#########                                                 #########

#importing table from eviews


johansen_results <- read.csv(paste(getwd(), "dofiles/tabla_johansen.csv", sep="/"),header=FALSE)
johansen_results <- johansen_results %>%
mutate(across(where(is.numeric), round, digits=2))
col1_johansen<-c(
  "Gasto Agregado",
  "",
  "Inversión fija pub",
  ""
)
col2_johansen<-c(
  "Traza",
  "Valor propio",
  "Traza",
  "Valor propio"
)

johansen_results$col1 <- col1_johansen
johansen_results$col2 <- col2_johansen
johansen_results<- johansen_results %>% relocate(col2)
johansen_results <-johansen_results %>% relocate(col1)

#Latex code
addtorow_johansen <- list()
addtorow_models_pub_inv$pos <- list(0)
addtorow_models_pub_inv$command <- c("\\hline
\\multicolumn{13}{c}{Variable dependiente: Logaritmo del PIB per cápita}                                                                                                                                                                                                                                        \\\ \\hline
                                                       &      & \\multicolumn{3}{c}{OLS}                      &                          & \\multicolumn{3}{c}{FMOLS}                         &                                                 & \\multicolumn{3}{c}{CCR}                        \\\ \\cline{3-5} \\cline{7-9} \\cline{11-13} 
\\multirow{-2}{*}{Variables}                            &      & Lineal      & Cuadrática     & Cúbica        & \\multicolumn{1}{c}{}     & Lineal          & Cuadrática     & Cúbica         & \\multicolumn{1}{c}{}                            & Lineal       & Cuadrática     & Cúbica         \\\ \\hline\\hline
")
print(xtable(models_pub_inv), add.to.row = addtorow_models_pub_inv , include.rownames = FALSE, include.colnames = FALSE )



######### Estimates of Long Run equations ##############


#All estimates were performed in Eviews the program is "DF_SEAS_GOV_SIZE.prg", however the table that summirizes all models is imported here to generate the Latex Table
models_pub_inv <- data.frame(matrix(NA, nrow = 20, ncol = 10))

coef_pval_pub_inv <- read.csv(paste(getwd(), "tabla_modelos_inv.csv", sep="/"), header = FALSE)#reading table with coefficients and p-values
coef_pval_pub_inv <- coef_pval_pub_inv %>% 
  
mutate(across(where(is.numeric), round, digits=2))#rounding

col<-c(
       "$c$",
       "",
       "$INV$",
       "",
       "$AC$",
       "",
       "$D_{2008}",
       "",
       "D_{2018}",
       "",
       "$GOB$",
       "",
       "$GOB^2",
       "",
       "$GOB^3",
       "",
       "$R^2$ ajustado",
       "Estadístico JB",
       "Prueba BGLM",
       "Prueba BPG")
models_pub_inv$col <- col
models_pub_inv <- models_pub_inv %>% relocate(col)
models_pub_inv <- cbind(col, coef_pval_pub_inv)

#generating latex code
addtorow_models_pub_inv <- list()
addtorow_models_pub_inv$pos <- list(0)
addtorow_models_pub_inv$command <- c("\\hline
\\multicolumn{13}{c}{Variable dependiente: Logaritmo del PIB per cápita}                                                                                                                                                                                                                                        \\\ \\hline
                                                       &      & \\multicolumn{3}{c}{OLS}                      &                          & \\multicolumn{3}{c}{FMOLS}                         &                                                 & \\multicolumn{3}{c}{CCR}                        \\\ \\cline{3-5} \\cline{7-9} \\cline{11-13} 
\\multirow{-2}{*}{Variables}                            &      & Lineal      & Cuadrática     & Cúbica        & \\multicolumn{1}{c}{}     & Lineal          & Cuadrática     & Cúbica         & \\multicolumn{1}{c}{}                            & Lineal       & Cuadrática     & Cúbica         \\\ \\hline\\hline
")
print(xtable(models_pub_inv), add.to.row = addtorow_models_pub_inv , include.rownames = FALSE, include.colnames = FALSE )

print(xtable(models_pub_inv), add.to.row = addtorow_models_pub_inv, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = function(x){x} )


#Bootstrap (Together but Tony Stark leading)


#The bootstrap exercise was performed in Eviews in the program is "DF_SEAS_GOV_SIZE.prg"







