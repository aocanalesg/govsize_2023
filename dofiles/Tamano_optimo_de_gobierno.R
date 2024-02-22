#### Investigacion: Tamano optimo de Gobierno en el crecimiento Economico: El caso de Nicaragua
#### Premio de Economía y Finanzas 2023 - Banco Central de Nicaragua
# CLEAR environment
rm(list = ls())
# Definición de directorio de trabajo
setwd('C:/Users/Axel Canales/Downloads/govsize_2023 (2)/govsize_2023')
path <- getwd()


# Instalación de paquetes necesarios (no correr de tenerlos ya instalados) ---- 


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

# Cargar paquetes ----
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
library(ggpubr)
library(aTSA) #pptest
library(broom)
library(dplyr)
library(stargazer)
library(vars)
library(readxl)

# Importar data ----

raw_data <- read_xlsx(paste(path, "data.xlsx", sep = "/"),
                      sheet = "RAW_DATA3",
                      col_names = TRUE,
                      range = "A1:I69")


# Procesacimiento de data----

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
    "pop" = "RAW_POP",
    "gdp_nom" = "RAW_GDP_NOM"
  )

#Declare vector of names 
var_names_bcn <- c("date", "gdp", "gov_con", "pub_inv", "priv_inv", "x", "m", "pop","gdp_nom")

#Rescale variables from BCN to millions of cords
raw_data <- raw_data %>%
  mutate(
    gdp = gdp*10^6,
    gov_con =gov_con*10^6,
    pub_inv= pub_inv*10^6,
    priv_inv=priv_inv*10^6,
    x=x*10^6,
    m=m*10^6, 
    gdp_nom=gdp_nom*10^6
         )

# Grafica de la serie poblacion ----
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
 raw_data[x,10] = (raw_data[x-1,10] +raw_data[x-2,10] +raw_data[x-3,10] +raw_data[x-4,10])/4
}

for (x in 28:1) {
  raw_data[x,10] = (raw_data[x+1,10] +raw_data[x+2,10] +raw_data[x+3,10] +raw_data[x+4,10])/4
}

for (x in 26:1) { 
  raw_data[x, 8] = raw_data[x+1, 8]/(1+raw_data[x+1,10])
}

for (x in 50:68) {
  raw_data[x,8] = raw_data[x-1,8]*(1+ raw_data[x,10])
}
# Serie de población re-escalada ----

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

# Grafico poblacion (without title) ----
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



# Creating variables as share of GDP per capita ----

raw_data <- raw_data %>%
  mutate(
    log_gdp_pc = log(gdp/pop),
    gov_gdp = (gov_con + pub_inv)/gdp_nom,
    gov_con_gdp = gov_con/gdp_nom,
    pub_inv_gdp= pub_inv/gdp_nom,
    priv_inv_gdp = priv_inv/gdp_nom,
    tr_op = (x+m)/gdp_nom,
  )

#create growth gdp per capita

raw_data <- raw_data %>%
  mutate(
    growth_gdp_pc = log_gdp_pc/lag(log_gdp_pc)-1
  )

# create dummies ----
raw_data <- raw_data %>%
  mutate(
    d_2008 = ifelse(date >= "2008-7-1" & date <= "2009-4-1" ,1,0)
  )

raw_data <- raw_data %>%
  mutate(
    d_2018 = ifelse(date >= "2017-10-1",1,0)
  )


# Time series set and raw data plot ----
ts_vars <- ts(data = raw_data[,11:ncol(raw_data)],
             start = c(2006,1),
             frequency = 4
)

#creating variable for date
df <- as.data.frame(raw_data)
df$date<-as.Date(df$date,  "%m/%d/%y")


raw_plot1 <- ggplot(df, aes(x = date, y = log_gdp_pc)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y")

raw_plot2 <- ggplot(df, aes(x = date, y = x)) +
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


# Desestacionalizacion y Figura 2 ----

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
        plot.title = element_text(color = "black", size = 12, face = "bold",family="serif"),
        plot.subtitle = element_text(color = "black", size = 10, face = "italic",family="serif"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Producto Interno Bruto Per Capita")+
  labs(x="",y="",title = "Producto Interno Bruto per cápita", 
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
        plot.title = element_text(color = "black", size = 12, face = "bold",family="serif"),
        plot.subtitle = element_text(color = "black", size = 10, face = "italic",family="serif"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(x="",y="",title = "Gasto de Goberno agregado", 
       subtitle = "Porcentaje respecto al PIB total", )

seas_plot3 <- ggplot(df_seas, aes(x = date, y = df_seas[,4])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 12, face = "bold",family="serif"),
        plot.subtitle = element_text(color = "black", size = 10, face = "italic",family="serif"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Producto Interno Bruto Per Capita")+
  labs(x="",y="",title = "Gasto de Gobierno corriente", 
       subtitle = "Porcentaje respecto al PIB total", )

seas_plot4 <- ggplot(df_seas, aes(x = date, y = df_seas[,5])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 12, face = "bold",family="serif"),
        plot.subtitle = element_text(color = "black", size = 10, face = "italic",family="serif"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Producto Interno Bruto Per Capita")+
  labs(x="",y="",title = "Inversión fija pública", 
       subtitle = "Porcentaje respecto al PIB total", )

seas_plot5 <- ggplot(df_seas, aes(x = date, y = df_seas[,6])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 12, face = "bold",family="serif"),
        plot.subtitle = element_text(color = "black", size = 10, face = "italic",family="serif"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Inversion Fija Privada")+
  labs(x="",y="",title = "Inversión fija privada", 
       subtitle = "Porcentaje respecto al PIB total", )

seas_plot6 <- ggplot(df_seas, aes(x = date, y = df_seas[,7])) +
  geom_line() +
  geom_smooth(method="lm", se=FALSE,  linetype = "dashed")+
  scale_x_date(date_breaks = "years" , date_labels = "%Y")+
  theme_classic()+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", size = 12, face = "bold",family="serif"),
        plot.subtitle = element_text(color = "black", size = 10, face = "italic",family="serif"),
        plot.caption.position = "plot",
        axis.text.x = element_text(angle=90, hjust = 1)
  )+
  #ggtitle("Apertura Comercial")+
  labs(x="",y="",title = "Apertura comercial", 
       subtitle = "Porcentaje respecto al PIB total", )

combined_plot_seas <- ggarrange(seas_plot1,
                                seas_plot2,
                                seas_plot3,
                                seas_plot4,
                                seas_plot5,
                                seas_plot6,
                           nrow = 2,
                           ncol = 3) #nrow & ncol depend on how you want to #organize your plots

combined_plot_seas
#annotate_figure(combined_plot_seas, bottom=text_grob("Fuente: Elaboración propia con base en datos dele BCN", hjust=0, x=0, family="serif", size=16), fig.lab.pos = "bottom.left")
ggsave("variables_sin_titulo.png", width=24, height =16 , units= c("cm"), dpi=500)

# Figura 3 (scatterplots) ----



seas_plot7 <- ggplot(df_seas, aes(x =df_seas[,3])) +
  geom_point(aes (y=df_seas[,2]), shape=16)+
  geom_smooth(aes(y=df_seas[,2]), method="lm", formula = y ~ x + I(x^2), se=FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(family="serif", size=14, hjust = 0),
        axis.title.x = element_text(size=16, family="serif"),
        axis.title.y= element_text(size=16, family="serif"),
        axis.text.x = element_text(size=14, family="serif"),
        axis.text.y = element_text(size=14, family="serif"),
  #      plot.caption = element_text(hjust = 0),
  #      plot.title.position = "plot",
  #      plot.title = element_text(color = "black", size = 10, face = "bold"),
  #      plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
  #      plot.caption.position = "plot",
  #      axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(y="Logaritmo del PIB per cápita",
       x="Gasto de Gobierno agregado (porcentaje del PIB)"
 #      caption = "Fuente: Elaboración propia"
       )
seas_plot7
ggsave("gdp_vs_aggregate_exp.png", width=18, height =12 , units= c("cm"), dpi=500)


seas_plot8 <- ggplot(df_seas, aes(x =df_seas[,5])) +
  geom_point(aes (y=df_seas[,2]), shape=16)+
  geom_smooth(aes(y=df_seas[,2]), method="lm", formula = y ~ x + I(x^2), se=FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(family="serif", size=14, hjust = 0),
        axis.title.x = element_text(size=16, family="serif"),
        axis.title.y= element_text(size=16, family="serif"),
        axis.text.x = element_text(size=14, family="serif"),
        axis.text.y = element_text(size=14, family="serif"),
        #      plot.caption = element_text(hjust = 0),
        #      plot.title.position = "plot",
        #      plot.title = element_text(color = "black", size = 10, face = "bold"),
        #      plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        #      plot.caption.position = "plot",
        #      axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(y="Logaritmo del PIB per cápita",
       x="Inversión pública (porcentaje del PIB)"
     #  caption = "Fuente: Elaboración propia"
     )
seas_plot8 
ggsave("gdp_vs_public_inv.png", width=18, height =12, units= c("cm"), dpi=500)



seas_plot9 <- ggplot(df_seas, aes(x =df_seas[,4])) +
  geom_point(aes (y=df_seas[,2]), shape=16)+
  geom_smooth(aes(y=df_seas[,2]), method="lm", formula = y ~ x + I(x^2), se=FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(family="serif", size=14, hjust = 0),
        axis.title.x = element_text(size=16, family="serif"),
        axis.title.y= element_text(size=16, family="serif"),
        axis.text.x = element_text(size=14, family="serif"),
        axis.text.y = element_text(size=14, family="serif"),
        #      plot.caption = element_text(hjust = 0),
        #      plot.title.position = "plot",
        #      plot.title = element_text(color = "black", size = 10, face = "bold"),
        #      plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        #      plot.caption.position = "plot",
        #      axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(y="Logaritmo del PIB per cápita",
       x="Gasto de Gobierno corriente (porcentaje del PIB)"
 #      caption = "Fuente: Elaboración propia"
 )
seas_plot9 
ggsave("gdp_vs_gov_con_gdp.png", width=18, height =12, units= c("cm"), dpi=500)


#No Caption

nc_seas_plot7 <- ggplot(df_seas, aes(x =df_seas[,3])) +
  geom_point(aes (y=df_seas[,2]), shape=16)+
  geom_smooth(aes(y=df_seas[,2]), method="lm", formula = y ~ x + I(x^2), se=FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(family="serif", size=14, hjust = 0),
        axis.title.x = element_text(size=14, family="serif"),
        axis.title.y= element_text(size=14, family="serif"),
        axis.text.x = element_text(size=11, family="serif"),
        #      plot.caption = element_text(hjust = 0),
        #      plot.title.position = "plot",
        #      plot.title = element_text(color = "black", size = 10, face = "bold"),
        #      plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        #      plot.caption.position = "plot",
        #      axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(y="Logaritmo del PIB per cápita",
       x="Gasto de Gobierno agregado (porcentaje del PIB)",
      )
nc_seas_plot7
ggsave("nc_seas_plot7.png", width=18, height =12 , units= c("cm"), dpi=500)


nc_seas_plot8 <- ggplot(df_seas, aes(x =df_seas[,5])) +
  geom_point(aes (y=df_seas[,2]), shape=16)+
  geom_smooth(aes(y=df_seas[,2]), method="lm", formula = y ~ x + I(x^2), se=FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(family="serif", size=14, hjust = 0),
        axis.title.x = element_text(size=14, family="serif"),
        axis.title.y= element_text(size=14, family="serif"),
        axis.text.x = element_text(size=11, family="serif"),
        axis.text.y = element_text(size=11, family="serif")
        #      plot.caption = element_text(hjust = 0),
        #      plot.title.position = "plot",
        #      plot.title = element_text(color = "black", size = 10, face = "bold"),
        #      plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        #      plot.caption.position = "plot",
        #      axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(y="Logaritmo del PIB per cápita",
       x="Inversión pública (porcentaje del PIB)",
    )
nc_seas_plot8
ggsave("nc_seas_plot8.png", width=18, height =12, units= c("cm"), dpi=500)



nc_seas_plot9 <- ggplot(df_seas, aes(x =df_seas[,4])) +
  geom_point(aes (y=df_seas[,2]), shape=16)+
  geom_smooth(aes(y=df_seas[,2]), method="lm", formula = y ~ x + I(x^2), se=FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(family="serif", size=14, hjust = 0),
        axis.title.x = element_text(size=14, family="serif"),
        axis.title.y= element_text(size=14, family="serif"),
        axis.text.x = element_text(size=11, family="serif"),
        axis.text.y = element_text(size=11, family="serif")
        #      plot.caption = element_text(hjust = 0),
        #      plot.title.position = "plot",
        #      plot.title = element_text(color = "black", size = 10, face = "bold"),
        #      plot.subtitle = element_text(color = "black", size = 7, face = "italic"),
        #      plot.caption.position = "plot",
        #      axis.text.x = element_text(angle=90, hjust = 1)
  )+
  labs(y="Logaritmo del PIB per cápita",
       x="Gasto de Gobierno corriente (porcentaje del PIB)",
   )
nc_seas_plot9
ggsave("nc_seas_plot9.png", width=18, height =12, units= c("cm"), dpi=500)




combined_scatterplot <- ggarrange(seas_plot7,
                                seas_plot8,
                                seas_plot9,
                                nrow = 1,
                                ncol = 3) #nrow & ncol depend on how you want to #organize your plots

combined_scatterplot
annotate_figure(combined_scatterplot, bottom=text_grob("Fuente: Elaboración propia", hjust=0, x=0, family="serif", size=13), fig.lab.pos = "bottom.left")
ggsave("scatterplot_combined.png", width=40, height =20 , units= c("cm"), dpi=500)





# Replicacion - CUADRO 1. Tabla estadisticos descriptivos ----

cuadro_1<-df_seas[,2:7]
cuadro_1<- cuadro_1 %>%
  mutate(
    log_gdp_pc = exp(log_gdp_pc),
   gov_gdp = gov_gdp*100,
    gov_con_gdp =  gov_con_gdp*100,
    pub_inv_gdp= pub_inv_gdp*100,
   priv_inv_gdp= priv_inv_gdp*100,
    tr_op = tr_op
  )

colnames(cuadro_1)<-c("PIB per capita",
                                  "Gasto de Gobierno agregado",
                                  "Gasto de Gobierno corriente",
                                  "Inversión fija pública",
                                  "Inversión fija privada",
                                  "Apertura comercial"
)

cuadro_1 <- stargazer(cuadro_1, type='latex', digits=2)
stargazer(cuadro_1, type='text', digits=2)

                                                        
# Tabla 2.Test de Raiz Unitaria ADF ----                  
                                                      
#          Variables en log-niveles                     

variables <- df_seas[,2:7] #Crea un dataframe con variables desestacionalizadas y en logaritmos
cuadro_2 <- data.frame(matrix(NA, nrow = 6, ncol = 12)) #Crear un dataframe vacio que sera la tabla de salidas para el analisis de raices unitarias


for (i in 1:ncol(variables)) { #Es un loop para realizar el test PP a cada variable guardada en save
     col <- variables[,i]
     cuadro_2[i,1] <-  adfTest(col, type = c("nc"))@test$p.value
     cuadro_2[i,2] <-  adfTest(col, type = c("c"))@test$p.value
     cuadro_2[i,3] <-  adfTest(col, type = c("ct"))@test$p.value
     cuadro_2[i,4] <-  adfTest(diff(col), type = c("nc"))@test$p.value
     cuadro_2[i,5] <-  adfTest(diff(col), type = c("c"))@test$p.value
     cuadro_2[i,6] <-  adfTest(diff(col), type = c("ct"))@test$p.value
     cuadro_2[i,7] <-  pp.test(col)[1,3]
     cuadro_2[i,8] <-  pp.test(col)[2,3]
     cuadro_2[i,9] <-  pp.test(col)[3,3]
     cuadro_2[i,10] <-  pp.test(diff(col))[1,3]
     cuadro_2[i,11] <-  pp.test(diff(col))[2,3]
     cuadro_2[i,12] <-  pp.test(diff(col))[3,3]
}

series = c( "PIB per ́capita","Gasto de Gobierno Agregado","Gasto de Gobierno Corriente","Inversion fija Publica","Inversion Fija Privada","Apertura Comercial")
cuadro_2$series = series
cuadro_2 <- cuadro_2 %>% relocate(series)
# Redondeando los valores en la tabla
cuadro_2 <- cuadro_2 %>% 
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
 print(xtable(cuadro_2), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )
 
                                                         
# Tabla 3.Criterio de seleccion de rezagos Gasto Agregado ----
                                                         

 var_gob <-  variables[,1:2]
 lag_selection_gov <- VARselect(var_gob, lag.max = 5, type = c("const", "trend", "both", "none"),
                                                 season = NULL, exogen = NULL)
 cuadro_3 <- as.data.frame(VARselect(var_gob, lag.max = 5, type = c("const", "trend", "both", "none"),
           season = NULL, exogen = NULL)[[2]])
 Criterio = c("AIC","HQ","SC","FPE")
 cuadro_3$Criterio = Criterio
 cuadro_3 <-cuadro_3 %>% relocate(Criterio)
 # Redondeando los valores en la tabla
 cuadro_3 <- cuadro_3 %>% 
   mutate(across(where(is.numeric), round, digits=2))
 
 addtorow <- list()
 addtorow$pos <- list(0)
 addtorow$command <- c(" \\toprule
\\headrow & \\multicolumn{5}{c}{Criterio de seleccion de rezagos} \\\\
  \\midrule &
  \\multicolumn{5}{c}{Numero de rezagos}
    \\midrule
\\headrow Criterio &
 \\multicolumn{1}{c}{1} &
  \\multicolumn{1}{c}{2} &
  \\multicolumn{1}{c}{3} &
  \\multicolumn{1}{c}{4} &
  \\multicolumn{1}{c}{5} \\\\
  \\bottomrule")
 print(xtable(cuadro_3), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )
 
                                                                
# Tabla 4.Criterio de seleccion de rezagos Inversion Fija Publica ############
                                                                  

 var_inv <- variables %>%  select(log_gdp_pc, pub_inv_gdp)
 cuadro_4<- VARselect(var_inv, lag.max = 5, type = c("const", "trend", "both", "none"),
                                season = NULL, exogen = NULL)
 df_cuadro_4<- as.data.frame(VARselect(var_inv, lag.max = 5, type = c("const", "trend", "both", "none"),
                                                 season = NULL, exogen = NULL)[[2]])
 Criterio = c("AIC","HQ","SC","FPE")
 df_cuadro_4$Criterio = Criterio
 df_cuadro_4<-df_cuadro_4%>% relocate(Criterio)
 # Redondeando los valores en la tabla
 df_cuadro_4<- df_cuadro_4%>% 
   mutate(across(where(is.numeric), round, digits=2))
 
 addtorow <- list()
 addtorow$pos <- list(0)
 addtorow$command <- c(" \\toprule
\\headrow & 
  \\multicolumn{5}{c}{Numero de rezagos}
    \\midrule
\\headrow Criterio &
 \\multicolumn{1}{c}{1} &
  \\multicolumn{1}{c}{2} &
  \\multicolumn{1}{c}{3} &
  \\multicolumn{1}{c}{4} &
  \\multicolumn{1}{c}{5} \\\\
  \\bottomrule")
 print(xtable(df_cuadro_4), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )


# Tabla 5.Prueba de Precedencia temporal            ############

 #Prueba de precedencia temporal De Gasto Gobierno Agregado 
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
 
cuadro_5 <- data.frame(matrix(NA, nrow = 4, ncol = 5))
cuadro_5[1,1] <- c("Gasto publico agregado no causa a PIB per capita")
cuadro_5[2,1] <- c("PIB per capita no causa a Gasto publico agregado")
cuadro_5[3,1] <- c("Inversion fija publica no causa a PIB per capita")
cuadro_5[4,1] <- c("PIB per capita no causa a Inversion fija publica")
 ###Llenado del cuadro de test de procedencia de Granger
 for (i in 1:4) { 
  cuadro_5[1,i+1] <- granger_gov_pib[[i]][2,4]
  cuadro_5[2,i+1] <- granger_pib_gov[[i]][2,4]
  cuadro_5[3,i+1] <- granger_inv_pib[[i]][2,4]
  cuadro_5[4,i+1] <- granger_pib_inv[[i]][2,4]
 }
# Creacion de encabezados para exportar cuadro a latex ----
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
 print(xtable(cuadro_5), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )

# Exporttando data para Eviews ###########


#exporting the raw data
write_xlsx(raw_data, paste(path,"raw_data.xlsx", sep="/"))
write.csv(raw_data, paste(path,"raw_data.csv", sep="/"))

#exporting the seasonal adjusted data
write_xlsx(df_seas, paste(path,"df_seas.xlsx", sep="/"))
write.csv(df_seas, paste(path,"df_seas.csv", sep="/"))


# Tabla 6.Prueba de cointegracion de Johansen ----



### ADVERTENCIA: Antes de correr las siguientes lineas se debe correr el archivo de eviews: "C:\Users\Axel Canales\Desktop\govsize_2023\dofiles\df_seas_gov_size.prg"


#importing table from eviews

cuadro_6 <- read.csv(paste(getwd(), "dofiles/tabla_johansen.csv", sep="/"),header=FALSE)
cuadro_6 <-cuadro_6 %>%
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

cuadro_6$col1 <- col1_johansen
cuadro_6$col2 <- col2_johansen
cuadro_6 <-cuadro_6 %>% relocate(col2)
cuadro_6 <-cuadro_6 %>% relocate(col1)

#Latex code
addtorow_johansen <- list()
addtorow_johansen$pos <- list(0)
addtorow_johansen$command <- c("
Variable & Tipo de test & \\multicolumn{2}{c}{Ninguna}& \\multicolumn{2}{c}{Lineal}&\\multicolumn{1}{c}{Cuadrática} \\\\
\\cline{3-7}
& & Sin Intercepto & Intercepto & Intercepto &Intercepto & Intercepto \\\\
&&Sin tendencia & Sin tendencia & Sin tendencia & Tendencia & Tendencia \\\\
\\hline 
")
print(xtable(cuadro_6, caption="Prueba de cointegración de Johansen", label="tab:cointegracion"), add.to.row = addtorow_johansen , include.rownames = FALSE, include.colnames = FALSE,caption.placement = "top" )

# Tabla 7. Estimacion de largo plazo de gobierno agregado  #########


#All estimates were performed in Eviews the program is "DF_SEAS_GOV_SIZE.prg", however the table that summirizes all models is imported here to generate the Latex Table
cuadro_7 <- data.frame(matrix(NA, nrow = 18, ncol = 10))

coef_pval_ag <- read.csv(paste(getwd(), "dofiles/tabla_modelos_agregado.csv", sep="/"), header = FALSE)#reading table with coefficients and p-values
coef_pval_ag <- coef_pval_ag %>% 
mutate(across(where(is.numeric), round, digits=2))#rounding

col<-c(
       "$c$",
       "",
       "$D_{2008}$",
       "",
       "$D_{2018}$",
       "",
       "$INV$",
       "",
       "$AC$",
       "",
       "$GOB$",
       "",
       "$GOB^2$",
       "",
       "$GOB^3$",
       "",
       "$R^2$ ajustado",
       "Estadístico JB")
cuadro_7$col <- col
cuadro_7 <- cuadro_7 %>% relocate(col)
cuadro_7 <- cbind(col, coef_pval_ag)

#generating latex code
addtorow_cuadro_7 <- list()
addtorow_cuadro_7$pos <- list(0)
addtorow_cuadro_7$command <- c("\\hline
\\multicolumn{10}{c}{Variable dependiente: Logaritmo del PIB per cápita}\\\\     \\hline
                                                       &      \\multicolumn{3}{c}{OLS}    &     \\multicolumn{3}{c}{FMOLS}    & \\multicolumn{3}{c}{CCR}                        \\\\ \\cline{2-10} 
Variables                           &  Lineal      & Cuadrática     & Cúbica            & Lineal          & Cuadrática     & Cúbica              & Lineal       & Cuadrática     & Cúbica         \\\\
")

print(xtable(cuadro_7), add.to.row = addtorow_cuadro_7, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = function(x){x} )


# Tabla 8. Estimacion de largo plazo de Inversion Fija Publica  #########

#All estimates were performed in Eviews the program is "DF_SEAS_GOV_SIZE.prg", however the table that summirizes all models is imported here to generate the Latex Table
cuadro_8 <- data.frame(matrix(NA, nrow = 18, ncol = 10))

coef_pval_inv <- read.csv(paste(getwd(), "/dofiles/tabla_modelos_inv.csv", sep="/"), header = FALSE)#reading table with coefficients and p-values
coef_pval_inv <- coef_pval_inv %>% 
  mutate(across(where(is.numeric), round, digits=2))#rounding

col<-c(
  "$c$",
  "",
  "$D_{2008}$",
  "",
  "$D_{2018}$",
  "",
  "$INV$",
  "",
  "$AC$",
  "",
  "$INV_PUB$",
  "",
  "$INV_PUB^2$",
  "",
  "$INV_PUB^3$",
  "",
  "$R^2$ ajustado",
  "Estadístico JB")
cuadro_8$col <- col
cuadro_8 <- cuadro_8 %>% relocate(col)
cuadro_8 <- cbind(col, coef_pval_inv)

#generating latex code
addtorow_cuadro_8 <- list()
addtorow_cuadro_8$pos <- list(0)
addtorow_cuadro_8$command <- c("\\hline
\\multicolumn{10}{c}{Variable dependiente: Logaritmo del PIB per cápita}\\\\     \\hline
                                                       &      \\multicolumn{3}{c}{OLS}    &     \\multicolumn{3}{c}{FMOLS}    & \\multicolumn{3}{c}{CCR}                        \\\\ \\cline{2-10} 
Variables                           &  Lineal      & Cuadrática     & Cúbica            & Lineal          & Cuadrática     & Cúbica              & Lineal       & Cuadrática     & Cúbica         \\\\
")

print(xtable(cuadro_8), add.to.row = addtorow_cuadro_8, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = function(x){x} )



# Tabla 9. Estimacion de tamano optimo                     #########
cuadro9_tamano_optimo <- read.csv(paste(getwd(), "dofiles/cuadro8.csv", sep="/"),header=FALSE)
cuadro9_tamano_optimo[1,1] <- "OLS"
cuadro9_tamano_optimo[2,1] <- "FMOLS"
cuadro9_tamano_optimo[3,1] <- "CCR"
cuadro9_tamano_optimo[1:3,2:3] <- "Si"
cuadro9_tamano_optimo$V6 <- c("",round(100*mean(raw_data$gov_gdp),2),"")
cuadro9_tamano_optimo$V7 <- c("",round(100*mean(raw_data$pub_inv_gdp),2),"")
cuadro9_tamano_optimo <- cuadro9_tamano_optimo %>%
  mutate(across(where(is.numeric), round, digits=2))
cuadro9_tamano_optimo$V4 <- paste(cuadro9_tamano_optimo$V4, "%", sep = "")
cuadro9_tamano_optimo$V5 <- paste(cuadro9_tamano_optimo$V5, "%", sep = "")
cuadro9_tamano_optimo[2,6:7] <- paste(cuadro9_tamano_optimo[2,6:7], "%", sep = "")

# Cuadro 9 latex code 

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- c(" \\toprule
\\headrow   \\multicolumn{1}{c}{}  &
            \\multicolumn{2}{c}{\begin{tabular}[c]{@{}c@{}}¿Es valida la \\ curva de Armey? \\end{tabular}} &
            \\multicolumn{2}{c}{Nivel óptimo de Gasto (\\% del PIB)}&
             \\multicolumn{2}{c}{Nivel Efectivo de Gasto}\\\\
  \\midrule
\\headrow  
           \\multicolumn{1}{c}{Metodologia} &
            \\multicolumn{1}{c}{Gasto de Gobierno Agregado} &
            \\multicolumn{1}{c}{Inversion Fija Publica} &
            \\multicolumn{1}{c}{Gasto de Gobierno Agregado} &
            \\multicolumn{1}{c}{Inversion Fija Publica} &
            \\multicolumn{1}{c}{Gasto de Gobierno Agregado} &
            \\multicolumn{1}{c}{Inversion Fija Publica}\\\\
  \\bottomrule")
print(xtable(cuadro9_tamano_optimo), add.to.row = addtorow, include.rownames = FALSE, include.colnames = FALSE )

# Tabla 10. Bootstrap Tamano de Gobierno                   #########

#The bootstrap exercise was performed in Eviews in the program is "DF_SEAS_GOV_SIZE.prg"

bootstrao_lin_ag <- read.csv(paste(getwd(), "dofiles/lineales_agregado.csv", sep="/"),header=FALSE)
bootstrao_cuad_ag<- read.csv(paste(getwd(), "dofiles/cuadraticos_agregado.csv", sep="/"),header=FALSE)
bootstrao_lin_inv<-read.csv(paste(getwd(), "dofiles/lineales_inversion.csv", sep="/"),header=FALSE)
bootstrao_cuad_inv<-read.csv(paste(getwd(), "dofiles/cuadraticos_inversion.csv", sep="/"),header=FALSE)

cuadro_10<-as.data.frame(cbind(bootstrao_lin_ag,bootstrao_cuad_ag,bootstrao_lin_inv,bootstrao_cuad_inv ))
colnames(cuadro_10)<-c("lin_ag", "cuad_ag", "lin_inv", "cuad_inv")

cuadro_10 <- cuadro_10 %>%
  mutate(
    ag_op = -lin_ag/(2*cuad_ag),
    inv_op = -lin_inv/(2*cuad_inv),
    
  )

# Apendice Histogramas de estimaciones de tamano optimo obtenido mediante bootstrap #######
hist_ag_op<-ggplot(cuadro_10, aes(x=ag_op))+
  geom_histogram(fill="gray", color="black",bins=10)+
  geom_vline(aes(xintercept=mean(ag_op)), color="blue",
             linetype="dashed")+
  theme_classic()+
  theme_bw()+
  theme(plot.caption = element_text(hjust = 0))+
  labs(x="Tamaño óptimo",y="Densidad",caption="Fuente: Elaboración propia" )
hist_ag_op
ggsave("hist_ag_op.png", width=12, height =10, units= c("cm"), dpi=500)

hist_inv_op<-ggplot(cuadro_10, aes(x=inv_op))+
  geom_histogram(fill="gray", color="black",bins=10)+
  geom_vline(aes(xintercept=mean(inv_op)), color="blue",
             linetype="dashed")+
  theme_classic()+
  theme_bw()+
  theme(plot.caption = element_text(hjust = 0))+
  labs(x="Tamaño óptimo",y="Densidad",caption="Fuente: Elaboración propia" )
hist_inv_op
ggsave("hist_inv_op.png", width=12, height =10, units= c("cm"), dpi=500)



# Tabla 10 (Apedice) Bootstrap ----
values<-unname(lapply(cuadro_10[5:6], quantile, na.rm=T,  prob = c(0.05,0.95), names = FALSE))

ag_bootstrap <-values[[1]]
inv_bootstrap <- values[[2]]
#Replication Table Bootstrap

tabla_10<-c(
  "Gasto Público Agregado",
  "Inversión Fija Pública"
)

tabla_10 <- data.frame(tabla_10)

tabla_10$opt_mean <- c(
 paste(round(100*mean(cuadro_10$ag_op),2),"%"),
 paste( round(100*mean(cuadro_10$inv_op),2),"%"))

tabla_10$lim_inf <- c(
  paste(round(100*ag_bootstrap[1],2),"%"),
  paste(round(100*inv_bootstrap[1],2),"%")
)

tabla_10$lim_sup<- c(
  paste(round(100*ag_bootstrap[2],2),"%"),
  paste(round(100*inv_bootstrap[2],2),"%")
)


#generating latex code

addtorow_tabla_10 <- list()
addtorow_tabla_10$pos <- list(0)
addtorow_tabla_10$command <- c(" \\toprule
\\headrow  \\multicolumn{1}{c}{Variable} &
            \\multicolumn{1}{c}{Nivel óptimo de gasto (\\% del PIB)}&
            \\multicolumn{2}{c}{Intervalo de confianza}\\\\
  \\midrule
\\headrow  \\multicolumn{1}{c}{} &
            \\multicolumn{1}{c}{} &
            \\multicolumn{1}{c}{Límite inferior} &
            \\multicolumn{1}{c}{Límite superior}\\\\
  \\bottomrule")
print(xtable(tabla_10), add.to.row = addtorow_tabla_10, include.rownames = FALSE, include.colnames = FALSE, sanitize.text.function = function(x){x})
 

# Tabla 14 Anexo ----
print(xtable(cuadro_10),  include.rownames = FALSE, digits=4)

