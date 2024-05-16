library(openxlsx)
library(dplyr)




dataset <- read.xlsx("~/repositorios_programas/Github_proyectos/predicciones_multiples_series_tiempo/data/rec.xlsx", sheet = "Hoja2",detectDates = T)

dataset <- dataset %>% select(c(fecha_corte, dep.vista, col.cart, exportaciones, mucif_duraderos))

#----codigo que va en power bi ----

library(tsibble)
library(feasts)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(ggthemes)
library(reshape2)
library(fable) #multiples series de tiempo
library(lubridate)


dd <- dataset %>% 
  dplyr::mutate(fecha_corte=tsibble::yearmonth(fecha_corte)) %>% 
  as_tsibble(index = "fecha_corte") %>%
  reshape2::melt("fecha_corte") %>% 
  as_tsibble(index= "fecha_corte", key = "variable")


modelo <- dd %>% model(arima =ARIMA(value))

modelo

f1 <- modelo %>% forecast(h = "1 year")
f1

f1 %>%  autoplot(dd, level = 95, alpha=0.5) + ggthemes::theme_pander()



#----codigo que va en power bi ----

library(tsibble)
library(feasts)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(ggthemes)
library(reshape2)
library(fable) #multiples series de tiempo
library(lubridate)


dd <- dataset %>% 
  dplyr::mutate(fecha_corte=tsibble::yearmonth(fecha_corte)) %>% 
  as_tsibble(index = "fecha_corte") %>%
  reshape2::melt("fecha_corte") %>% 
  as_tsibble(index= "fecha_corte", key = "variable")


modelo2 <- dd %>% model(
  arima = ARIMA(value),
  ets = ETS(value),
  snaive = SNAIVE(value))

f1 <- modelo2 %>% forecast(h = "1 year")
  
f1 %>%  autoplot(dd, level = 95, alpha = 0.5) + ggthemes::theme_pander()


#---minemos un poco el modelo #--------

report(modelo)
modelo

#si quiero llamar un solo modelo

modelo %>% 
  filter(variable == "dep.vista") %>% 
  select(arima) %>% 
  report()

#para saber la desviacion standard hay que dividir los coeficientes, como n es mayor que 30,
#entonces se toma el valor absoluto de los coeficientes

0.8635/0.1203
# [1] 7.177889 mayor que 2, es estadisticamente significativo 

0.0015/0.0007
#[1] 2.142857 igual, mayor que 2


accuracy(modelo)

accuracy(modelo2)