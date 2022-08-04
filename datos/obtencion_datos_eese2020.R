library(tidyverse)
library(openxlsx)
library(haven)

rm(list=ls())




#Cargamos encuesta adultos
campos <- read.xlsx("datos/Adul20.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
adulto2020 <- read_fwf("datos/ADULTO20.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))


# Cargamos encuesta hogar
campos <- read.xlsx("datos/Hogar20.xlsx", colNames = FALSE) %>%
  filter(is.na(X3) == FALSE & X3 != "Longitud") 
nombres <- campos$X1
anchos  <- campos$X3 %>% as.numeric
hogar2020 <- read_fwf("datos/HOGAR20.txt", col_positions = fwf_widths(widths = anchos, col_names = nombres))

#Juntamos adultos con hogar y nos quedamos con la muestra de hogar
dta <- adulto2020 %>%
  left_join(hogar2020, by="IDENTHOGAR")
save(dta, file = "datos/dta.RData")
