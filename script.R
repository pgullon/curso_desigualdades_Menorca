library (tidyverse)


rm(list=ls())



load("datos/dta.RData")


#### LIMPIEZA DE DATOS #####

# Nos vamos a quedar solo con las variables que queremos
# Para esta prueba vamos a calcular las desigualdades en diabetes; por tanto, nos vamos a quedar con: 
# Diabetes
# Sexo
# Edad
# Clase social
# Máximos estudios alcanzados
# Renta en el hogar
# Número de personas en el hgoar


#Diabetes: creamos una variable si alguien ha sido diagnosticado o refiere tener diabetes
# Esta bariable tendrá valor 0 si no se tiene diabetes, y valor 1 si se tiene diabetes
dta <-  dta %>% 
  mutate(diabetes=case_when((G25a_12==1 |G25c_12==1) ~ 1,
                            (G25a_12==2 | G25c_12==2) ~ 0))

# Sexo: la variable se llama SEXOa, 1=Hombre, 2=Mujer

# Edad: la tenemos que calcular porque no la coge como numérica, y descartamos a menores de 18
dta <- dta %>%
  mutate(edad=as.numeric(EDADa))

# Clase social: mandamos a valores perdidos las categorías 8 y 9 
dta <- dta %>%
  mutate(clase=na_if(CLASE_PR.x, "8")) %>%
  mutate(clase=na_if(CLASE_PR.x, "9")) 


# Nivel de estudios: mandamos a valores perdidos las categorías 98 y 99 
dta <- dta %>%
  mutate(estudios=na_if(ESTUDIOS, "98")) %>%
  mutate(estudios=na_if(ESTUDIOS, "99")) 


# Renta: mandamos a valores perdidos las categorías 98 y 99 
dta <- dta %>%
  mutate(renta=na_if(D26, "98")) %>%
  mutate(renta=na_if(D26, "99"))

# Número de adultos: sumamos los adultos y los niños en el hogar
dta <- dta %>%
  mutate(NADULTOS=as.numeric(NADULTOS), 
         NMENORES=as.numeric(NMENORES)) %>%
  mutate(personas_hogar=NADULTOS+NMENORES)


# Último código de la limpieza: 
# Quitamos a los menores de edad y a todas las perosnas que tienen valores perdidos en nuestras variables
# Eliminamos también duplicados, que al juntar con la encuesta de hogar puede haber
dta <- dta %>%
  filter(edad>17) %>%
  drop_na(diabetes) %>%
  drop_na(clase) %>%
  drop_na(estudios) %>%
  drop_na(renta) %>%
  drop_na(personas_hogar) %>%
  distinct(IDENTHOGAR, .keep_all= TRUE)


