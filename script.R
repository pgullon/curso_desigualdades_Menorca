### INSTALAR LOS PAQUETES EN R ###

# R funciona mediante "paquetes" que contienen una serie de funciones que nos van a permitir trabajar con los datos.
# Para poder trabajar con las funciones de los paquetes, necesitamos primero tenerlos instalados en nuestro programa.
# Solo es necesario instalar una vez los paquetes para poderlos usar cada vez que abramos el programa. La función que
# nos permite instalar paquetes es 'install.packages("nombre del paquete)'. A continuación, instalamos los paquetes 
# que vamos a utilizar durante el curso.

install.packages("tidyverse")
install.packages("broom")
install.packages("scales")
install.packages("PHEindicatormethods")
install.packages("qwraps2")



### CARGAR LOS PAQUETES ###

# Una vez que tenemos instalados los paquetes, necesitamos cargarlos en nuestra librería para poder empezar a trabajar.
# Con que los carguemos una vez al abrir R, ya es suficiente. Si cerramos el programa, necesitamremos volverlos al cargar
# antes de ponernos a trabajar. 

library (tidyverse)
library(broom)
library(scales)
library(PHEindicatormethods)
library(qwraps2)



rm(list=ls()) # Esta función sirve para eliminar los datos y objetos que pudiéramos tener cargados de sesiones previas



load("datos/dta.RData") # Cargamos los últimos datos disponibles de la Encuesta Europea de Salud en España del 2020 (EESE) 


#### LIMPIEZA DE DATOS #####

# Nos vamos a quedar solo con las variables que queremos de la EESE
# Para esta prueba vamos a calcular las desigualdades en diabetes. Por tanto, nos vamos a quedar con: 
# Diabetes
# Sexo
# Edad
# Clase social
# Máximos estudios alcanzados
# Renta en el hogar
# Número de personas en el hgoar


#Diabetes: creamos una variable si alguien ha sido diagnosticado o refiere tener diabetes. La función para 
# crear variables nuevas es 'mutate'. Esta variable tendrá valor 0 si no se tiene diabetes, y valor 1 si se tiene diabetes

dta <-  dta %>% # El '%>%' nos permite indicar que las siguientes funciones se aplican al objeto "dta"
  mutate(diabetes=case_when((G25a_12==1 |G25c_12==1) ~ 1, #'case_when' nos permite establecer condiciones para asignar valores
                            (G25a_12==2 | G25c_12==2) ~ 0))

## Podemos interpretar lo de arriba (y los siguientes ejemplos) de la siguiente forma:
## Crea un objeto llamado "dta" (como nuestro objeto ya se llama "dta", lo sobreescribirá) que sea el objeto "dta" añadiendo
## la variable "diabetes" que tendrá valor 1 (DIABETES) cuando las variables G25a_12 o G25c_12 sean igual a 1, y que tendrá
## valor 0 (NO DIABETES) cuando las variables G25a_12 y G25c_12 sean igual a 2.

# Sexo: la variable se llama SEXOa, y los valores que toma son 1=Hombre, 2=Mujer

# Edad: la tenemos que calcular porque no la coge como numérica al cargar la base de datos de la EESE
dta <- dta %>%
  mutate(edad=as.numeric(EDADa)) # Como carga la variable como texto necesitamos crear una variable que interprete EDADa como numérica ('as.numeric')
  
# Clase social: mandamos a valores perdidos las categorías 8 y 9 (correspondiente a NS/NC)
dta <- dta %>%
  mutate(clase=na_if(CLASE_PR.x, "8")) %>% 
  mutate(clase=na_if(CLASE_PR.x, "9")) 

## 'na_if' nos permite generar valores perdidos ("missings") si la variable CLASE_PR.x es igual a 8 o 9. Los valores perdidos en R aparecen como 'NA'

# Nivel de estudios: mandamos a valores perdidos las categorías 98 y 99 (correspondiente a NS/NC)
dta <- dta %>%
  mutate(estudios=na_if(ESTUDIOS, "98")) %>%
  mutate(estudios=na_if(ESTUDIOS, "99")) 


# Renta: mandamos a valores perdidos las categorías 98 y 99 (correspondiente a NS/NC)
dta <- dta %>%
  mutate(renta=na_if(D26, "98")) %>%
  mutate(renta=na_if(D26, "99"))

# Número de adultos: sumamos los adultos y los niños en el hogar
dta <- dta %>%
  mutate(NADULTOS=as.numeric(NADULTOS), # Al igual que pasa con EDADa, las variables se han cargado como categóricas y necesitamos decirle que son numéricas
         NMENORES=as.numeric(NMENORES)) %>%
  mutate(personas_hogar=NADULTOS+NMENORES)


# Último código de la limpieza: 
# Quitamos a los menores de edad y a todas las personas que tienen valores perdidos (missings == NA) en nuestras variables
# Eliminamos también los duplicados que se hayan podido producir al juntar con la encuesta de hogar
dta <- dta %>%
  filter(edad>17) %>% # 'filter' permite filtrar variables por los valores que deseemos. En este caso, para quedarnos con los mayores de edad
  drop_na(diabetes) %>% # 'drop_na' elimina los valores perdidos o missings ('NA') de la columna que indiquemos
  drop_na(clase) %>%
  drop_na(estudios) %>%
  drop_na(renta) %>%
  drop_na(personas_hogar) %>%
  distinct(IDENTHOGAR, .keep_all= TRUE) # 'distinct' indica a R que queremos quedarnos con un único valor (sin repetidos) de la variable que indiquemos. '.keep_all=TRUE' le indica que queremos quedarnos con el resto de variables de la base de datos



##### DESCRIPTIVOS DE DIABETES POR CLASE SOCIAL #######

# Combinamos funciones de dplyr y sacamos la n y el %, y este comando nos sirve para otras categóricas
dta %>% group_by(clase, diabetes) %>% # Agrupamos por las dos variables 
  summarise(n = n()) %>% # Calculamos la n en cada una de la celdas, es decir, cuántos diabéticos y no diabéticos hay por cada nivel de clase social
  spread(key = diabetes, value = n) %>% # Introducimos la función spread para dividir diabetes por columnas
  mutate(prop=(`1`/`0`)*100) %>% # Calculamos el % de diabeticos por clase social al dividir diabético y no diabeticos
  select(-`0`) %>% rename(n_diabetes=`1`) # Nos quitamos la columna de los 0 y renombramos la otra columna para dejarla bonita


# ¿Quieres guardar esta tabla como un data frame y exportarlo? ¡Muy fácil!
clase_diabetes <- dta %>% group_by(clase, diabetes) %>% 
  summarise(n = n()) %>% 
  spread(key = diabetes, value = n) %>% 
  mutate(prop=(`1`/`0`)*100) %>% 
  select(-`0`) %>% rename(n_diabetes=`1`) 

write.csv(clase_diabetes, file="clase_diabetes.csv") # Así exportamos la tabla que hemos creado a un archivo formato 'csv'




#Vamos a hacer una regresión de Poisson, ajustando por edad y sexo, para ver la relación entre diabetes y clase social, y graficarla para verlo de otra forma

dta$clase<-as.factor(dta$clase)
model1<-glm(formula=diabetes~clase+SEXOa+edad, data=dta, #Ajustamos por edad y sexo en el modelo
               family=poisson(link="log"))
summary(model1) # Así podemos ver en R las estimaciones del modelo

#Sacamos las razones de prevalencia en tablita que ya sabes interpretar (clase 6 tiene 2.38 veces más prevalencia que clase 1)
coef_model1 <- model1 %>% tidy %>% 
  mutate(PR=exp(estimate), #Esto es calcular la razón de prevalencias desde el estimador poisson
         infci=exp(estimate-1.96*std.error),
         supci=exp(estimate+1.96*std.error)) %>% # Calculamos IC 95%
  select(term, PR, infci, supci) %>% #Limpiamos variables
  filter(term!="(Intercept)") %>% #Nos quitamos el intercepto
  filter(term!="SEXOa") %>%
  filter(term!="edad") %>%
  mutate(group=1) %>% #Esta variable es solo para la figura
  add_case(term="Clase1", PR=1, infci=1, supci=1, group=1, .before=1) #Añadimos el grupo de referencia en clase! 
coef_model1


#Este código nos vale para convertir la escala del eje Y de la la figura en logarítmica 
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}



#Aquí hacemos la figura!
figura_modelo <-ggplot(coef_model1, aes(x=term, y=PR, group=group)) + #'figura_modelo' será el objeto con nuestra figura, primero indicamos de dónde salen los datos, y luego cuáles son las variables X e Y que hay que representar
  geom_hline(yintercept = 1, lty=2)+ #Añadimos una línea horizontal para marcar el "valor nulo" que, al ser razones de prevalencia, corresponde con Y=1.
  geom_line()+ #Como hemos indicado antes cuáles son las variables, con esta función le indicamos que queremos que dibuje líneas tomando los valores de las variables
  geom_point(size=2, position=position_dodge(width = 0.75)) + #Esto dibujará un punto tomando los valores de las variables
  geom_errorbar(aes(ymin=infci, ymax=supci), width=.0, position = position_dodge(width=0.75))+ #Esto dibujará una línea que representará el intervalo de confianza de cada PR según la clase social
  labs(y="Razón de prevalencia (IC 95%)", x="Clase social ocupacional")+
  scale_y_continuous(trans='log10', breaks = pretty_breaks(5), limits = c(0.8, 2.77))+
  theme_bw()
figura_modelo
ggsave(filename = "figura_modelo.pdf", plot = figura_modelo, width=10, height=7.5)





## MEDIDAS DE ASOCIACIÓN DE LAS DESIGUALDADES SOCIALES EN SALUD ## 

#RELATIVE INDEX OF INEQUALITY (RII) Y SLOPE INDEX OF INEQUALITY (SII)
#Primero creamos variaciones de la variable clase social para poder calcular los índices
dta <- dta %>%
  mutate(clase_cont=as.numeric(clase)) %>% #clase en continua para algunas de las medidas
  mutate(clase_inv=(clase_cont-7)*(-1)) %>% #clase invertida (clase 6 pasa a ser clase 1)
  mutate(clase_tr=rescale(clase_cont)) #En esta la clase en lugar de 1 a 6 va de 0 a 1


# MÉTODO 1: PAQUETE PHE
# Vamos a preparar la base de datos para hacer el RII y el SII con el paquete phe
# Esta base va a ser una base agrupando las prevalencias por clase
# Usamos la clase invertida, donde el 6 representa la clase más alta, y el 1 a trabajadorxs manuales
phe_data <- dta %>% 
  group_by(clase_inv, SEXOa) %>% 
  summarise(pop=n(),
             mean.ci = list(mean_ci(diabetes))) %>%
  unnest_wider(mean.ci) %>%
  rename(diabetes=mean) %>%
  mutate(diabetes=as.numeric(diabetes), 
         lcl=as.numeric(lcl), 
         ucl=as.numeric(ucl))

phe <- phe_data %>%
  group_by(SEXOa) %>%
  phe_sii(quantile=clase_inv, 
          population=pop, 
          value=diabetes, 
          value_type = 2, 
          lower_cl = lcl, 
          upper_cl= ucl,
          multiplier = -100, 
          rii=T) 
phe <- phe %>%
  rename(sii_infci=sii_lower95_0cl, 
         sii_supci=sii_upper95_0cl, 
         rii_infci=rii_lower95_0cl, 
         rii_supci=rii_upper95_0cl) %>%
  select(sii, sii_infci, sii_supci, 
         rii, rii_infci, rii_supci) %>%
  mutate(method="phe")
phe


# MÉTODO 2: HACIENDO REGRESIONES (sin ajustar primero por otras variables)
#Regresiones de Poisson multiplicativas para el RII
model2_hombres<-glm(formula=diabetes~clase_tr, data=subset(dta,SEXOa==1), 
            family=poisson(link="log"))
coef_model2_hombres <- model2_hombres %>% tidy %>% 
  mutate(rii=exp(estimate), #Esto es calcular la razón de prevalencias desde el estimador poisson
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% # Calculamos IC 95%
  filter(term!="(Intercept)") %>% #Nos quitamos el intercepto
  select(rii, rii_infci, rii_supci) %>% #Limpiamos variables
  mutate(SEXOa=1)
coef_model2_hombres

model2_mujeres<-glm(formula=diabetes~clase_tr, data=subset(dta,SEXOa==2), 
                    family=poisson(link="log"))
coef_model2_mujeres <- model2_mujeres %>% tidy %>% 
  mutate(rii=exp(estimate), #Esto es calcular la razón de prevalencias desde el estimador poisson
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% # Calculamos IC 95%
  filter(term!="(Intercept)") %>% #Nos quitamos el intercepto
  select(rii, rii_infci, rii_supci) %>% #Limpiamos variables
  mutate(SEXOa=2)
coef_model2_mujeres
rii_regresion=rbind(coef_model2_hombres, coef_model2_mujeres)

#Regresiones de Poisson aditivas para el SII
model3_hombres<-glm(formula=diabetes~clase_tr, data=subset(dta,SEXOa==1), 
                    family=poisson(link="identity"))
coef_model3_hombres <- model3_hombres %>% tidy %>% 
  mutate(sii=estimate*100, #Para expresar la diferencia en %
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% # Calculamos IC 95%
  filter(term!="(Intercept)") %>% #Nos quitamos el intercepto
  select(sii, sii_infci, sii_supci) %>% #Limpiamos variables
  mutate(SEXOa=1)
coef_model3_hombres

model3_mujeres<-glm(formula=diabetes~clase_tr, data=subset(dta,SEXOa==2), 
                    family=poisson(link="identity"))
coef_model3_mujeres <- model3_mujeres %>% tidy %>% 
  mutate(sii=estimate*100, #Para expresar la diferencia en %
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% # Calculamos IC 95%
  filter(term!="(Intercept)") %>% #Nos quitamos el intercepto
  select(sii, sii_infci, sii_supci) %>% #Limpiamos variables
  mutate(SEXOa=2)

coef_model3_mujeres

sii_regresion=rbind(coef_model3_hombres, coef_model3_mujeres) #Esto añade las filas del modelo de mujeres debajo de las filas de los resultados del modelo de hombres

regresion <- rii_regresion %>% #De esta forma, juntamos los resultados del RII e SII creando una variable que permita identificar los resultados de cada modelo
  left_join(sii_regresion) %>%
  mutate(method="regresion")

comparacion_metodos<- regresion %>% #Aquí juntamos los resultados obtenidos con el método 1 para poder compararlos entre sí
  rbind(phe)

comparacion_metodos
#Veis que dan resultados parecidos


#Como la regresión permite ajustar por variables, vamos a hacer las regresiones ajustadas por edad
#RII
mod_rii_ajustado_overall<-glm(formula=diabetes~clase_tr+edad+SEXOa, data=dta, 
                              family=poisson(link="log"))
coef_rii_ajustado_overall <- mod_rii_ajustado_overall %>% tidy %>% 
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(SEXOa=0)
coef_rii_ajustado_overall

mod_rii_ajustado_hombres<-glm(formula=diabetes~clase_tr+edad, data=subset(dta,SEXOa==1), 
                    family=poisson(link="log"))
coef_rii_ajustado_hombres <- mod_rii_ajustado_hombres %>% tidy %>% 
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% 
  mutate(SEXOa=1)
coef_rii_ajustado_hombres

mod_rii_ajustado_mujeres<-glm(formula=diabetes~clase_tr+edad, data=subset(dta,SEXOa==2), 
                    family=poisson(link="log"))
coef_rii_ajustado_mujeres <- mod_rii_ajustado_mujeres %>% tidy %>% 
  mutate(rii=exp(estimate), 
         rii_infci=exp(estimate-1.96*std.error),
         rii_supci=exp(estimate+1.96*std.error)) %>% 
  filter(term=="clase_tr") %>% 
  select(rii, rii_infci, rii_supci) %>% #Limpiamos variables
  mutate(SEXOa=2)

coef_rii_ajustado_mujeres

rii_ajustado=coef_rii_ajustado_overall %>%
  rbind(coef_rii_ajustado_hombres) %>%
  rbind(coef_rii_ajustado_mujeres)

rii_ajustado

#SII
mod_sii_ajustado_overall<-glm(formula=diabetes~clase_tr+edad, data=dta, 
                              family=poisson(link="identity"),start = c(0, 1, 18))
coef_sii_ajustado_overall <- mod_sii_ajustado_overall %>% tidy %>% 
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>%
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci) %>% 
  mutate(SEXOa=0)
coef_sii_ajustado_overall

mod_sii_ajustado_hombres<-glm(formula=diabetes~clase_tr+edad, data=subset(dta,SEXOa==1), 
                    family=poisson(link="identity"), start = c(0, 1, 18))
coef_sii_ajustado_hombres <- mod_sii_ajustado_hombres %>% tidy %>% 
  mutate(sii=estimate*100, 
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>%
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci) %>% 
  mutate(SEXOa=1)
coef_sii_ajustado_hombres

mod_sii_ajustado_mujeres<-glm(formula=diabetes~clase_tr+edad, data=subset(dta,SEXOa==2), 
                    family=poisson(link="identity"), start = c(0, 1, 18))
coef_sii_ajustado_mujeres <- mod_sii_ajustado_mujeres %>% tidy %>% 
  mutate(sii=estimate*100,
         sii_infci=(estimate-1.96*std.error)*100,
         sii_supci=(estimate+1.96*std.error)*100) %>% 
  filter(term=="clase_tr") %>% 
  select(sii, sii_infci, sii_supci) %>%
  mutate(SEXOa=2)
coef_sii_ajustado_mujeres

sii_ajustado=coef_sii_ajustado_overall %>%
  rbind(coef_sii_ajustado_hombres) %>%
  rbind(coef_sii_ajustado_mujeres)
sii_ajustado


