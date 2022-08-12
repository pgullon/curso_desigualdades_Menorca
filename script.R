### INSTALAR LOS PAQUETES EN R ###

# R funciona mediante "paquetes" que contienen una serie de funciones que nos van a permitir trabajar con los datos.
# Para poder trabajar con las funciones de los paquetes, necesitamos primero tenerlos instalados en nuestro programa.
# Solo es necesario instalar una vez los paquetes para poderlos usar cada vez que abramos el programa. La función que
# nos permite instalar paquetes es 'install.packages("nombre del paquete)'. A continuación, instalamos los paquetes 
# que vamos a utilizar durante el curso.

install.packages("tidyverse")
install.packages("broom")
install.packages("scales")

### CARGAR LOS PAQUETES ###

# Una vez que tenemos instalados los paquetes, necesitamos cargarlos en nuestra librería para poder empezar a trabajar.
# Con que los carguemos una vez al abrir R, ya es suficiente. Si cerramos el programa, necesitamremos volverlos al cargar
# antes de ponernos a trabajar. 

library (tidyverse)
library(broom)
library(scales)

rm(list=ls()) # Esta función sirve para eliminar los datos y objetos que pudiéramos tener cargados de sesiones previas



load("datos/dta.RData") # Cargamos los últimos datos disponibles de la Encuesta Europea de Salud en España (EESE) 


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


#Diabetes: creamos una variable si alguien ha sido diagnosticado o refiere tener diabetes. La función para 
# crear variables nuevas es 'mutate'. Esta variable tendrá valor 0 si no se tiene diabetes, y valor 1 si se tiene diabetes

dta <-  dta %>% # El '%>%' nos permite indicar que las siguientes funciones se aplican al objeto "dta"
  mutate(diabetes=case_when((G25a_12==1 |G25c_12==1) ~ 1, #'case_when' nos permite establecer condiciones para asignar valores
                            (G25a_12==2 | G25c_12==2) ~ 0))

## Podemos interpretar lo de arriba (y los siguientes ejemplos) de la siguiente forma:
## Crea un objeto llamado "dta" (como nuestro objeto ya se llama "dta", lo sobreescribirá) que sea el objeto "dta" añadiendo
## la variable "diabetes" que tendrá valor 1 (DIABETES) cuando las variables G25a_12 o G25c_12 sean igual a 1, y que tendrá
## valor 0 (NO DIABETES) cuando las variables G25a_12 y G"%c_12 sean igual a 2.

# Sexo: la variable se llama SEXOa, 1=Hombre, 2=Mujer

# Edad: la tenemos que calcular porque no la coge como numérica
dta <- dta %>%
  mutate(edad=as.numeric(EDADa)) %>%  # Como carga la variable como categórica (¿?) necesitamos crear una variable que interprete EDADa como numérica ('as.numeric')
  
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
# Quitamos a los menores de edad y a todas las perosnas que tienen valores perdidos en nuestras variables
# Eliminamos también duplicados, que al juntar con la encuesta de hogar puede haber
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
dta %>% group_by(clase, diabetes) %>% # Agrupamos por las 2 variables 
  summarise(n = n()) %>% # Calculamos la n en cada una de la celdas
  spread(key = diabetes, value = n) %>% # Introducimos la función spread para dividir diabetes por columnas
  mutate(prop=(`1`/`0`)*100) %>% # Calculamos el % de diabeticos por clase social al dividir diabético y no diabeticos
  select(-`0`) %>% rename(n_diabetes=`1`) # Nos quitamos la columna de los 0 y renombramos la otra columna para dejarla bonita


# ¿Quieres guardar esta tabla como un data frame y exportarlo? Muy fácil!
clase_diabetes <- dta %>% group_by(clase, diabetes) %>% 
  summarise(n = n()) %>% 
  spread(key = diabetes, value = n) %>% 
  mutate(prop=(`1`/`0`)*100) %>% 
  select(-`0`) %>% rename(n_diabetes=`1`) 

write.csv(clase_diabetes, file="clase_diabetes.csv") # Así exportamos la tabla que hemos creado a un archivo formato 'csv'




#Vamos a hacer una regresión de Poisson y graficarla para verlo de otra forma

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



base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}




figura_modelo <-ggplot(coef_model1, aes(x=term, y=PR, group=group)) + 
  geom_hline(yintercept = 1, lty=2)+
  geom_line()+
  geom_point(size=2, position=position_dodge(width = 0.75)) +
  geom_errorbar(aes(ymin=infci, ymax=supci), width=.0, position = position_dodge(width=0.75))+
  labs(y="Razón de prevalencia (IC 95%)", x="Clase social ocupacional")+
  scale_y_continuous(trans='log10', breaks = pretty_breaks(5), limits = c(0.8, 2.77))+
  theme_bw()
figura_modelo
ggsave(filename = "figura_modelo.pdf", plot = figura_modelo, width=10, height=7.5)




