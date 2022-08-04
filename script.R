library (tidyverse)
library(broom)
library(scales)

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



##### DESCRIPTIVOS DE DIABETES POR CLASE SOCIAL #######

# Combinamos funciones de dplyr y sacamos la n y el %, y este comando nos sirve para otras categóricas
dta %>% group_by(clase, diabetes) %>% #agrupamos por las 2 variables 
  summarise(n = n()) %>% #Calculamos la n en cada una de la celdas
  spread(key = diabetes, value = n) %>% #Introducimos la función spread para dividir diabetes por columnas
  mutate(prop=(`1`/`0`)*100) %>% #Calculamos el % de diabeticos por clase social al dividir diabético y no diabeticos
  select(-`0`) %>% rename(n_diabetes=`1`) #Nos quitamos la columna de los 0 y renombramos la otra columna para dejarla bonita


# ¿Quieres guardar esta tabla como un data frame y exportarlo? Muy fácil!
clase_diabetes <- dta %>% group_by(clase, diabetes) %>% 
  summarise(n = n()) %>% 
  spread(key = diabetes, value = n) %>% 
  mutate(prop=(`1`/`0`)*100) %>% 
  select(-`0`) %>% rename(n_diabetes=`1`) 

write.csv(clase_diabetes, file="clase_diabetes.csv") #yo siempre exporto todo a csv




#Vamos a hacer una regresión de Poisson y graficarla para verlo de otra forma

dta$clase<-as.factor(dta$clase)
model1<-glm(formula=diabetes~clase+SEXOa+edad, data=dta,
               family=poisson(link="log"))
summary(model1)

#Sacamos las razones de prevalencia en tablita que ya sabes interpretar (clase 6 tiene 2.38 veces más prevalencia que clase 1)
coef_model1 <- model1 %>% tidy %>% 
  mutate(PR=exp(estimate), #Esto es calcular la razón de prevalencias desde el estimador poisson
         infci=exp(estimate-1.96*std.error),
         supci=exp(estimate+1.96*std.error)) %>% #Calculamos IC 95%
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




