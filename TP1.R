#Trabajo Práctico N° 1

#Se trabajará con datos correspondientes a los Departamentos en venta del 2020,
#según la información disponible en BA Data.

#Activación de librerías

library(tidyverse)
library(skimr)

#Importación de datos
datos <- read_csv(file = "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/secretaria-de-desarrollo-urbano/departamentos-venta/departamentos-en-venta-2020.csv")

# Visualización rápida del contenido de la base de datos:
skimr::skim(datos)

#Preguntas a responder:
# 1- ¿Qué tipo de departamento, según la cantidad de ambientes, ha sido el más vendido?
# 2- ¿Cuál es el precio por m2 promedio para cada barrio, y cuál es el barrio con un precio por m2 más elevado?


  
# 1- ¿Qué tipo de departamento, según la cantidad de ambientes, ha sido el más vendido?

# Visualiación de las variables que componen la columna "Ambientes"

unique(datos$Ambientes)


# Primero se limpiará la base de datos, descartando las columnas que no poseen relevancia para el análisis,
# así como aquellas filas que indican que la propiedad posee -7 ambientes

dptos_venta <- datos %>% 
  select(-PropiedadS,-Cotizacion,-Trimestre) %>% 
  filter(Ambientes >= 0)
        
# Verificación de la limpieza realizada

names(dptos_venta)
unique(dptos_venta$Ambientes)

# Determinación del tipo de departamento más vendido, y su categorización según sean los más o menos vendidos,
# considerando que departamentos con menos de 10.000 transacciones son los "menos vendidos":

tipo_dpto <- dptos_venta %>% 
  group_by(Ambientes) %>% 
  summarise(Cantidad = n()) %>% 
  arrange(-Cantidad) %>%
  mutate(Relevancia_Ventas = case_when(Cantidad >= 10000 ~ "Más vendidos",
                                       Cantidad < 10000 ~ "Menos vendidos"))

# Los departamentos más vendidos son los compuestos por 2 ambientes (51.328 unidades),
# seguidos por los de 3 (43.992 unidades), por los de 1 (29.754 unidades) y los de 4 (25.230) ambientes.


# 2- ¿Cuál es el precio por m2 promedio para cada barrio, y cuál es el barrio con un precio por m2 más elevado?

# Para responder a esta pregunta, se agruparán los datos según la variable categórica "Barrio", relevándose la cantidad de unidades por barrio,
# junto al valor medio del precio en pesos y dólares, ordenándose los registros de mayor a menor y redondeando los valores en dólares.

promedio_barrios <- dptos_venta %>% 
  group_by(Barrio) %>% 
  summarise(  cantidad = n(),
              promedio_pesos = mean(PesosM2),
              promedio_dolares = mean(DolaresM2)) %>% 
    arrange (-promedio_dolares) %>% 
  mutate(promedio_dolares = (round(promedio_dolares, digits = 2)))
  
# De esta manera, se puede observar que Puerto Madero es el barrio con el precio por m2 en dólares promedio más elevado (USD6536.06),
# seguido por Nuñez (USD 3680.15) y Palermo (USD 3651.93).
