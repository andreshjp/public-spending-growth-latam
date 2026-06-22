# ============================================================
# public-spending-growth-latam
# Author: Andrés Jiménez | github.com/andreshjp
# ============================================================

# Paquetes necesarios:
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(tibble)
library(purrr)
library(countrycode)   
library(lmtest)
library(plm)
library(car)
library(broom)
library(sandwich)
library(mctest)
library(mice)
library(caret) 
library(segmented)

#===============================================================================
# Empezaremos por limpiar las bases de datos
# Datos de la CEPAL
# Importar xlsx a RStudio
CEPAL <- read_excel("Datos CEPAL.xlsx")

# Mantener solo las comlumnas que serán utilizadas
CEPAL <- CEPAL[, c("País__ESTANDAR", "Gasto público por función", "Años__ESTANDAR", "value")]

# Renombramos las columnas
CEPAL <- CEPAL %>% 
  rename(
    país = País__ESTANDAR,
    función = 'Gasto público por función',
    año = Años__ESTANDAR,
    valor = value)

# Vistazo general
glimpse(CEPAL)

# Cambiamos el nombre de los países para que se acoplen a los demás datos
CEPAL <- CEPAL %>%
  mutate(país = case_when(
    país == "Bolivia (Estado Plurinacional de)" ~ "Bolivia",
    TRUE ~ país
  ))

# Cambiamos el formato de las variable 
CEPAL <- CEPAL %>% 
  mutate(across(c(año, valor), as.numeric))


# Transformamos esto a un formato de datos de panel
# Primero debenos completar los años faltantes para algunos países
# Crear un vector con todos los años
años <- min(CEPAL$año):max(CEPAL$año)

# Completar los años faltantes y luego usar pivot_wider 
CEPAL_panel <- CEPAL %>%
  complete(país, función, año = años) %>%
  pivot_wider(names_from = año, values_from = valor)


# Datos del FMI
# Importar xlsx a RStudio
FMI <- read_excel("Datos FMI.xlsx")

# Convertimos los 0 en N/A
FMI[FMI == 0] <- NA

# Eliminar columnas que no serán utilizadas
FMI <- FMI %>% 
  dplyr::select(-c('Indicator ID','Attribute 2', 'Attribute 3', 'Partner', 'Economy ISO3'))

# Renombramos las columnas
FMI <- FMI %>% 
  rename(
    país_inglés = 'Economy Name',
    función = Indicator,
    'sección gobierno' = 'Attribute 1')

# Vistazo general
glimpse(FMI)
head(FMI,66) %>% 
  print(n=66)

# Ahora debemos filtrar para solo manetener aquellas funciones del gasto a estudiar
FMI <- FMI %>% 
  filter(función %in% c("Expenditure on economic affairs, Domestic currency",
                        "Expenditure on defense, Domestic currency",
                        "Expenditure on education, Domestic currency",
                        "Expenditure on general public services, Domestic currency",
                        "Expenditure on housing & community amenities, Domestic currency",
                        "Expenditure on health, Domestic currency",
                        "Expenditure on environment protection, Domestic currency",
                        "Expenditure on public order & safety, Domestic currency",
                        "Expenditure on recreation, culture, & religion, Domestic currency",
                        "Expenditure on social protection, Domestic currency",
                        "Expenditure, Domestic currency"))

# Ahora debemos unificar el gasto de los distitnos tipo de gobierno para aquellos cuyos datos sobre el gobierno general no están disponibles
# Definir las secciones de gobierno a sumar cuando general governmet no esté diponible
secciones_a_sumar <- c("Budgetary central government", 
                       "Central government (incl. social security funds)", 
                       "Extrabudgetary central government", 
                       "Social security funds", 
                       "State governments")

# Filtrar y sumar
FMI_consolidado <- FMI %>%
  group_by(país_inglés, función) %>%
  summarise(across(`1972`:`2022`, ~ {
    if ("General government" %in% `sección gobierno` && !is.na(.x[`sección gobierno` == "General government"])) {
      .x[`sección gobierno` == "General government"]
    } else {
      sum(.x[`sección gobierno` %in% secciones_a_sumar], na.rm = TRUE)
    }
  })) %>%
  ungroup()

FMI_final <- FMI_consolidado %>%
  pivot_longer(cols = `1972`:`2022`, names_to = "Año", values_to = "Gasto") %>%
  pivot_wider(names_from = función, values_from = Gasto)

# Expandir al formato de panel
FMI_panel <- FMI_consolidado %>%
  mutate(`sección gobierno` = "General government") %>%  # Agregar la columna de sección de gobierno
  pivot_longer(cols = `1972`:`2022`, names_to = "Año", values_to = "Gasto") %>%  # Convertir años en filas
  pivot_wider(names_from = Año, values_from = Gasto)  # Volver a formato wide (años como columnas)

# Cambiamos los nombres de las funciones para que coincidan con las demás bases
FMI_panel <- FMI_panel %>%
  mutate(función = case_when(
    función == "Expenditure on defense, Domestic currency" ~ "Defensa",
    función == "Expenditure on economic affairs, Domestic currency" ~ "Asuntos económicos",
    función == "Expenditure on education, Domestic currency" ~ "Educación",
    función == "Expenditure on environment protection, Domestic currency" ~ "Protección del medio ambiente",
    función == "Expenditure on general public services, Domestic currency" ~ "Servicios públicos generales",
    función == "Expenditure on health, Domestic currency" ~ "Salud",
    función == "Expenditure on housing & community amenities, Domestic currency" ~ "Vivienda y servicios comunitarios",
    función == "Expenditure on public order & safety, Domestic currency" ~ "Orden público y seguridad",
    función == "Expenditure on recreation, culture, & religion, Domestic currency" ~ "Actividades recreativas, cultura y religión",
    función == "Expenditure on social protection, Domestic currency" ~ "Protección social",
    función == "Expenditure, Domestic currency" ~ "Erogaciones totales",
    TRUE ~ función
  ))

# Eliminamos la columna de sección de gobierno puesto que no será necesaria
FMI_panel <- FMI_panel[, -3]

# Traducimos los países a Español 
FMI_panel$país <- countrycode(FMI_panel$país_inglés, origin = "country.name", destination = "cldr.short.es")

# Reorganizar las columnas para que país quede en la segunda posición
FMI_panel <- FMI_panel %>%
  dplyr::select(país, everything())

# Eliminamos la columna de países en inglés puesto que no será necesaria
FMI_panel <- FMI_panel[, -2]

# Utilizamos solo los países que tienen todas la finalidades del gasto
FMI_panel <- FMI_panel %>%
  filter(!is.na(`país`)) %>%
  group_by(`país`) %>%
  filter(n() == 11) %>%
  ungroup()

# Convertimos los 0 en N/A
FMI_panel[FMI_panel == 0] <- NA


# Datos del Eurostat
# Cómo esta viene en varias hojas de excel, se creará una función para procesar cada hoja y seguir el mismo procedimiento en cada una
funcion_Eurostat <- function(hojas, funciones) {
  # Importar xlsx a RStudio
  Eurostat_Total <- read_excel("Datos Eurostat.xlsx", sheet = hojas)
  
  # Eliminamos el encabezado y otras filas que no serán utilizadas 
  Eurostat_Total <- Eurostat_Total %>%
    slice(-c(1:9, 11:14, 45:n()))
  
  # Mantenemos solo las columnas que no están vacías y que no tienen valores "p" y "b"
  Eurostat_Total <- Eurostat_Total %>%
    select_if(function(x) {
      !all(is.na(x)) &&
        !(any(x %in% c("p","b"), na.rm = TRUE) && sum(is.na(x)) == (length(x) - sum(x %in% c("p","b"), na.rm = TRUE)))
    })
  
  # Colocamos la primera fila como los nombres de las columnas y la eliminamos 
  # Verificamos que la primera fila no contenga NA antes de usarla como nombres
  nombres_columnas <- as.character(Eurostat_Total[1, ])
  nombres_columnas[is.na(nombres_columnas)] <- "Columna_Sin_Nombre"  # Reemplazar NA con un nombre válido
  Eurostat_Total <- Eurostat_Total[-1, ]
  colnames(Eurostat_Total) <- nombres_columnas
  
  # Renombramos las columnas 
  Eurostat_Total <- Eurostat_Total %>% 
    rename(país_inglés = TIME) 
  
  # Añadimos la función respectiva
  Eurostat_Total <- Eurostat_Total %>%
    add_column(función = funciones, .after = 1)
  
  return(Eurostat_Total)
}

# Creamos una lista con los nombres de las hojas de excel a procesar y las funciones respectivas a cada hoja
hojas <- c("Sheet 1", "Sheet 2", "Sheet 11", "Sheet 17", "Sheet 24", "Sheet 34", "Sheet 41", "Sheet 48", "Sheet 55", "Sheet 62", "Sheet 71")
funciones <- c("Erogaciones totales", "Servicios públicos generales", "Defensa", "Orden público y seguridad", "Asuntos económicos", "Protección del medio ambiente", "Vivienda y servicios comunitarios", "Salud", "Actividades recreativas, cultura y religión", "Educación", "Protección social")

# Iteramos sobre los vectores hechos anteriormente
Eurostat_Completo <- map2(hojas, funciones, funcion_Eurostat)              

# Finalmente, combinamos los dataframes resultantes en uno solo
Eurostat <- bind_rows(Eurostat_Completo)

# Vistazo general
glimpse(Eurostat)

# Traducimos los países a Español 
Eurostat$país <- countrycode(Eurostat$país_inglés, origin = "country.name", destination = "cldr.short.es")

# Reorganizar las columnas para que país quede en la segunda posición
Eurostat <- Eurostat %>%
  dplyr::select(país, everything())

# Eliminamos la columna de países en inglés puesto que no será necesaria
Eurostat <- Eurostat[, -2]

# Convertimos los : en N/A
Eurostat[Eurostat == ":"] <- NA


#===============================================================================
# Ahora se unificará la información de las anteriores bases de datos 
# Primero debemos filtar de la base del FMI los países que hay en las otras bases
# Obtener los países únicos de CEPAL y Eurostat
paises_cepal <- unique(CEPAL_panel$país)

paises_eurostat <- unique(Eurostat$país)

# Combinar los países únicos de ambas bases de datos
paises_filtrar <- unique(c(paises_cepal, paises_eurostat))

# Filtrar la base de datos del FMI
FMI_panel <- FMI_panel %>%
  anti_join(tibble(país = paises_filtrar), by = "país")

FMI_panel %>% 
  distinct(`país`) %>% 
  nrow()


# Primero unificamos las de la CEPAL, FMI y Eurostat
# Antes de esto colocamos las columnas del mismo formato
CEPAL_panel <- CEPAL_panel %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric),
         across(c(país, función), as.factor))

FMI_panel <- FMI_panel %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric),
         across(c(país, función), as.factor))

Eurostat <- Eurostat %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric),
         across(c(país, función), as.factor))

# Unificamos estas tres bases
Datos <- bind_rows(Eurostat, CEPAL_panel, FMI_panel)

# Eliminamos los años que no aplican para el análsis
Datos <- Datos[, -(37:54)]

# Verificamos la muestra de países hasta el momento
Datos %>% 
  distinct(`país`) %>% 
  nrow()

Datos %>% 
  distinct(`país`) %>% 
  print(n=139)

# Cambiamos el nombre de algunos países para que coincida con las siguientes bases hacemos la variable factor
Datos <- Datos %>%
  mutate(país = as.factor(  
    case_when(
      país == "EE. UU." ~ "Estados Unidos",
      país == "Myanmar (Birmania)" ~ "Myanmar",
      TRUE ~ país
    )  
  )) 


#===============================================================================
# Ya que tenemos el gasto, ahora deberemos incluir el crecimeinto económico y, con este, otras variables explicativas para el modelo
# Específicamente la inversión, el consumo final de los hogares, las exportaciones y las importaciones.
# Importamos los csv saltando las filas innecesarias
PIB <- read_csv("Datos PIB.csv", skip = 3)
consumo <- read_csv("Datos Consumo.csv", skip = 3)
inversion <- read_csv("Datos FBKF.csv", skip = 3)
exportaciones <- read_csv("Datos Exportaciones.csv",skip = 3)
importaciones <- read_csv("Datos Importaciones.csv", skip = 3)

# Eliminamos las columnas que no serán utilizadas
PIB <- PIB[, -(2:4)]
consumo <- consumo[, -(2:4)]
inversion <- inversion[, -(2:4)]
exportaciones <- exportaciones[, -(2:4)]
importaciones <- importaciones[, -(2:4)]

# Agregamos la columna función que para cada caso y colocamos el nombre para que coincida con las otras bases de datos
PIB <- PIB %>% 
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>% 
  rename(país = 'Country Name') %>% 
  mutate(
    función = "PIB",
    país = as.factor(país), 
    función = as.factor(función) 
  ) %>% 
  relocate(función, .after = 1)

PIB %>% 
  distinct(`país`) %>% 
  nrow()

consumo <- consumo %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>%
  rename(país = 'Country Name') %>%
  mutate(
    función = "Consumo final de los hogares",
    país = as.factor(país), 
    función = as.factor(función) 
  ) %>% 
  relocate(función, .after = 1)

consumo %>% 
  distinct(`país`) %>% 
  nrow()

inversion <- inversion %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>%
  rename(país = 'Country Name') %>%
  mutate(
    función = "Formación bruta de capital fijo",
    país = as.factor(país), 
    función = as.factor(función) 
  ) %>% 
  relocate(función, .after = 1)

inversion %>% 
  distinct(`país`) %>% 
  nrow()

exportaciones <- exportaciones %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>%
  rename(país = 'Country Name') %>%
  mutate(
    función = "Exportaciones",
    país = as.factor(país), 
    función = as.factor(función) 
  ) %>% 
  relocate(función, .after = 1)

exportaciones %>% 
  distinct(`país`) %>% 
  nrow()

importaciones <- importaciones %>%
  mutate(across(starts_with("19") | starts_with("20"), as.numeric)) %>%
  rename(país = 'Country Name') %>%
  mutate(
    función = "Importaciones",
    país = as.factor(país), 
    función = as.factor(función) 
  ) %>% 
  relocate(función, .after = 1)

importaciones %>% 
  distinct(`país`) %>% 
  nrow()

# Unificamos estas nuevas variables en un df
Datos_PIB <- bind_rows(PIB, consumo, inversion, exportaciones, importaciones)


# Verificamos la muestra de países
Datos_PIB %>% 
  distinct(`país`) %>% 
  nrow()

Datos_PIB %>%
  count(`país`, sort = TRUE) %>% 
  print(n=262)

# Cambiamos el nombre a algunos países para que coincidan con los nombres del df Datos
Datos_PIB <- Datos_PIB %>%
  mutate(país = as.factor(
    case_when(
      país == "Rumania" ~ "Rumanía",
      país == "Bahrein" ~ "Baréin",
      país == "Bangladesh" ~ "Bangladés",
      país == "Bhután" ~ "Bután",
      país == "Botswana" ~ "Botsuana",
      país == "Côte d'Ivoire" ~ "Côte d’Ivoire",
      país == "Fiji" ~ "Fiyi",
      país == "Kazajstán" ~ "Kazajistán",
      país == "Kenya" ~ "Kenia",
      país == "Corea" ~ "Corea del Sur",
      país == "Región Administrativa Especial de Macao" ~ "Macao",
      país == "Nueva Zelandia" ~ "Nueva Zelanda",
      país == "Palau" ~ "Palaos",
      país == "Papua Nueva Guinea" ~ "Papúa Nueva Guinea",
      país == "Qatar" ~ "Catar",
      país == "Tanzanía" ~ "Tanzania",
      país == "Belarús" ~ "Bielorrusia",
      TRUE ~ país
    )
  ))

# Debemos filtar los países para los cuales seleccionaremos las demás variables
# Obtener los países únicos en Datos
paises_en_datos <- unique(Datos$país)

# Filtrar para incluir solo los países que están en Datos
Datos_PIB_filtrado <- Datos_PIB %>%
  semi_join(tibble(país = paises_en_datos), by = "país")

Datos_PIB_filtrado %>% 
  distinct(`país`) %>% 
  nrow()

setdiff(Datos$país, Datos_PIB_filtrado$país)

# Unificar Datos con las nuevas variables
paises_comunes <- intersect(Datos$país, Datos_PIB_filtrado$país)

Datos_completos <- bind_rows(
  Datos %>% 
    filter(país %in% paises_comunes),
  Datos_PIB_filtrado %>% 
    filter(país %in% paises_comunes)
)

# Verificamos la muestra
Datos_completos %>% 
  distinct(`país`) %>% 
  nrow()

Datos_completos %>%
  count(`país`, sort = TRUE) %>% 
  print(n=133)

# Eliminar columnas innecesarios
Datos_completos <- Datos_completos[, -(37:67)]

# Exportar la base de datos final
write_csv(Datos_completos, file = "Datos Completos.csv")

# Transformar a formato largo
Datos_largo <- Datos_completos %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), 
               names_to = "año", 
               values_to = "valor") %>%
  mutate(año = as.numeric(año))

# Verficamos la muestra
Datos_largo %>% 
  distinct(`país`) %>% 
  nrow()

Datos_largo %>%
  count(`país`, sort = TRUE) %>% 
  print(n=133)

# Verificamos datos faltantes
colSums(is.na(Datos_largo))

# Separar el PIB y las variables explicativas, verificando valores faltantes
pib <- Datos_largo %>%
  filter(función == "PIB") %>%
  dplyr::select(país, año, valor) %>%
  rename(pib = valor)

colSums(is.na(pib))

variables_explicativas <- Datos_largo %>%
  filter(función != "PIB") %>%
  pivot_wider(names_from = función, values_from = valor)

colSums(is.na(variables_explicativas))

# Unir las dos bases de datos
datos_regresion <- pib %>%
  left_join(variables_explicativas, by = c("país", "año"))

colSums(is.na(datos_regresion))

# Incluir el comercio neto 
datos_regresion <- datos_regresion %>%
  mutate(Comercio = Exportaciones - Importaciones)

colSums(is.na(datos_regresion))

# Exportar la base de datos final para la regresión
write_csv(datos_regresion, file = "Datos Regresión.csv")

# Convertir a un objeto de panel
datos_panel <- pdata.frame(datos_regresion, index = c("país", "año"))

datos_panel %>% 
  distinct(país) %>% 
  nrow()


#===============================================================================
# Veamos los países de la muestra
datos_regresion %>% 
  distinct(`país`) %>% 
  print(n=133)

datos_regresion %>%
  count(`país`, sort = TRUE) %>% 
  print(n=133)

# Ahora procedemos a hacer los modelos
# Recrear datos de panel como un pdata.frame
datos_panel <- pdata.frame(datos_regresion, index = c("país", "año"))

# Ajustar el primer modelo: Variables macroeconómicas + Erogaciones totales, para todo el dataset
modelo_1 <- plm(pib ~ Consumo.final.de.los.hogares + Formación.bruta.de.capital.fijo + 
                  Comercio + Erogaciones.totales, 
                data = datos_panel, 
                model = "within")

# Resumen del primer modelo
summary(modelo_1)

bptest(modelo_1)
pbgtest(modelo_1)
shapiro.test(residuals(modelo_1))


#===============================================================================
# A partir de aquí unos cambios de muestra para mejorar los resultados, 
# utilizando los países con más datos
#===============================================================================
# Tomar de la base en formato largo solo las observaciones entre 2000-2022
# y los 60 países con más datos, 20 de cada nivel de desarrollo

# Tomar de la base en formato largo solo las observaciones entre 2000-2022
Datos_largo_2000_2022 <- Datos_largo %>%
  filter(año >= 2000 & año <= 2022)

# Verficamos la muestra
Datos_largo_2000_2022 %>% 
  distinct(`país`) %>% 
  nrow()

Datos_largo_2000_2022 %>%
  count(`país`, sort = TRUE) %>% 
  print(n=133)


# Establecemos los países vállidos según su ínidce de desarrollo humano
muy_alto <- c(
  "Alemania", "Austria", "Bélgica", "Chipre", "Croacia", 
  "Dinamarca","Eslovenia", "España", "Estonia", "Finlandia", 
  "Francia", "Grecia", "Hungría", "Irlanda", "Islandia",
  "Italia", "Letonia", "Japón", "Estados Unidos", "Canadá" 
)

alto <- c(
  "Bulgaria", "Líbano", "Sudáfrica", "Mauricio", "Ucrania",
  "Filipinas", "Perú", "Paraguay", "Indonesia", "China", 
  "Jordania", "Albania", "Cuba", "Egipto", "Sri Lanka",
  "Jamaica", "Kirguistán", "Armenia", "Brasil", "Azerbaiyán"
)

medio_bajo <- c(
  "Guatemala", "El Salvador", "Namibia", "Uganda", "Nepal", 
  "Madagascar", "Angola", "Kenia", "India", "Zambia", 
  "Bután", "Bangladés", "Pakistán", "Etiopía", "Bolivia", 
  "Cabo Verde", "Congo", "Yemen", "Islas Salomón", "Nicaragua"
)

paises_60 <- c(muy_alto, alto, medio_bajo)

# Filtrar el dataframe original
datos_IDH  <- Datos_largo_2000_2022 %>%
  filter(país %in% paises_60)

# Verficamos la muestra
datos_IDH %>% 
  distinct(`país`) %>% 
  nrow()

datos_IDH %>%
  count(`país`, sort = TRUE) %>% 
  print(n=133)

# Verificamos datos faltantes
colSums(is.na(datos_IDH))

# Separar el PIB y las variables explicativas, verificando valores faltantes
pib_IDH <- datos_IDH  %>%
  filter(función == "PIB") %>%
  dplyr::select(país, año, valor) %>%
  rename(pib = valor)

colSums(is.na(pib_IDH))


variables_explicativas_IDH <- datos_IDH  %>%
  filter(función != "PIB") %>%
  pivot_wider(names_from = función, values_from = valor)

colSums(is.na(variables_explicativas_IDH))

# Unir las dos bases de datos
datos_regresion_IDH <- pib_IDH %>%
  left_join(variables_explicativas_IDH, by = c("país", "año"))

colSums(is.na(datos_regresion_IDH))

# Incluir el comercio neto 
datos_regresion_IDH <- datos_regresion_IDH %>%
  mutate(Comercio = Exportaciones - Importaciones)

colSums(is.na(datos_regresion_IDH))

datos_regresion_IDH %>% 
  distinct(`país`) %>% 
  nrow()

datos_regresion_IDH %>%
  count(`país`, sort = TRUE) %>% 
  print(n=133)

# Lista de variables a transformar (excluyendo país y año)
vars_numericas <- setdiff(names(datos_regresion_IDH), c("país", "año"))

# Crear variables logarítmicas
datos_regresion_IDH_log <- datos_regresion_IDH %>%
  mutate(across(all_of(vars_numericas),
                ~ ifelse(. > 0, log(.), NA_real_),  
                .names = "ln_{.col}"))

datos_regresion_IDH_log <- datos_regresion_IDH_log[, -c(3:19, 36)]

colSums(is.na(datos_regresion_IDH_log))

# Crear variables como porcentaj del PIB
# Lista de variables para dividir por PIB (excluyendo el propio pib)
vars_ratio_pib <- setdiff(vars_numericas, "pib")

datos_regresion_IDH_pib <- datos_regresion_IDH %>%
  mutate(
    ln_pib = ifelse(pib > 0, log(pib), NA_real_),
    across(all_of(vars_ratio_pib),
           ~ ifelse(pib != 0, ./pib, NA_real_),
           .names = "{.col}_pib")
  ) %>%
  dplyr::select(país, año, ln_pib, ends_with("_pib"))

# Exportar la base de datos final para la regresión
write_csv(datos_regresion_IDH, file = "Datos Regresión IDH.csv")

#===============================================================================
# IMPUTACIÓN DE DATOS
#===============================================================================
# IMPUTANDO EN LOG
# MICE (Imputación Multivariante con Correlaciones)
# Seleccionar variables a imputar (todas las numéricas)
vars_imputar_log <- datos_regresion_IDH_log %>%
  dplyr::select(-país, -año)

# Configurar MICE 
imputaciones_mice <- mice(
  datos_regresion_IDH_log,
  m = 5,                      
  method = "pmm",             
  maxit = 10,                 
  seed = 123                  
)

# Extraer el primer conjunto imputado 
datos_imputados_log <- complete(imputaciones_mice, 1)

colSums(is.na(datos_imputados_log))

# Incluir el comercio
datos_imputados_log <- datos_imputados_log %>%
  mutate(ln_Comercio = ln_Exportaciones - ln_Importaciones)

# Calcular primeras diferencias
datos_imputados_log_diff <- datos_imputados_log %>%
  arrange(país, año) %>%
  mutate(
    d_ln_pib = ifelse(
      año - dplyr::lag(año) == 1,  
      ln_pib - dplyr::lag(ln_pib),
      NA_real_),
    `d_ln_Erogaciones totales` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Erogaciones totales` - dplyr::lag(`ln_Erogaciones totales`),
      NA_real_),
    `d_ln_Servicios públicos generales` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Servicios públicos generales` - dplyr::lag(`ln_Servicios públicos generales`),
      NA_real_),
    `d_ln_Defensa` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Defensa` - dplyr::lag(`ln_Defensa`),
      NA_real_),
    `d_ln_Orden público y seguridad` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Orden público y seguridad` - dplyr::lag(`ln_Orden público y seguridad`),
      NA_real_),
    `d_ln_Asuntos económicos` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Asuntos económicos` - dplyr::lag(`ln_Asuntos económicos`),
      NA_real_),
    `d_ln_Protección del medio ambiente` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Protección del medio ambiente` - dplyr::lag(`ln_Protección del medio ambiente`),
      NA_real_),
    `d_ln_Vivienda y servicios comunitarios` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Vivienda y servicios comunitarios` - dplyr::lag(`ln_Vivienda y servicios comunitarios`),
      NA_real_),
    `d_ln_Salud` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Salud` - dplyr::lag(`ln_Salud`),
      NA_real_),
    `d_ln_Actividades recreativas, cultura y religión` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Actividades recreativas, cultura y religión` - dplyr::lag(`ln_Actividades recreativas, cultura y religión`),
      NA_real_),
    `d_ln_Educación` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Educación` - dplyr::lag(`ln_Educación`),
      NA_real_),
    `d_ln_Protección social` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Protección social` - dplyr::lag(`ln_Protección social`),
      NA_real_),
    `d_ln_Consumo final de los hogares` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Consumo final de los hogares` - dplyr::lag(`ln_Consumo final de los hogares`),
      NA_real_),
    `d_ln_Formación bruta de capital fijo` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Formación bruta de capital fijo` - dplyr::lag(`ln_Formación bruta de capital fijo`),
      NA_real_),
    `d_ln_Exportaciones` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Exportaciones` - dplyr::lag(`ln_Exportaciones`),
      NA_real_),
    `d_ln_Importaciones` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Importaciones` - dplyr::lag(`ln_Importaciones`),
      NA_real_),
    `d_ln_Comercio` = ifelse(
      año - dplyr::lag(año) == 1, 
      `ln_Comercio` - dplyr::lag(`ln_Comercio`),
      NA_real_),
  ) %>% 
  ungroup() %>%
  dplyr::select(país, año, starts_with("d_ln"))

write_csv(datos_imputados_log, file = "Datos_log.csv")
write_csv(datos_imputados_log_diff, file = "Datos_log_diff.csv")

#===============================================================================
# IMPUTANDO EN PIB
# MICE 

# Seleccionar variables a imputar (todas las numéricas)
vars_imputar_pib <- datos_regresion_IDH_pib %>%
  dplyr::select(-país, -año)

# Configurar MICE 
imputaciones_mice_pib <- mice(
  datos_regresion_IDH_pib,
  m = 5,                      
  method = "pmm",             
  maxit = 10,                 
  seed = 123                  
)

# Extraer el primer conjunto imputado 
datos_imputados_pib <- complete(imputaciones_mice_pib, 1)

colSums(is.na(datos_imputados_pib))

write_csv(datos_imputados_pib, file = "Datos_pib.csv")

# Df final 
datos_IDH <- cbind(datos_imputados_log, datos_imputados_log_diff, datos_imputados_pib)
datos_IDH <- datos_IDH[, !(names(datos_IDH) %in% c("país.1", "año.1", "país.2", "año.2","ln_pib.1", "ect"))]
glimpse(datos_IDH)

write_csv(datos_IDH, file = "Datos IDH.csv")

# Ahora incluimos los indicadores de calidad institucional
# Incluimos la base de datos de calidad institucional
INST <- read_excel("Datos Instituciones.xlsx")

# Mantener solo las comlumnas que serán utilizadas
INST <- INST[, c("countryname", "year", "indicator", "estimate")]

# Renombramos las columnas
INST <- INST %>% 
  rename(
    país_inglés = "countryname",
    función = "indicator",
    año = "year",
    valor = "estimate")

# Cambiamos los nombres de las funciones 
INST <- INST %>%
  mutate(función = case_when(
    función == "cc" ~ "Control de Corrupción",
    función == "ge" ~ "Efectividad Gubernamental",
    función == "pv" ~ "Estabilidad Política y Ausencia de Violencia",
    función == "rl" ~ "Estado de Derecho",
    función == "rq" ~ "Calidad Regulatoria",
    función == "va" ~ "Voz y Rendición de Cuentas",
    TRUE ~ función
  ))

# Cambiamos el formato de las variable 
INST <- INST %>% 
  mutate(
    across(c(año, valor), as.numeric),
    across(c(país_inglés, función), as.factor))

# Transformamos esto a un formato de datos de panel
# Tomar solo los años de análsis
INST <- INST %>%
  filter(año >= 2000 & año <= 2022)

# Usar pivot_wider 
INST_panel <- INST %>%
  pivot_wider(names_from = función, values_from = valor)

# Traducimos los países a Español 
INST_panel$país <- countrycode(INST_panel$país_inglés, origin = "country.name", destination = "cldr.short.es")

# Reorganizar las columnas para que país quede en la segunda posición
INST_panel <- INST_panel %>%
  dplyr::select(país, everything())

# Eliminamos la columna de países en inglés puesto que no será necesaria
INST_panel <- INST_panel[, -2]

# Verficamos la muestra
INST_panel  %>% 
  distinct(`país`) %>% 
  nrow()

INST_panel  %>%
  count(`país`, sort = TRUE) %>% 
  print(n=213)

# Cambiamos el nombre a algunos países para que coincidan con los nombres del df Datos
INST_panel <- INST_panel %>%
  mutate(país = as.factor(
    case_when(
      país == "EE. UU." ~ "Estados Unidos",
      TRUE ~ país
    )
  ))

# Filtrar para incluir solo los países que están en los datos
INST_panel <- INST_panel %>%
  semi_join(tibble(país = paises_60), by = "país")

INST_panel %>% 
  distinct(`país`) %>% 
  nrow()

INST_panel  %>%
  count(`país`, sort = TRUE) %>% 
  print(n=213)

# Unificar datos con las nuevas variables
datos_imputados_log <- datos_imputados_log %>% 
  left_join(INST_panel, by = c("país", "año"))

datos_imputados_log_diff <- datos_imputados_log_diff %>% 
  left_join(INST_panel, by = c("país", "año"))

datos_imputados_pib <- datos_imputados_pib %>% 
  left_join(INST_panel, by = c("país", "año"))

# Ahora incluiremos determinados agrupaciones a discreción
# En ln 
# Componente 1: seguridad y administración pública
datos_imputados_log$ln_Gasto_Seguridad_Admin <- rowSums(
  datos_imputados_log[, c("ln_Servicios públicos generales", 
                      "ln_Defensa", 
                      "ln_Orden público y seguridad")]
)

# Componente 2: desarrollo social 
datos_imputados_log$ln_Gasto_Desarrollo_Social <- rowSums(
  datos_imputados_log[, c("ln_Salud", 
                      "ln_Educación", 
                      "ln_Protección social")]
)

# Componente 3: desarrollo económico y ambiental
datos_imputados_log$ln_Gasto_Economico_Ambiental <- rowSums(
  datos_imputados_log[, c("ln_Asuntos económicos",
                      "ln_Protección del medio ambiente",
                      "ln_Vivienda y servicios comunitarios",
                      "ln_Actividades recreativas, cultura y religión")]
)


cor_matrix_group_log <- cor(
  datos_imputados_log[, c("ln_Gasto_Seguridad_Admin", "ln_Gasto_Desarrollo_Social", "ln_Gasto_Economico_Ambiental", 
                      "ln_Formación bruta de capital fijo",
                      "ln_Comercio", "ln_pib", "ln_Consumo final de los hogares")],
  use = "complete.obs"
)
print(cor_matrix_group_log) # Muy alto 

# En d_ln
# Componente 1: seguridad y administración pública
datos_imputados_log_diff$Gasto_Seguridad_Admin_diff <- rowSums(
  datos_imputados_log_diff[, c("d_ln_Servicios públicos generales", 
                           "d_ln_Defensa", 
                           "d_ln_Orden público y seguridad")]
)

# Componente 2: desarrollo social 
datos_imputados_log_diff$Gasto_Desarrollo_Social_diff <- rowSums(
  datos_imputados_log_diff[, c("d_ln_Salud", 
                           "d_ln_Educación", 
                           "d_ln_Protección social")]
)

# Componente 3: desarrollo económico y ambiental
datos_imputados_log_diff$Gasto_Economico_Ambiental_diff <- rowSums(
  datos_imputados_log_diff[, c("d_ln_Asuntos económicos",
                           "d_ln_Protección del medio ambiente",
                           "d_ln_Vivienda y servicios comunitarios",
                           "d_ln_Actividades recreativas, cultura y religión")]
)


cor_matrix_group_diff <- cor(
  datos_imputados_log_diff[, c("Gasto_Seguridad_Admin_diff", "Gasto_Desarrollo_Social_diff", "Gasto_Economico_Ambiental_diff", 
                           "d_ln_Formación bruta de capital fijo",
                           "d_ln_Comercio", "d_ln_pib", "d_ln_Consumo final de los hogares")],
  use = "complete.obs"
)
print(cor_matrix_group_diff) 


# En porcentaje del pib
# Componente 1: seguridad y administración pública
datos_imputados_pib$Gasto_Seguridad_Admin_pib <- rowSums(
  datos_imputados_pib[, c("Servicios públicos generales_pib", 
                      "Defensa_pib", 
                      "Orden público y seguridad_pib")]
)

# Componente 2: desarrollo social 
datos_imputados_pib$Gasto_Desarrollo_Social_pib <- rowSums(
  datos_imputados_pib[, c("Salud_pib", 
                      "Educación_pib", 
                      "Protección social_pib")]
)

# Componente 3: desarrollo económico y ambiental
datos_imputados_pib$Gasto_Economico_Ambiental_pib <- rowSums(
  datos_imputados_pib[, c("Asuntos económicos_pib",
                      "Protección del medio ambiente_pib",
                      "Vivienda y servicios comunitarios_pib",
                      "Actividades recreativas, cultura y religión_pib")]
)


cor_matrix_group_pib <- cor(
  datos_imputados_pib[, c("Gasto_Seguridad_Admin_pib", "Gasto_Desarrollo_Social_pib", "Gasto_Economico_Ambiental_pib", 
                      "Formación bruta de capital fijo_pib",
                      "Comercio_pib", "ln_pib", "Consumo final de los hogares_pib")],
  use = "complete.obs"
)
print(cor_matrix_group_pib)


# Convertimos los df en pdataframe
datos_panel_log <- pdata.frame(datos_imputados_log, index = c("país", "año"))
datos_panel_log_diff <- pdata.frame(datos_imputados_log_diff, index = c("país", "año"))
datos_panel_pib <- pdata.frame(datos_imputados_pib, index = c("país", "año"))

# Antes de continuar se le harán distintos cambios a las bases respectivas
# Establecer un PCA en cada df
# Primero variables log
# Seleccionar solo las variables de gasto público (proporción del PIB)
variables_pca_log <- datos_panel_log %>%
  dplyr::select(
    ln_Servicios.públicos.generales,
    ln_Defensa,
    ln_Orden.público.y.seguridad,
    ln_Asuntos.económicos,
    ln_Protección.del.medio.ambiente,
    ln_Vivienda.y.servicios.comunitarios,
    ln_Salud,
    ln_Actividades.recreativas..cultura.y.religión,
    ln_Educación,
    ln_Protección.social
  )

pca_log_result <- prcomp(variables_pca_log, center = TRUE, scale. = TRUE)

# Resumen de los componentes
summary(pca_log_result)

# Gráfico de varianza explicada (Scree Plot)
plot(pca_log_result, type = "l", main = "Scree Plot de Componentes Principales")

# Extraer los componentes necesarios
datos_panel_log$PC1 <- pca_log_result$x[,1]
datos_panel_log$PC2 <- pca_log_result$x[,2]
datos_panel_log$PC3 <- pca_log_result$x[,3]

# Cargas del componente 
loadings_log <- pca_log_result$rotation[,1:10]
print(loadings_log)
# No funciona tener un solo componente

# Ahora variables en diferencia logarítmica
# Seleccionar solo las variables de gasto público (proporción del PIB)
variables_pca_diff <- datos_panel_log_diff %>%
  dplyr::select(
    d_ln_Servicios.públicos.generales,
    d_ln_Defensa,
    d_ln_Orden.público.y.seguridad,
    d_ln_Asuntos.económicos,
    d_ln_Protección.del.medio.ambiente,
    d_ln_Vivienda.y.servicios.comunitarios,
    d_ln_Salud,
    d_ln_Actividades.recreativas..cultura.y.religión,
    d_ln_Educación,
    d_ln_Protección.social
  )

# Crea una variable auxiliar para identificar las filas con NA en
tiene_NA_pca <- apply(variables_pca_diff, 1, anyNA)

# Filas donde NO hay NA en ninguna de esas columnas
no_tiene_NA_pca <- which(!tiene_NA_pca)

# Quitamos NA para PCA
variables_pca_diff_clean <- variables_pca_diff[no_tiene_NA_pca, ]

pca_diff_result <- prcomp(variables_pca_diff_clean, center = TRUE, scale. = TRUE)

# Resumen de los componentes
summary(pca_diff_result)

# Gráfico de varianza explicada (Scree Plot)
plot(pca_diff_result, type = "l", main = "Scree Plot de Componentes Principales")

# Inicializa las columnas de los PC en tu dataframe original con NAs
# Esto crea columnas con el número correcto de filas 
datos_panel_log_diff$PC1 <- NA
datos_panel_log_diff$PC2 <- NA
datos_panel_log_diff$PC3 <- NA 
datos_panel_log_diff$PC4 <- NA 
datos_panel_log_diff$PC5 <- NA 
datos_panel_log_diff$PC6 <- NA 
datos_panel_log_diff$PC7 <- NA 

# Asigna los resultados del PCA a las filas correspondientes
datos_panel_log_diff$PC1[no_tiene_NA_pca] <- pca_diff_result$x[,1]
datos_panel_log_diff$PC2[no_tiene_NA_pca] <- pca_diff_result$x[,2]
datos_panel_log_diff$PC3[no_tiene_NA_pca] <- pca_diff_result$x[,3]
datos_panel_log_diff$PC4[no_tiene_NA_pca] <- pca_diff_result$x[,4]
datos_panel_log_diff$PC5[no_tiene_NA_pca] <- pca_diff_result$x[,5]
datos_panel_log_diff$PC6[no_tiene_NA_pca] <- pca_diff_result$x[,6]
datos_panel_log_diff$PC7[no_tiene_NA_pca] <- pca_diff_result$x[,7]

# Cargas del componente 
loadings_diff <- pca_log_result$rotation[,1:10]
print(loadings_diff)

# Ahora variables en proporción del pib 
# Seleccionar solo las variables de gasto público (proporción del PIB)
variables_pca_pib <- datos_panel_pib %>%
  dplyr::select(
    Servicios.públicos.generales_pib,
    Defensa_pib,
    Orden.público.y.seguridad_pib,
    Asuntos.económicos_pib,
    Protección.del.medio.ambiente_pib,
    Vivienda.y.servicios.comunitarios_pib,
    Salud_pib,
    Actividades.recreativas..cultura.y.religión_pib,
    Educación_pib,
    Protección.social_pib
  )

pca_pib_result <- prcomp(variables_pca_pib, center = TRUE, scale. = TRUE)

# Resumen de los componentes
summary(pca_pib_result)

# Gráfico de varianza explicada (Scree Plot)
plot(pca_pib_result, type = "l", main = "Scree Plot de Componentes Principales")

# Extraer los componentes necesarios
datos_panel_pib$PC1 <- pca_pib_result$x[,1]   
datos_panel_pib$PC2 <- pca_pib_result$x[,2]  
datos_panel_pib$PC3 <- pca_pib_result$x[,3]  

# Ahora creamos unas variables dummy 
# Define el rango de años para los que quieres crear las dummies
años_para_dummies <- 2000:2022

# Crea una lista con tus dataframes para iterar sobre ellos
lista_de_dataframes <- list(
  datos_panel_log = datos_panel_log,
  datos_panel_log_diff = datos_panel_log_diff,
  datos_panel_pib = datos_panel_pib
)

# Itera sobre cada dataframe en la lista
for (nombre_df in names(lista_de_dataframes)) {
  df_actual <- get(nombre_df)
  for (año in años_para_dummies) {
    nombre_columna_dummy <- paste0("dummy_", año)
    df_actual[[nombre_columna_dummy]] <- ifelse(df_actual$año == año, 1, 0)
  }
  
  assign(nombre_df, df_actual, envir = .GlobalEnv)
}

# Exportamos csv con los datos
write_csv(datos_panel_log, file = "Datos log.csv")
write_csv(datos_panel_log_diff, file = "Datos log_diff.csv")
write_csv(datos_panel_pib, file = "Datos pib.csv")



#===============================================================================
#===============================================================================
# COMENZAMOS CON LOS MODELOS, AQUÍ LOS CABALLOS
#===============================================================================
#===============================================================================
#===============================================================================
# MODELO Nº1: PIB CONTRA COMPONENTES DEL GASTO
#===============================================================================
# Modelo  
modelo_1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                  ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                data = datos_panel_log, 
                model = "within")
summary(modelo_1)
bptest(modelo_1)
pbgtest(modelo_1)
shapiro.test(residuals(modelo_1))
summary(modelo_1, vcov = vcovHC(modelo_1, cluster = "group"))

#===============================================================================
# MODELO Nº2: PIB CONTRA COMPONENTES DEL GASTO Y DUMMIES
#===============================================================================
# Modelo  
modelo_2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                  ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental +
                  dummy_2008 + dummy_2020, 
                data = datos_panel_log, 
                model = "within")
summary(modelo_2)
bptest(modelo_2)
pbgtest(modelo_2)
shapiro.test(residuals(modelo_2))
summary(modelo_2, vcov = vcovHC(modelo_2, cluster = "group"))

#===============================================================================
# MODELO Nº3: PIB CONTRA COMPONENTES DEL GASTO Y DUMMIES (ECC)
#===============================================================================
# Modelo 
modelo_3 <- pcce(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                  ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                data = datos_panel_log, 
                model = "p")
summary(modelo_3.1)
bptest(modelo_3.1)
pbgtest(modelo_3.1)
shapiro.test(residuals(modelo_3.1))
summary(modelo_3.1, vcov = vcovHC(modelo_3.1, cluster = "group"))

#===============================================================================
# MODELO Nº4: PIB CONTRA COMPONENTES DEL GASTO E INTERACCIONES DE CALIDAD
#===============================================================================
# Modelos control de corrupción
modelo_4.1.1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                  ln_Gasto_Seguridad_Admin*Control.de.Corrupción + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                data = datos_panel_log, 
                model = "within")
summary(modelo_4.1.1)
bptest(modelo_4.1.1)
pbgtest(modelo_4.1.1)
shapiro.test(residuals(modelo_4.1.1))
summary(modelo_4.1.1, vcov = vcovHC(modelo_4.1.1, cluster = "group"))

modelo_4.1.2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Control.de.Corrupción + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.1.2)
bptest(modelo_4.1.2)
pbgtest(modelo_4.1.2)
shapiro.test(residuals(modelo_4.1.2))
summary(modelo_4.1.2, vcov = vcovHC(modelo_4.1.2, cluster = "group"))

modelo_4.1.3 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Control.de.Corrupción, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.1.3)
bptest(modelo_4.1.3)
pbgtest(modelo_4.1.3)
shapiro.test(residuals(modelo_4.1.3))
summary(modelo_4.1.3, vcov = vcovHC(modelo_4.1.3, cluster = "group"))

# Modelos efectividad gubernamental
modelo_4.2.1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin*Efectividad.Gubernamental + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.2.1)
bptest(modelo_4.2.1)
pbgtest(modelo_4.2.1)
shapiro.test(residuals(modelo_4.2.1))
summary(modelo_4.2.1, vcov = vcovHC(modelo_4.2.1, cluster = "group"))

modelo_4.2.2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Efectividad.Gubernamental + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.2.2)
bptest(modelo_4.2.2)
pbgtest(modelo_4.2.2)
shapiro.test(residuals(modelo_4.2.2))
summary(modelo_4.2.2, vcov = vcovHC(modelo_4.2.2, cluster = "group"))

modelo_4.2.3 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Efectividad.Gubernamental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.2.3)
bptest(modelo_4.2.3)
pbgtest(modelo_4.2.3)
shapiro.test(residuals(modelo_4.2.3))
summary(modelo_4.2.3, vcov = vcovHC(modelo_4.2.3, cluster = "group"))

# Modelos estabilidad política
modelo_4.3.1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin*Estabilidad.Política.y.Ausencia.de.Violencia + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.3.1)
bptest(modelo_4.3.1)
pbgtest(modelo_4.3.1)
shapiro.test(residuals(modelo_4.3.1))
summary(modelo_4.3.1, vcov = vcovHC(modelo_4.3.1, cluster = "group"))

modelo_4.3.2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Estabilidad.Política.y.Ausencia.de.Violencia + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.3.2)
bptest(modelo_4.3.2)
pbgtest(modelo_4.3.2)
shapiro.test(residuals(modelo_4.3.2))
summary(modelo_4.3.2, vcov = vcovHC(modelo_4.3.2, cluster = "group"))

modelo_4.3.3 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Estabilidad.Política.y.Ausencia.de.Violencia, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.3.3)
bptest(modelo_4.3.3)
pbgtest(modelo_4.3.3)
shapiro.test(residuals(modelo_4.3.3))
summary(modelo_4.3.3, vcov = vcovHC(modelo_4.3.3, cluster = "group"))

# Modelos estado de derecho
modelo_4.4.1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin*Estado.de.Derecho + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.4.1)
bptest(modelo_4.4.1)
pbgtest(modelo_4.4.1)
shapiro.test(residuals(modelo_4.4.1))
summary(modelo_4.4.1, vcov = vcovHC(modelo_4.4.1, cluster = "group"))

modelo_4.4.2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Estado.de.Derecho + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.4.2)
bptest(modelo_4.4.2)
pbgtest(modelo_4.4.2)
shapiro.test(residuals(modelo_4.4.2))
summary(modelo_4.4.2, vcov = vcovHC(modelo_4.4.2, cluster = "group"))

modelo_4.4.3 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Estado.de.Derecho, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.4.3)
bptest(modelo_4.4.3)
pbgtest(modelo_4.4.3)
shapiro.test(residuals(modelo_4.4.3))
summary(modelo_4.4.3, vcov = vcovHC(modelo_4.4.3, cluster = "group"))

# Modelos calidad regulatoria
modelo_4.5.1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin*Calidad.Regulatoria + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.5.1)
bptest(modelo_4.5.1)
pbgtest(modelo_4.5.1)
shapiro.test(residuals(modelo_4.5.1))
summary(modelo_4.5.1, vcov = vcovHC(modelo_4.5.1, cluster = "group"))

modelo_4.5.2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Calidad.Regulatoria + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.5.2)
bptest(modelo_4.5.2)
pbgtest(modelo_4.5.2)
shapiro.test(residuals(modelo_4.5.2))
summary(modelo_4.5.2, vcov = vcovHC(modelo_4.5.2, cluster = "group"))

modelo_4.5.3 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Calidad.Regulatoria, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.5.3)
bptest(modelo_4.5.3)
pbgtest(modelo_4.5.3)
shapiro.test(residuals(modelo_4.5.3))
summary(modelo_4.5.3, vcov = vcovHC(modelo_4.5.3, cluster = "group"))

# Modelos voz y rendición
modelo_4.6.1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin*Voz.y.Rendición.de.Cuentas + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.6.1)
bptest(modelo_4.6.1)
pbgtest(modelo_4.6.1)
shapiro.test(residuals(modelo_4.6.1))
summary(modelo_4.6.1, vcov = vcovHC(modelo_4.6.1, cluster = "group"))

modelo_4.6.2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Voz.y.Rendición.de.Cuentas + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.6.2)
bptest(modelo_4.6.2)
pbgtest(modelo_4.6.2)
shapiro.test(residuals(modelo_4.6.2))
summary(modelo_4.6.2, vcov = vcovHC(modelo_4.6.2, cluster = "group"))

modelo_4.6.3 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Voz.y.Rendición.de.Cuentas, 
                    data = datos_panel_log, 
                    model = "within")
summary(modelo_4.6.3)
bptest(modelo_4.6.3)
pbgtest(modelo_4.6.3)
shapiro.test(residuals(modelo_4.6.3))
summary(modelo_4.6.3, vcov = vcovHC(modelo_4.6.3, cluster = "group"))

#===============================================================================
# MODELO Nº5: PIB CONTRA COMPONENTES DEL GASTO POR REGIÓN
#===============================================================================
# Modelo IDH muy alto:
datos_IDH_muy_alto  <- datos_imputados_log %>%
  filter(país %in% muy_alto)

datos_panel_IDH_muy_alto <- pdata.frame(datos_IDH_muy_alto, index = c("país", "año"))

modelo_5.1.1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_IDH_muy_alto, 
                    model = "within")
summary(modelo_5.1.1)
bptest(modelo_5.1.1)
pbgtest(modelo_5.1.1)
shapiro.test(residuals(modelo_5.1.1))
summary(modelo_5.1.1, vcov = vcovHC(modelo_5.1.1, cluster = "group"))

# Modelo IDH alto:
datos_IDH_alto  <- datos_imputados_log %>%
  filter(país %in% alto)

datos_panel_IDH_alto <- pdata.frame(datos_IDH_alto, index = c("país", "año"))

modelo_5.1.2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_IDH_alto, 
                    model = "within")
summary(modelo_5.1.2)
bptest(modelo_5.1.2)
pbgtest(modelo_5.1.2)
shapiro.test(residuals(modelo_5.1.2))
summary(modelo_5.1.2, vcov = vcovHC(modelo_5.1.2, cluster = "group"))

# Modelo IDH medio y bajo:
datos_IDH_medio_bajo  <- datos_imputados_log %>%
  filter(país %in% medio_bajo)

datos_panel_IDH_medio_bajo <- pdata.frame(datos_IDH_medio_bajo, index = c("país", "año"))

modelo_5.1.3 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_IDH_medio_bajo, 
                    model = "within")
summary(modelo_5.1.3)
bptest(modelo_5.1.3)
pbgtest(modelo_5.1.3)
shapiro.test(residuals(modelo_5.1.3))
summary(modelo_5.1.3, vcov = vcovHC(modelo_5.1.3, cluster = "group"))

# Separamos por continente:
america <- c(
  "Estados Unidos", "Canadá", "Cuba", "Jamaica", "Perú",
  "Paraguay", "Brasil", "Guatemala", "El Salvador", "Bolivia",
  "Nicaragua"
)

europa <- c(
  "Alemania", "Austria", "Bélgica", "Chipre", "Croacia",
  "Dinamarca", "Eslovenia", "España", "Estonia", "Finlandia",
  "Francia", "Irlanda", "Islandia", "Italia", "Letonia",
  "Bulgaria", "Ucrania", "Albania", "Azerbaiyán", "Hungría", "Grecia"
)

asia_oceania <- c(
  "Japón", "Líbano", "Filipinas", "Indonesia", "China",
  "Jordania", "Sri Lanka", "Kirguistán", "Nepal", "Bután",
  "Bangladés", "Pakistán", "India", "Yemen", "Armenia", "Islas Salomón"
)

africa <- c(
  "Sudáfrica", "Mauricio", "Egipto", "Namibia", "Uganda",
  "Madagascar", "Angola", "Kenia", "Zambia", "Etiopía",
  "Cabo Verde", "Congo"
)

# Modelo América
datos_america  <- datos_imputados_log %>%
  filter(país %in% america)

datos_panel_america <- pdata.frame(datos_america, index = c("país", "año"))

modelo_5.2.1 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_america, 
                    model = "within")
summary(modelo_5.2.1)
bptest(modelo_5.2.1)
pbgtest(modelo_5.2.1)
shapiro.test(residuals(modelo_5.2.1))
summary(modelo_5.2.1, vcov = vcovHC(modelo_5.2.1, cluster = "group"))

# Modelo Europa
datos_europa  <- datos_imputados_log %>%
  filter(país %in% europa)

datos_panel_europa <- pdata.frame(datos_europa, index = c("país", "año"))

modelo_5.2.2 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_europa, 
                    model = "within")
summary(modelo_5.2.2)
bptest(modelo_5.2.2)
pbgtest(modelo_5.2.2)
shapiro.test(residuals(modelo_5.2.2))
summary(modelo_5.2.2, vcov = vcovHC(modelo_5.2.2, cluster = "group"))

# Modelo Asia y Oceanía
datos_asia_oceania  <- datos_imputados_log %>%
  filter(país %in% asia_oceania)

datos_panel_asia_oceania <- pdata.frame(datos_asia_oceania, index = c("país", "año"))

modelo_5.2.3 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_asia_oceania, 
                    model = "within")
summary(modelo_5.2.3)
bptest(modelo_5.2.3)
pbgtest(modelo_5.2.3)
shapiro.test(residuals(modelo_5.2.3))
summary(modelo_5.2.3, vcov = vcovHC(modelo_5.2.3, cluster = "group"))

# Modelo África
datos_africa  <- datos_imputados_log %>%
  filter(país %in% africa)

datos_panel_africa <- pdata.frame(datos_africa, index = c("país", "año"))

modelo_5.2.4 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                      ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental, 
                    data = datos_panel_africa, 
                    model = "within")
summary(modelo_5.2.4)
bptest(modelo_5.2.4)
pbgtest(modelo_5.2.4)
shapiro.test(residuals(modelo_5.2.4))
summary(modelo_5.2.4, vcov = vcovHC(modelo_5.2.4, cluster = "group"))

#===============================================================================
# MODELO Nº6: MODELOS SEGMENTADOS
#===============================================================================
# Ajustar un modelo lm "simple" ignorando la estructura de panel (para que funcione segmented)
modelo_6 <- lm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo +
                         ln_Comercio + ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social +
                         ln_Gasto_Economico_Ambiental,
                       data = datos_panel_log)

# Aplicar segmented() a cada variable de gasto por separado
# Segmentación para ln_Gasto_Seguridad_Admin
# Asumiendo un quiebre, con psi = mediana
modelo_6.1 <- segmented(modelo_6,
                       seg.Z = ~ ln_Gasto_Seguridad_Admin,
                       psi = list(ln_Gasto_Seguridad_Admin = median(datos_panel_log$ln_Gasto_Seguridad_Admin, na.rm = TRUE)))
summary(modelo_6.1)
plot(modelo_6.1)
bptest(modelo_6.1)
shapiro.test(residuals(modelo_6.1))
summary(modelo_6.1, vcov = vcovHC(modelo_6.1, cluster = "group"))

# Segmentación para ln_Gasto_Desarrollo_Social
modelo_6.2 <- segmented(modelo_6,
                        seg.Z = ~ ln_Gasto_Desarrollo_Social,
                        psi = list(ln_Gasto_Desarrollo_Social = median(datos_panel_log$ln_Gasto_Desarrollo_Social, na.rm = TRUE)))
summary(modelo_6.2)
plot(modelo_6.2)
bptest(modelo_6.2)
shapiro.test(residuals(modelo_6.2))
summary(modelo_6.2, vcov = vcovHC(modelo_6.2, cluster = "group"))

# Segmentación para ln_Gasto_Economico_Ambiental
modelo_6.3 <- segmented(modelo_6,
                         seg.Z = ~ ln_Gasto_Economico_Ambiental,
                         psi = list(ln_Gasto_Economico_Ambiental = median(datos_panel_log$ln_Gasto_Economico_Ambiental, na.rm = TRUE)))
summary(modelo_6.3)
plot(modelo_6.3)
bptest(modelo_6.3)
shapiro.test(residuals(modelo_6.3))
summary(modelo_6.3, vcov = vcovHC(modelo_6.3, cluster = "group"))

#===============================================================================
# MODELO Nº7: MODELO ARELLANO BOND
#===============================================================================
modelo_7 <- pgmm(d_ln_pib ~ lag(d_ln_pib, 1) + 
                           d_ln_Consumo.final.de.los.hogares +
                           d_ln_Formación.bruta.de.capital.fijo +
                           d_ln_Comercio +
                           Gasto_Seguridad_Admin_diff +
                           Gasto_Desarrollo_Social_diff +
                           Gasto_Economico_Ambiental_diff | 
                           lag(d_ln_pib, 2), 
                         data = datos_panel_log_diff,
                         effect = "individual", 
                         model = "onestep", 
                         transformation = "ld", 
                         collapse = TRUE, 
                         lag.gmm = list(c(2,3)) 
)

summary(modelo_7)
summary(modelo_7, vcov = vcovHC(modelo_7, cluster = "group"))
sargan_hansen_test <- sargan(modelo_7)
print(sargan_hansen_test)
arellano_bond_test_ar1 <- mtest(modelo_7, order = 1, type = "ar")
print(arellano_bond_test_ar1)
arellano_bond_test_ar2 <- mtest(modelo_7, order = 2, type = "ar")
print(arellano_bond_test_ar2)

#===============================================================================
# MODELO Nº8: PCA LOG
#===============================================================================
# Modelo 8 (este no sé porque en el primer PCA hay básicamente todo)
summary(pca_log_result)
print(loadings_log)
modelo_8 <- plm(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo +
                      ln_Comercio + PC1 + PC2 + PC3, 
                    data = datos_panel_log, 
                    model = "within")

summary(modelo_8)
bptest(modelo_8)
pbgtest(modelo_8)
shapiro.test(residuals(modelo_8))
summary(modelo_8, vcov = vcovHC(modelo_8, cluster = "group"))

#===============================================================================
# MODELO Nº9: PCA LOG DIFF
#===============================================================================ç
# Modelo 9 (este tiene muchos PCA)
summary(pca_diff_result)
print(loadings_diff)
modelo_9 <- plm(d_ln_pib ~ d_ln_Consumo.final.de.los.hogares + d_ln_Formación.bruta.de.capital.fijo +
                  d_ln_Comercio + PC1 + PC2 + PC3 + PC4 + PC5,
                data = datos_panel_log_diff, 
                model = "within")

summary(modelo_9)
bptest(modelo_9)
pbgtest(modelo_9)
shapiro.test(residuals(modelo_9))
summary(modelo_9, vcov = vcovHC(modelo_9, cluster = "group"))


#===============================================================================
# MODELO Nº10: PIB CONTRA COMPONENTES DEL GASTO (FGLS)
#===============================================================================
# Modelo  
modelo_1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                          ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                        data = datos_panel_log,
                        model = "within", 
                        effect = "individual" 
)

summary(modelo_1fgls)
bptest(modelo_1fgls)
pwartest(modelo_1fgls)
shapiro.test(residuals(modelo_1fgls))


#===============================================================================
# MODELO Nº11: PIB CONTRA COMPONENTES DEL GASTO Y DUMMIES (FGLS)
#===============================================================================
# Modelo  
modelo_2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                        ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental +
                        dummy_2008 + dummy_2020,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_2fgls)
bptest(modelo_2fgls)
pwartest(modelo_2fgls)
shapiro.test(residuals(modelo_2fgls))

#===============================================================================
# MODELO Nº12: PIB CONTRA COMPONENTES DEL GASTO E INTERACCIONES DE CALIDAD (FGLS)
#===============================================================================
# Modelos control de corrupción
modelo_4.1.1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                        ln_Gasto_Seguridad_Admin*Control.de.Corrupción + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_4.1.1fgls)
bptest(modelo_4.1.1fgls)
pwartest(modelo_4.1.1fgls)
shapiro.test(residuals(modelo_4.1.1fgls))

modelo_4.1.2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Control.de.Corrupción + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.1.2fgls)
bptest(modelo_4.1.2fgls)
pwartest(modelo_4.1.2fgls)
shapiro.test(residuals(modelo_4.1.2fgls))

modelo_4.1.3fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Control.de.Corrupción,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.1.3fgls)
bptest(modelo_4.1.3fgls)
pwartest(modelo_4.1.3fgls)
shapiro.test(residuals(modelo_4.1.3fgls))


# Modelos efectividad gubernamental
modelo_4.2.1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin*Efectividad.Gubernamental + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.2.1fgls)
bptest(modelo_4.2.1fgls)
pwartest(modelo_4.2.1fgls)
shapiro.test(residuals(modelo_4.2.1fgls))

modelo_4.2.2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                        ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Efectividad.Gubernamental + ln_Gasto_Economico_Ambiental,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_4.2.2fgls)
bptest(modelo_4.2.2fgls)
pwartest(modelo_4.2.2fgls)
shapiro.test(residuals(modelo_4.2.2fgls))

modelo_4.2.3fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Efectividad.Gubernamental,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.2.3fgls)
bptest(modelo_4.2.3fgls)
pwartest(modelo_4.2.3fgls)
shapiro.test(residuals(modelo_4.2.3fgls))

# Modelos estabilidad política
modelo_4.3.1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                        ln_Gasto_Seguridad_Admin*Estabilidad.Política.y.Ausencia.de.Violencia + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_4.3.1fgls)
bptest(modelo_4.3.1fgls)
pwartest(modelo_4.3.1fgls)
shapiro.test(residuals(modelo_4.3.1fgls))

modelo_4.3.2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                        ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Estabilidad.Política.y.Ausencia.de.Violencia + ln_Gasto_Economico_Ambiental,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_4.3.2fgls)
bptest(modelo_4.3.2fgls)
pwartest(modelo_4.3.2fgls)
shapiro.test(residuals(modelo_4.3.2fgls))

modelo_4.3.3fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                        ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Estabilidad.Política.y.Ausencia.de.Violencia,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_4.3.3fgls)
bptest(modelo_4.3.3fgls)
pwartest(modelo_4.3.3fgls)
shapiro.test(residuals(modelo_4.3.3fgls))

# Modelos estado de derecho
modelo_4.4.1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin*Estado.de.Derecho + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_4.4.1fgls)
bptest(modelo_4.4.1fgls)
pwartest(modelo_4.4.1fgls)
shapiro.test(residuals(modelo_4.4.1fgls))

modelo_4.4.2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Estado.de.Derecho + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.4.2fgls)
bptest(modelo_4.4.2fgls)
pwartest(modelo_4.4.2fgls)
shapiro.test(residuals(modelo_4.4.2fgls))

modelo_4.4.3fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Estado.de.Derecho,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.4.3fgls)
bptest(modelo_4.4.3fgls)
pwartest(modelo_4.4.3fgls)
shapiro.test(residuals(modelo_4.4.3fgls))

# Modelos calidad regulatoria
modelo_4.5.1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin*Calidad.Regulatoria + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.5.1fgls)
bptest(modelo_4.5.1fgls)
pwartest(modelo_4.5.1fgls)
shapiro.test(residuals(modelo_4.5.1fgls))

modelo_4.5.2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                        ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Calidad.Regulatoria + ln_Gasto_Economico_Ambiental,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_4.5.2fgls)
bptest(modelo_4.5.2fgls)
pwartest(modelo_4.5.2fgls)
shapiro.test(residuals(modelo_4.5.2fgls))

modelo_4.5.3fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Calidad.Regulatoria,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.5.3fgls)
bptest(modelo_4.5.3fgls)
pwartest(modelo_4.5.3fgls)
shapiro.test(residuals(modelo_4.5.3fgls))

# Modelos voz y rendición de cuentas
modelo_4.6.1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin*Voz.y.Rendición.de.Cuentas + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.6.1fgls)
bptest(modelo_4.6.1fgls)
pwartest(modelo_4.6.1fgls)
shapiro.test(residuals(modelo_4.6.1fgls))

modelo_4.6.2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social*Voz.y.Rendición.de.Cuentas + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_4.6.2fgls)
bptest(modelo_4.6.2fgls)
pwartest(modelo_4.6.2fgls)
shapiro.test(residuals(modelo_4.6.2fgls))

modelo_4.6.3fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental*Voz.y.Rendición.de.Cuentas,
                      data = datos_panel_log,
                      model = "within", 
                      effect = "individual" 
)

summary(modelo_4.6.3fgls)
bptest(modelo_4.6.3fgls)
pwartest(modelo_4.6.3fgls)
shapiro.test(residuals(modelo_4.6.3fgls))

#===============================================================================
# MODELO Nº13: PIB CONTRA COMPONENTES DEL GASTO POR REGIÓN (FGLS)
#===============================================================================
# Modelo IDH muy alto:
modelo_5.1.1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_IDH_muy_alto,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_5.1.1fgls)
bptest(modelo_5.1.1fgls)
pwartest(modelo_5.1.1fgls)
shapiro.test(residuals(modelo_5.1.1fgls))

# Modelo IDH alto:
modelo_5.1.2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_IDH_alto,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_5.1.2fgls)
bptest(modelo_5.1.2fgls)
pwartest(modelo_5.1.2fgls)
shapiro.test(residuals(modelo_5.1.2fgls))

# Modelo IDH medio y bajo:
modelo_5.1.3fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_IDH_medio_bajo,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_5.1.3fgls)
bptest(modelo_5.1.3fgls)
pwartest(modelo_5.1.3fgls)
shapiro.test(residuals(modelo_5.1.3fgls))

# Modelo América
modelo_5.2.1fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_america,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_5.2.1fgls)
bptest(modelo_5.2.1fgls)
pwartest(modelo_5.1.1fgls)
shapiro.test(residuals(modelo_5.2.1fgls))

# Modelo Europa
modelo_5.2.2fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_europa,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_5.2.2fgls)
bptest(modelo_5.2.2fgls)
pwartest(modelo_5.2.2fgls)
shapiro.test(residuals(modelo_5.2.2fgls))

# Modelo Asia y Oceanía
modelo_5.2.3fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_asia_oceania,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_5.2.3fgls)
bptest(modelo_5.2.3fgls)
pwartest(modelo_5.2.3fgls)
shapiro.test(residuals(modelo_5.2.3fgls))

# Modelo África
modelo_5.2.4fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo + ln_Comercio +
                            ln_Gasto_Seguridad_Admin + ln_Gasto_Desarrollo_Social + ln_Gasto_Economico_Ambiental,
                          data = datos_panel_africa,
                          model = "within", 
                          effect = "individual" 
)

summary(modelo_5.2.4fgls)
bptest(modelo_5.2.4fgls)
pwartest(modelo_5.2.4fgls)
shapiro.test(residuals(modelo_5.2.4fgls))

#===============================================================================
# MODELO Nº14: PCA LOG (FGLS)
#===============================================================================
# Modelo 
modelo_8fgls <- pggls(ln_pib ~ ln_Consumo.final.de.los.hogares + ln_Formación.bruta.de.capital.fijo +
                        ln_Comercio + PC1 + PC2 + PC3,
                          data = datos_panel_log,
                          model = "within", 
                          effect = "individual" 
)

# Resumen del modelo FGLS
summary(modelo_8fgls)
bptest(modelo_8fgls)
pwartest(modelo_8fgls)
shapiro.test(residuals(modelo_8fgls))

#===============================================================================
# MODELO Nº15: PCA LOG DIFF (FGLS)
#===============================================================================ç
# Modelo 9 (este tiene muchos PCA)
modelo_9fgls <- pggls(d_ln_pib ~ d_ln_Consumo.final.de.los.hogares + d_ln_Formación.bruta.de.capital.fijo +
                        d_ln_Comercio + PC1 + PC2 + PC3 + PC4 + PC5,,
                      data = datos_panel_log_diff,
                      model = "within", 
                      effect = "individual" 
)

# Resumen del modelo FGLS
summary(modelo_9fgls)
bptest(modelo_9fgls)
pwartest(modelo_9fgls)
shapiro.test(residuals(modelo_9fgls))





# Exportar base en nivel para entrega con WGI
library(writexl)
# Unificar datos con las nuevas variables
datos_TG <- datos_regresion_IDH %>% 
  left_join(INST_panel, by = c("país", "año"))

# Eliminar columna "Comercio"
datos_TG <- datos_TG %>%
  dplyr::select(-Comercio)

# Exportar excel 
write_xlsx(datos_TG, path = "datos_TG.xlsx") 
