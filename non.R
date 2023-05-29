#--------------------------------------#
#  Actividad 4: Análisis de senderos   #
#--------------------------------------#

## Carga paquetes
library(lavaan)
library(semPlot)
library(semTable)

# Parte A ----
## Importar datos (formato nativo R)
pnud13 <- readRDS("pnud-idh2015_rec.rds")

## Revisa los nombres de variables en los datos
sjPlot::view_df(pnud13)


## Explorar datos: distribución univariada
summary(pnud13)



## Especificar y estimar el modelo con efecto indirecto solamente
mpath4a <- 'legit_aut ~ trtinj
            ac_norm   ~ legit_aut'

## Ajustar modelo
path4a <- sem(mpath4a, data = pnud13)

## Ver resultados completos
summary(path4a, 
        fit.measures = T, # mostrar índices de ajuste extendidos
        standardized = T, # mostrar coeficientes estandarizados
        rsquare = T,  # mostrar r cuadrado
        modindices = T) # mostrar índices de modificación

## Exportar tablas
semTable(path4a, type = "html", 
         paramSets = c("slopes", "fits"),
         file = "resultados_actividad4a")

browseURL("resultados_actividad4.html") # Esta tabla puede copiarse y pegarse en procesador de texto

## Crear diagrama de sendero
## Gráfico con coeficientes estandarizados y opciones gráficas por defecto
semPaths(path4a, what = "std")

## Gráfico con ediciones de formato
semPaths(path4a, 
         what = "std", # mostrar coeficientes estandarizados
         residuals = FALSE, # no mostrar errores         
         sizeMan = 35, sizeMan2 = 12, # tamaño nodos
         weighted = F, label.scale = F, # opciones gráficas
         edge.label.cex = 1.6, label.cex = 1.6, # tamaño fuente o letra
         layout = "circle2",
         posCol = "black", negCol = "black", edge.color = "black", # color
  # Agregar etiquetas
         nodeLabels	= c("Legitimidad\nautoridades",
                        "Acción colectiva\nnormativa",
                        "Injusticia trato\nde autoridades"))

# Ver índices de modificación
modificationindices(path4a)

# Parte B ----
## Especifica el modelo agregando un efecto directo de injusticia a acción colectiva
## Tienes que reemplazar "______"
mpath4b <- '
    legit_aut ~ trtinj
    ac_norm   ~ trtinj + legit_aut'

## Ajustar modelo 4b
path4b <- sem(mpath4b, data = pnud13)

# Revisar ajuste del modelo y coeficientes
summary(path4b, fit.measures = T, standardized = T)

## Especifica y estima los nuevos parámetros de mediación y efecto total
mpath4b_med <- '
# Mismo modelo anterior
  legit_aut ~ a*trtinj
  ac_norm   ~ c*trtinj + b*legit_aut
# Definir nuevos parámetros
  injus_norm := a*b
  injus_norm_t := injus_norm + c'

path4bmed <- sem(mpath4b_med, data = pnud13)
summary(path4bmed, fit.measures = T, standardized = T)
