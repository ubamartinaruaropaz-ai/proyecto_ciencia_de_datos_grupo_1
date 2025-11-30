# REGRESION
#-------------------------------------------------------------------------------

source(here::here("configuracion", "parametros.R"))
data_limpia_filtrada <- read_csv("data/processed/data_filtrada_anios.csv")


# Variables dependiente y explicativas
y <- "SM.POP.NETM"               # Tasa de migración neta
x <- c( "SL.UEM.1524.ZS",         # Desempleo juvenil
       "SI.POV.DDAY",            # Pobreza
       "NY.GDP.PCAP.PP.CD",      # PBI per cápita
       "FP.CPI.TOTL.ZG")         # Inflación


# Modelo de regresion 
modelo <- lm(SM.POP.NETM~+SL.UEM.1524.ZS+SI.POV.DDAY+NY.GDP.PCAP.PP.CD+FP.CPI.TOTL.ZG, data = data_limpia_filtrada)

# Resumen del modelo
summary(modelo)

# Coeficientes con intervalos de confianza al 95%
confint(modelo)

# Supuestos
png(filename = file.path(dir_output_figures, "diagnostico_regresion.png"),
    width = 1200, height = 1000, res = 150)  # res = resolución
par(mfrow = c(2,2))
plot(modelo)
par(mfrow = c(1,1))
dev.off() 


# Valores de coeficientes y errores
coeficientes <- data.frame(
  variable = rownames(summary(modelo)$coefficients),
  estimate = summary(modelo)$coefficients[,1],
  std_error = summary(modelo)$coefficients[,2],
  t_value = summary(modelo)$coefficients[,3],
  p_value = summary(modelo)$coefficients[,4]
)

write_csv(coeficientes, file.path(dir_output_tables, "resultados_regresion_multiple.csv"))