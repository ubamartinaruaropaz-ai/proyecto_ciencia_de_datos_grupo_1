# IMPACTO DE LIMPIEZA
#-------------------------------------------------------------------------------

source(here::here("configuracion", "parametros.R"))
data_raw <- read_csv("data/raw/data_raw_wdi.csv")
data_limpia <- read_csv("data/clean/data_clean_wdi.csv")


anios_interes <- c(2000, 2010, 2020)
data_raw_filtrada <- data_raw %>% filter(year %in% anios_interes)
data_limpia_filtrada <- data_limpia %>% filter(year %in% anios_interes)
write_csv(data_limpia_filtrada, file.path(dir_data_processed, "data_filtrada_anios.csv"))


# Variables continuas de interés
vars_continuas <- c("SM.POP.NETM",  
                    "SL.UEM.1524.ZS", "NY.GDP.PCAP.PP.CD", 
                    "FP.CPI.TOTL.ZG", "SI.POV.DDAY")

# Estadísticas originales
desc_original <- data_raw_filtrada %>%
  select(all_of(vars_continuas)) %>%
  summarise(
    across(everything(), list(
      n_missing = ~sum(is.na(.)),
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      p25 = ~quantile(., 0.25, na.rm = TRUE),
      p50 = ~median(., na.rm = TRUE),
      p75 = ~quantile(., 0.75, na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}")
  )

# Estadísticas descriptivas post limpieza
desc_limpia <- data_limpia_filtrada %>%
  select(all_of(vars_continuas)) %>%
  summarise(
    across(everything(), list(
      n_missing = ~sum(is.na(.)),
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      p25 = ~quantile(., 0.25, na.rm = TRUE),
      p50 = ~median(., na.rm = TRUE),
      p75 = ~quantile(., 0.75, na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}")
  )
# Comparación
comparacion <- bind_cols(desc_original, desc_limpia)


# Diferencias
diferencias <- tibble(
  variable = vars_continuas,
  cambio_media = sapply(vars_continuas, function(v) comparacion[[paste0(v, "_mean", "_2")]] - comparacion[[paste0(v, "_mean", "_1")]]),
  cambio_sd = sapply(vars_continuas, function(v) comparacion[[paste0(v, "_sd", "_2")]] - comparacion[[paste0(v, "_sd", "_1")]]),
  cambio_min = sapply(vars_continuas, function(v) comparacion[[paste0(v, "_min", "_2")]] - comparacion[[paste0(v, "_min", "_1")]]),
  cambio_max = sapply(vars_continuas, function(v) comparacion[[paste0(v, "_max", "_2")]] - comparacion[[paste0(v, "_max", "_1")]]),
  porcentaje_n_missing = sapply(vars_continuas, function(v) 
    (comparacion[[paste0(v, "_n_missing", "_1")]] - comparacion[[paste0(v, "_n_missing", "_2")]]) / nrow(data_raw_filtrada) * 100
  )
)

# Grafico
for (var in vars_continuas) {
  ggplot() +
    geom_boxplot(aes(x = "Original", y = data_raw_filtrada[[var]]), fill = "lightblue") +
    geom_boxplot(aes(x = "Limpia", y = data_limpia_filtrada[[var]]), fill = "salmon") +
    labs(title = paste("Comparación de Boxplot -", var), y = var, x = "") +
    theme_minimal() -> p
  print(p)
}

write_csv(comparacion, file.path(dir_output_tables, "comparacion_estadisticas_limpieza.csv"))