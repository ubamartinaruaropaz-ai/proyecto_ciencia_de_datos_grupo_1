# OUTLIERS
#-------------------------------------------------------------------------------

source(here::here("configuracion", "parametros.R"))
data_raw <- read_csv("data/raw/data_raw_wdi.csv")

# Seleccionamos los 3 años de interés (ejemplo: 2000, 2010, 2020)
anios_interes <- c(2000, 2010, 2020)
data_filtrada <- data_raw %>% 
  filter(year %in% anios_interes)

skimr::skim(data_filtrada)
naniar::miss_var_summary(data_filtrada)
naniar::vis_miss(data_filtrada)

# Detectar outliers usando el método de Tukey (IQR)
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5*IQR
  upper <- Q3 + 1.5*IQR
  return(which(x < lower | x > upper))
}

# Variables continuas relevantes
vars_continuas <- c("SM.POP.NETM", "SL.UEM.1524.ZS", 
                    "NY.GDP.PCAP.PP.CD", "FP.CPI.TOTL.ZG", "SI.POV.DDAY")

outliers_lista <- lapply(data_filtrada[vars_continuas], detect_outliers)
outliers <- sapply(outliers_lista, length)
outliers

# Variables continuas incluyendo población joven
vars_continuas <- c("SM.POP.NETM", "SP.POP.1524.TO.ZS", 
                    "SL.UEM.1524.ZS", "NY.GDP.PCAP.PP.CD", 
                    "FP.CPI.TOTL.ZG", "SI.POV.DDAY")

# Imputación de outliers y datos faltantes
data_limpia <- data_filtrada
for (var in vars_continuas) {
  # Detectar outliers
  idx_out <- detect_outliers(data_limpia[[var]])
  if(length(idx_out) > 0){      
    data_limpia[[var]][idx_out] <- NA  # marcar como NA
  }
  # Imputar NA con mediana
  mediana <- median(data_limpia[[var]], na.rm = TRUE)
  data_limpia[[var]][is.na(data_limpia[[var]])] <- mediana
}

# Guardar archivo
write_csv(data_limpia, file.path(dir_data_clean, "data_clean_wdi.csv"))

