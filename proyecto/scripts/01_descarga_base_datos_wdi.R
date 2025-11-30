#DESCARGA DE DATOS WDI
#-------------------------------------------------------------------------------

source(here::here("configuracion", "parametros.R"))


# Desagresar las regiones, para poder ver por pais
paises <- WDI::WDI_data$country %>% 
  filter(region != "Aggregates") %>% 
  pull(iso2c)

# Indicadores necesarios para el proyecto
indicadores_raw <- c(
  "country",                   # Nombre del país
  "iso2c",                     
  "iso3c",                    
  "year", 
  "SM.POP.NETM",             # Migracion NETA
  "SP.POP.1524.TO.ZS",       # Poblacion entre 15 y 24
  "SL.UEM.TOTL.ZS",          # Indice de desempleados
  "SL.UEM.1524.ZS",          # Desempleo joven entre 15 y 24
  "NY.GDP.PCAP.PP.CD",       # PBI per capita
  "FP.CPI.TOTL.ZG",          # Inflacion
  "SI.POV.DDAY",             # Indice de pobreza
  "SP.POP.TOTL",             # Poblacion total por pais
  "SP.URB.TOTL.IN.ZS"        # % urbanizacion
  )

# Descargar todo
data_raw <- WDI(
  country = paises,
  indicator = indicadores_raw,
  extra = FALSE
)

# Guardar archivo
write_csv(data_raw, file.path(dir_data_raw, "data_raw_wdi.csv"))
          