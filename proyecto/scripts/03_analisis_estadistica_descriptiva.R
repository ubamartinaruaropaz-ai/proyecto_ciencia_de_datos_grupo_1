# ESTADÍSTICA DESCRIPTIVAS
#-------------------------------------------------------------------------------

source(here::here("configuracion", "parametros.R"))
data_raw <- read_csv("data/raw/data_raw_wdi.csv")

#Determinar variables
variables_numericas <- data_raw %>%
  select(where(is.numeric)) %>%
  colnames()

# Estadísticas para var numéricas
desc_stats <- data_raw %>%
  select(all_of(variables_numericas)) %>%
  summarise(
    across(
      everything(),
      list(
        media   = ~mean(., na.rm = TRUE),
        mediana = ~median(., na.rm = TRUE),
        sd      = ~sd(., na.rm = TRUE),
        IQR     = ~IQR(., na.rm = TRUE),
        min     = ~min(., na.rm = TRUE),
        max     = ~max(., na.rm = TRUE)
      ),
      .names = "{col}_{fn}"
    )
  )

print(desc_stats)


#Histograma & Boxplot

#Migración
grafico_migracion_inicial <- ggplot(data_raw, aes(x = SM.POP.NETM)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "black") +
  geom_boxplot(aes(x = 0, y = SM.POP.NETM), width = 0.2, alpha = 0.3,
               position = position_nudge(x = 0)) +
  labs(title = "Distribución de Migración Neta", x = "Migración Neta", y = "Frecuencia",
       caption = "Fuente: World Bank (WDI)") +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave(
  filename = file.path(dir_output_figures, "distribucion_migracion_neta_inicial.png"),
  plot = grafico_migracion_inicial,      
  width = 10,              
  height = 6,              
  dpi = 300                
)

# Población joven entre 15 y 24
grafico_distribucion_poblacion_joven_inicial <-ggplot(data_raw, aes(x = 1, y = SL.UEM.1524.ZS)) +
  geom_boxplot(width = 0.2, fill = "salmon", alpha = 0.3, na.rm = TRUE) +
  labs(title = "Boxplot de Desempleo Juvenil 15-24 (%)",
       x = "", y = "Desempleo Juvenil (%)",
       caption = "Fuente: World Bank (WDI)") +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave(
  filename = file.path(dir_output_figures, "distribucion_poblacion_joven_inicial.png"),
  plot = grafico_distribucion_poblacion_joven_inicial,      
  width = 10,              
  height = 6,              
  dpi = 300                
)


# Relación migración y población joven

grafico_relacion_migracion_joven <-ggplot(data_raw, aes(x = 1, y = SL.UEM.1524.ZS)) +
  ggplot(data_raw, aes(x = SP.POP.1524.TO.ZS, y = SM.POP.NETM)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Migración Neta vs Población Joven 15-24 (%)",
       x = "Población Joven (%)",
       y = "Migración Neta",
       caption = "Fuente: World Bank (WDI)") +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"))

ggsave(
  filename = file.path(dir_output_figures, "relacion_migracion_joven_inicial.png"),
  plot = grafico_relacion_migracion_joven,      
  width = 10,              
  height = 6,              
  dpi = 300                
)


# -----------------------------
# 5. Resumen anual
# -----------------------------
resumen_anual <- data_raw %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    mig_media = mean(SM.POP.NETM, na.rm = TRUE),
    mig_sd = sd(SM.POP.NETM, na.rm = TRUE),
    pop15_24_media = mean(SP.POP.1524.TO.ZS, na.rm = TRUE),
    pop15_24_sd = sd(SP.POP.1524.TO.ZS, na.rm = TRUE),
    desempleo_media = mean(SL.UEM.1524.ZS, na.rm = TRUE),
    desempleo_sd = sd(SL.UEM.1524.ZS, na.rm = TRUE)
  ) %>%
  arrange(year)

print(resumen_anual)

# -----------------------------
# 6. Outliers extremos
# -----------------------------
outliers_mig <- data_raw %>%
  filter(SM.POP.NETM > quantile(SM.POP.NETM, 0.99, na.rm = TRUE) |
           SM.POP.NETM < quantile(SM.POP.NETM, 0.01, na.rm = TRUE)) %>%
  select(country, year, SM.POP.NETM)

print(outliers_mig)
#Distribuciones de variables importantes para la hipotesis
ggplot(data_raw, aes(x = SM.POP.NETM)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "black") +
  labs(title = "Migración Neta", x = "Migración Neta", y = "Frecuencia") +
  theme_minimal()

ggplot(data_raw, aes(x = SP.POP.1524.TO.ZS)) +
  geom_histogram(bins = 25, fill = "salmon", color = "black") +
  labs(title = "Población Joven 15-24 (%)", x = "Población Joven (%)", y = "Frecuencia") +
  theme_minimal()


#Relación migración y población joven
ggplot(data_raw, aes(x = SP.POP.1524.TO.ZS, y = SM.POP.NETM)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Migración Neta & Población Joven 15-24", 
       x = "Poblción Joven (%)", y = "Migración Neta") +
  theme_minimal()


# Anualmente
# -----------------------------
data_raw %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    mig_media = mean(SM.POP.NETM, na.rm = TRUE),
    pop15_24_media = mean(SP.POP.1524.TO.ZS, na.rm = TRUE),
    desempleo_media = mean(SL.UEM.1524.ZS, na.rm = TRUE)
  ) %>%
  arrange(year)

# Outliers extremos
data_raw %>%
  filter(SM.POP.NETM > quantile(SM.POP.NETM, 0.99, na.rm = TRUE) |
           SM.POP.NETM < quantile(SM.POP.NETM, 0.01, na.rm = TRUE)) %>%
  select(country, year, SM.POP.NETM)