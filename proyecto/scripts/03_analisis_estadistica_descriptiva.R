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
  summarise(
    across(
      all_of(variables_numericas),
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

# Migración 
grafico_migracion_inicial <- ggplot(data_raw, aes(x = SM.POP.NETM)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "steelblue",
    color = "black",
    alpha = 0.6
  ) +
  geom_density(
    color = "darkblue",
    linewidth = 1.2,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución de la Migración Neta",
    subtitle = "Histograma con curva de densidad (se observan fuertes outliers)",
    x = "Migración Neta (personas)",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(dir_output_figures, "distribucion_migracion_inicial.png"),
  plot = grafico_migracion_inicial,
  width = 10, height = 6, dpi = 300
)

#HABLAR DE PRESENCIA DE OUTLIERS< POR ESO SALE DESFASADO

# Desempleo joven entre 15 y 24
grafico_desempleo_joven <- ggplot(data_raw, aes(y = SL.UEM.1524.ZS, x = "")) +
  geom_boxplot(fill = "salmon", alpha = 0.4, na.rm = TRUE) +
  labs(
    title = "Distribución del Desempleo Juvenil 15-24 (%)",
    y = "Desempleo Juvenil (%)",
    x = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background  = element_rect(fill = "white")
  )

ggsave(
  filename = file.path(dir_output_figures, "desempleo_joven_inicial.png"),
  plot = grafico_desempleo_joven,
  width = 10, height = 6, dpi = 300
)


# Relación migración y desempleo joven

grafico_migracion_vs_desempleo <- ggplot(
  data_raw,
  aes(x = SL.UEM.1524.ZS, y = SM.POP.NETM)
) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "loess", color = "blue") +
  labs(
    title = "Migración Neta vs Desempleo Juvenil 15–24",
    x = "Desempleo Juvenil (%)",
    y = "Migración Neta (personas)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave(
  filename = file.path(dir_output_figures, "relacion_migracion_desempleo_joven.png"),
  plot = grafico_migracion_vs_desempleo,
  width = 10, height = 6, dpi = 300
)


# Anualmente
resumen_anual <- data_raw %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    mig_media = mean(SM.POP.NETM, na.rm = TRUE),
    mig_sd    = sd(SM.POP.NETM, na.rm = TRUE),
    desempleo_media = mean(SL.UEM.1524.ZS, na.rm = TRUE),
    desempleo_sd    = sd(SL.UEM.1524.ZS, na.rm = TRUE)
  ) %>%
  arrange(year)

print(resumen_anual)


