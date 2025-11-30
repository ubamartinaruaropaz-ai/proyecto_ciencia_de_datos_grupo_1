#ANALISIS EXPLORATORIO DE DATOS
#-------------------------------------------------------------------------------

source(here::here("configuracion", "parametros.R"))


data_raw <- read_csv("data/raw/data_raw_wdi.csv")

#Analisis general

cat("Dimensiones:\n")
dim(data_raw)

cat("\nTipos de variables:\n")
str(data_raw)

# Revision filas
cat("\nPrimeras 6 filas:\n")
head(data_raw)

cat("\nNA por variable:\n")
na_counts <- sapply(data_raw, function(x) sum(is.na(x)))
print(na_counts)

# Detectar patrones de datos perdidos
na_summary <- data_raw %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing)) %>%
  mutate(pct_missing = n_missing / nrow(data_raw) * 100)

grafico_na <- ggplot(na_summary, aes(x = reorder(variable, pct_missing), y = pct_missing, fill = pct_missing)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "skyblue", high = "darkred", name = "% NA") +
  labs(
    title = "Patrones de valores faltantes en la base de datos WDI",
    subtitle = "Proporción de valores faltantes por variable",
    x = "Variable",
    y = "Porcentaje de NA",
    caption = "Fuente: World Bank (WDI) – Datos crudos descargados"
  ) +
  geom_text(aes(label = paste0(round(pct_missing, 1), "%")), 
            hjust = -0.1, size = 3) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

ggsave(
  filename = file.path(dir_output_figures, "valores_faltantes_en_base_inicial.png"),
  plot = grafico_na,      
  width = 10,              
  height = 6,              
  dpi = 300                
)

#Resumen estadístico
# -----------------------------
cat("\nResumen estadístico:\n")
skimr::skim(data_raw)