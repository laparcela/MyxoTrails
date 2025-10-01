library(tidyverse)
library(readr)

datos <- read_csv("groupsCategoriesProportion.csv")

datos_largos <- datos %>%
  pivot_longer(everything(), names_to = "Etiqueta", values_to = "Area") %>%
  drop_na()

datos_largos <- datos_largos %>%
  mutate(
    Replica = str_extract(Etiqueta, "R[1-4]"),
    Subcuadro = str_extract(Etiqueta, "_[1-3]"),
    Tiempo_original = str_extract(Etiqueta, "-\\d+h"),
    Tiempo_h = as.numeric(str_remove_all(Tiempo_original, "[-h]")),
    Submin = case_when(Subcuadro == "_1" ~ 0,
                       Subcuadro == "_2" ~ 45,
                       Subcuadro == "_3" ~ 90),
    Minuto_relativo = Tiempo_h * 60 + Submin
  )

limites <- c(0.0000, 0.0289, 0.0702, 0.1670, 0.3686, Inf)
categorias <- c(
  "Células individuales",
  "Pares o pequeños grupos",
  "Grupos pequeños-medianos",
  "Grupos medianos-grandes",
  "Grandes agregados"
)

datos_largos <- datos_largos %>%
  mutate(Categoria = cut(Area, breaks = limites, labels = categorias, include.lowest = TRUE))

proporciones <- datos_largos %>%
  group_by(Minuto_relativo, Replica, Categoria) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Minuto_relativo, Replica) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ungroup()

proporciones_resumen <- proporciones %>%
  group_by(Minuto_relativo, Categoria) %>%
  summarise(
    Promedio = mean(Proporcion),
    SD = sd(Proporcion),
    .groups = "drop"
  )

ggplot(proporciones_resumen, aes(x = Minuto_relativo, y = Promedio)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = Promedio - SD, ymax = Promedio + SD), alpha = 0.2, fill = "blue") +
  facet_wrap(~ Categoria, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Cambio en la proporción de agregados por categoría de tamaño",
    x = "Tiempo (minutos)",
    y = "Proporción de agregados"
  )


area_por_categoria <- datos_largos %>%
  group_by(Minuto_relativo, Replica, Categoria) %>%
  summarise(Area_categoria = sum(Area), .groups = "drop")

area_total <- datos_largos %>%
  group_by(Minuto_relativo, Replica) %>%
  summarise(Area_total = sum(Area), .groups = "drop")

area_proporcion <- area_por_categoria %>%
  left_join(area_total, by = c("Minuto_relativo", "Replica")) %>%
  mutate(Proporcion_area = Area_categoria / Area_total)

area_resumen <- area_proporcion %>%
  group_by(Minuto_relativo, Categoria) %>%
  summarise(
    Promedio = mean(Proporcion_area),
    SD = sd(Proporcion_area),
    .groups = "drop"
  )

ggplot(area_resumen, aes(x = Minuto_relativo, y = Promedio)) +
  geom_line(color = "darkgreen") +
  geom_ribbon(aes(ymin = Promedio - SD, ymax = Promedio + SD), alpha = 0.2, fill = "green") +
  facet_wrap(~ Categoria, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Proporción del área total ocupada por categoría (promedio entre réplicas)",
    x = "Tiempo (minutos)",
    y = "Proporción del área"
  )

ggplot(area_proporcion, aes(x = Minuto_relativo, y = Proporcion_area)) +
  geom_line() +
  facet_grid(Categoria ~ Replica, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Proporción del área ocupada por categoría por réplica",
    x = "Tiempo (minutos)",
    y = "Proporción del área"
  )

