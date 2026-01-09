library(data.table)
library(tidyverse)
library(simmer)
library(simmer.plot)
library(dplyr)
library(caret)
library(purrr)


# Carregar funcions/dades del teu script
source("scripts/codi.R")

# -------------------------------------------------------------------------
# Crear la taula final amb totals anuals
# -------------------------------------------------------------------------

taula_final <- taula_resultat_completa %>%
  dplyr::select(Any, Domèstic_total, Activitats_total, Consum_total, Població) %>%
  group_by(Any) %>%
  summarise(
    Domèstic_total      = sum(Domèstic_total, na.rm = TRUE),
    Activitats_total    = sum(Activitats_total, na.rm = TRUE),
    Consum_total        = sum(Consum_total, na.rm = TRUE),
    Població_total      = sum(Població, na.rm = TRUE)
  )


# ========================================================================
#           GRÀFIC  Domèstic_total relativitzat al primer any
# ========================================================================

# Calculem Consum_per_capita
taula_a_estudiar <- taula_resultat_completa %>%
  mutate(Consum_per_capita = Domèstic_total/Població)

# Obtenir valor del primer any per cada regió
primer_any_valor <- taula_a_estudiar %>%
  group_by(Regio) %>%
  filter(Any == min(Any)) %>%
  dplyr::select(Regio, Consum_per_capita) %>%
  rename(Consum_per_capita_inicial = Consum_per_capita)

# Afegir valor inicial a la taula completa
taula_a_estudiar <- taula_a_estudiar %>%
  left_join(primer_any_valor, by = "Regio") %>%
  mutate(Consum_per_capita_index = Consum_per_capita / Consum_per_capita_inicial)

# Obtenir valor del primer any per cada regió
primer_any_valor <- taula_a_estudiar %>%
  group_by(Regio) %>%
  filter(Any == min(Any)) %>%
  dplyr::select(Regio, Activitats_total) %>%
  rename(Activitats_total_inicial = Activitats_total)

# Afegir valor inicial (2) a la taula completa
taula_a_estudiar <- taula_a_estudiar %>%
  left_join(primer_any_valor, by = "Regio") %>%
  mutate(Activitats_total_index = Activitats_total / Activitats_total_inicial)


# Obtenir valor del primer any per cada regió
primer_any_valor <- taula_a_estudiar %>%
  group_by(Regio) %>%
  filter(Any == min(Any)) %>%
  dplyr::select(Regio, Domèstic_total) %>%
  rename(Domèstic_total_inicial = Domèstic_total)

# Afegir valor inicial (2) a la taula completa
taula_a_estudiar <- taula_a_estudiar %>%
  left_join(primer_any_valor, by = "Regio") %>%
  mutate(Domèstic_total_index = Domèstic_total / Domèstic_total_inicial)


# -------------------------------------------------------------------------
# Gràfic amb índex (relatiu al primer any)
# -------------------------------------------------------------------------

ggplot(taula_a_estudiar,
       aes(
         x = Any,
         y = Consum_per_capita_index,
         color = Regio,
         group = Regio
       )) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua per capita total relatiu al primer any per regió",
    x = "Any",
    y = "Consum domestic/total (índex, primer any = 1)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_a_estudiar$Any)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )+ geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1.3,
    linetype = "dashed"
  )


# -------------------------------------------------------------------------
# Gràfic amb índex (relatiu al primer any) económic
# -------------------------------------------------------------------------

ggplot(taula_a_estudiar,
       aes(
         x = Any,
         y = Activitats_total_index,
         color = Regio,
         group = Regio
       )) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua economic per total relatiu al primer any per regió",
    x = "Any",
    y = "Consum economic/total (índex, primer any = 1)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_a_estudiar$Any)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )+ geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1.3,
    linetype = "dashed"
  )


# -------------------------------------------------------------------------
# Gràfic amb índex (relatiu al primer any) económic
# -------------------------------------------------------------------------

ggplot(taula_a_estudiar,
       aes(
         x = Any,
         y = Domèstic_total_index,
         color = Regio,
         group = Regio
       )) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Consum d'aigua domestic_total per total relatiu al primer any per regió",
    x = "Any",
    y = "Consum economic/total (índex, primer any = 1)",
    color = "Regió"
  ) +
  scale_x_continuous(breaks = unique(taula_a_estudiar$Any)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     limits = c(0, NA)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )+ geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1.3,
    linetype = "dashed"
  )

# -------------------------------------------------------------------------
# Contrast amb un test t-student
# -------------------------------------------------------------------------

#model_global_cap <- lm(Consum_per_capita_index ~ Any, data = taula_a_estudiar)
#summary(model_global)

#model_global_domestic <- lm(Domèstic_total_index ~ Any, data = taula_a_estudiar)
#summary(model_global)
regions_cat <- c(
  "taula_alt_pirineu_i_aran",
  "taula_girona",
  "taula_catcentral",
  "taula_ebre",
  "taula_camp_tarragona",
  "taula_ponent",
  "taula_metropolita"
)

llista_taules <- vector("list", length(regions_cat))
names(llista_taules) <- regions_cat

for(i in seq_along(regions_cat)) {
  
  taula_a_estudiar_test <- taula_a_estudiar %>%
    dplyr::filter(Regio == regions_cat[i])
  
  ml  <- lm(Domèstic_total_index ~ Any, data = taula_a_estudiar_test)
  ml2 <- lm(Consum_per_capita_index ~ Any, data = taula_a_estudiar_test)
  
  taula_reg <- taula_a_estudiar_test %>%
    dplyr::select(Domèstic_total_index, Consum_per_capita_index, Any) %>%
    dplyr::mutate(
      valor_regresio_dom = ml$coefficients[1] + ml$coefficients[2] * Any,
      valor_regresio_cap = ml2$coefficients[1] + ml2$coefficients[2] * Any,
      Regio = regions_cat[i]
    )
  
  llista_taules[[i]] <- taula_reg
}

llista_student <- vector("list", length(regions_cat))
names(llista_student) <- regions_cat

for(i in seq_along(regions_cat)) {
  # No serveix molt
  llista_student[[i]] <- t.test(llista_taules[[i]]$valor_regresio_cap, llista_taules[[i]]$valor_regresio_dom)
}


for(i in seq_along(llista_taules)) {
  
  df <- llista_taules[[i]]
  
  plot(
    df$valor_regresio_dom,
    df$valor_regresio_cap,
    main = names(llista_taules)[i],
    xlab = "Valor regressió Domèstic total",
    ylab = "Valor regressió Consum per càpita",
    pch = 19
  )
}


shapiro.test(residuals(ml))