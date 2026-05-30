# Etapa 3: Strategia aleatoare (10% verificare)
# ==========================================================

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

source("/Users/rolandteslaru/Desktop/ProiectLabR/problema_1/functii.R")

set.seed(42)

# Un singur an cu strategia aleatoare 10%
an_aleator <- simuleaza_aleatoare_full(
  n_iteratii = 1, n_zile = 365, # functia permite mai multe iteratii, dar noi vrem doar o iteratie de un an pentru inceput
  lambda = 1000, p_sus = 0.005,
  p_verif = 0.10
)

# Indicatorii ceruți la cerința 5
# Indicator de eficiență = rata_detectie / efort_relativ
# (> 1 înseamnă că strategia bate verificarea aleatoare echivalentă)
indicatori <- data.frame(
  P_detectie_zi          = mean(an_aleator$detectate >= 1),
  Proportie_detectate    = sum(an_aleator$detectate) / sum(an_aleator$n_sus),
  Proportie_nedetectate  = sum(an_aleator$nedetectate) / sum(an_aleator$n_sus),
  Verificari_zilnice_med = mean(an_aleator$n_verificate),
  Eficienta              = (sum(an_aleator$detectate) /
                            sum(an_aleator$n_sus)) /
                           (sum(an_aleator$n_verificate) /
                            sum(an_aleator$n_req))
)

cat("=== INDICATORI (Strategia ALEATOARE, p_verif = 10%) ===\n")
print(indicatori)

# Grafic: evoluția CUMULATIVĂ a cererilor suspecte vs detectate
# Distanța verticală dintre cele două curbe = câte am ratat până în ziua X.
an_cumulativ <- an_aleator |>
  arrange(zi) |>
  mutate(
    Suspecte_total      = cumsum(n_sus),
    Detectate_cumulativ = cumsum(detectate),
    Nedetectate_cumulativ = cumsum(nedetectate)
  )

g <- an_cumulativ |>
  pivot_longer(cols = c(Suspecte_total, Detectate_cumulativ),
               names_to = "tip", values_to = "valoare") |>
  ggplot(aes(x = zi, y = valoare, color = tip, fill = tip)) +
  geom_ribbon(data = an_cumulativ,
              aes(x = zi, ymin = Detectate_cumulativ, ymax = Suspecte_total),
              fill = "tomato", alpha = 0.20,
              inherit.aes = FALSE) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Suspecte_total" = "steelblue",
                                "Detectate_cumulativ" = "darkgreen"),
                     labels = c("Detectate (cumulativ)",
                                "Suspecte totale (cumulativ)")) +
  annotate("text",
           x = max(an_cumulativ$zi) * 0.6,
           y = max(an_cumulativ$Suspecte_total) * 0.55,
           label = "zona roșie =\ncereri ratate",
           color = "tomato", fontface = "italic", size = 4) +
  labs(title = "Cereri suspecte vs. detectate — evoluție cumulativă",
       subtitle = sprintf("Strategia aleatoare 10%% — la final: %d / %d detectate",
                          sum(an_aleator$detectate), sum(an_aleator$n_sus)),
       x = "Ziua din an", y = "Număr cumulativ",
       color = NULL) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Histogramă: numărul de cereri suspecte pe zi (cerința 6)
g_hist_sus <- ggplot(an_aleator, aes(x = n_sus)) +
  geom_histogram(binwidth = 1, fill = "steelblue",
                 color = "white", alpha = 0.85) +
  labs(title = "Distribuția zilnică a cererilor suspecte",
       subtitle = "Strategia aleatoare 10%",
       x = "Suspecte într-o zi", y = "Număr de zile") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Histogramă: numărul de cereri suspecte detectate (cerința 6)
g_hist_det <- ggplot(an_aleator, aes(x = detectate)) +
  geom_histogram(binwidth = 1, fill = "darkgreen",
                 color = "white", alpha = 0.85) +
  labs(title = "Distribuția zilnică a cererilor suspecte detectate",
       subtitle = sprintf("Strategia aleatoare 10%% — %d zile fără detecție",
                          sum(an_aleator$detectate == 0)),
       x = "Detectate într-o zi", y = "Număr de zile") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Combinăm graficul cumulativ + 2 histograme într-o singură figură
# (cumulativ sus lat, cele 2 histograme jos pe 2 coloane)
print(g / (g_hist_sus | g_hist_det))
