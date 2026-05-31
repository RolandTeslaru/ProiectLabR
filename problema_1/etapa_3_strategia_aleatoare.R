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

indicatori <- data.frame(
  # Prob empirica de a detecta cel putin o cerere suspecta intr-o zi
  P_detectie_zi          = mean(an_aleator$detectate >= 1),                     # vector true/false, media e proportia de zile cu cel putin o detectie
  Proportie_detectate    = sum(an_aleator$detectate) / sum(an_aleator$n_sus),   # nr sus detectate / nr sus totale
  Proportie_nedetectate  = sum(an_aleator$nedetectate) / sum(an_aleator$n_sus), # invers
  Verificari_zilnice_med = mean(an_aleator$n_verificate),                       # media de verificari pe zi

  # eficienta: proportia de sus detectate / proportia de cereri verificate

  Eficienta              = (sum(an_aleator$detectate) /
                            sum(an_aleator$n_sus)) /
                           (sum(an_aleator$n_verificate) /
                            sum(an_aleator$n_req))

  # Daca avem 290 detctate / 365 suspecte = 0.79% (proportia de sus detectate)
  # Daca verificam 36500 cereri / 365000 cereri totale = 0.10% (proportia de cereri verificate)
  # Eficienta = 0.79% / 0.10% = 7.9 

  # eficienta === 1 => detectam exact cat verificam (fiecare verificare aduce o detectie)
  # eficienta > 1 => detectam mai mult decat verificam
  # eficienta < 1 => detectam mai putin decat verificam (verificarile aduc mai multe false positive decat detectii reale)
)

cat("=== INDICATORI (Strategia ALEATOARE, p_verif = 10%) ===\n")
print(indicatori)

# pipe, echivalent cu method chaining (dot chanining: ex [4,3,2,1].sort().max() ) in alte limbaje.
an_cumulativ <- an_aleator |>
  arrange(zi) |> # sortam df ul, dupa zi
  mutate(        # adaugam coloane noi
    Suspecte_total      = cumsum(n_sus), 
    Detectate_cumulativ = cumsum(detectate), 
    Nedetectate_cumulativ = cumsum(nedetectate)
  )

# Grafic cumulativ: cereri suspecte vs detectate (cerința 6)

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
  theme_minimal(base_size = 24) +
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
