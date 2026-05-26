# ==========================================================
# ETAPA 9 — Strategia GEOGRAFICĂ (cerința 4c — opțional)
# ----------------------------------------------------------
# Generăm O SINGURĂ DATĂ toate cererile (cu regiune și
# statut de suspect). Apoi aplicăm 3 strategii de verificare
# pe ACEEAȘI dată. Toate strategiile văd exact aceleași
# cereri, exact aceiași suspecți. Doar verificarea diferă.
#
# Suspectele au structură regională: regiunile cu risc mare
# (Rusia, Pakistan) au p_sus mai mare → exact pe asta se
# bazează avantajul geograficei.
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

n_zile <- 365
lambda <- 1000

# --- Generare date partajate ---
n_req_zi   <- rpois(n_zile, lambda)
total      <- sum(n_req_zi)
zi_cerere  <- rep(1:n_zile, times = n_req_zi)

# Fiecare cerere primește o regiune (după pondere_trafic)
regiune <- sample.int(nrow(config_regiuni), total, replace = TRUE,
                      prob = config_regiuni$pondere_trafic)

# Fiecare cerere e suspect cu probabilitatea regiunii ei
este_suspect <- runif(total) < config_regiuni$p_sus[regiune]

# --- Strategie 1: aleatoare (verifică 10% din TOATE cererile) ---
este_ver_al <- runif(total) < 0.10

# --- Strategie 2: adaptivă (5% în zile liniștite, 20% în aglomerate) ---
prag        <- 1000
p_ver_zi    <- ifelse(n_req_zi > prag, 0.20, 0.05)
p_ver_per_cerere <- p_ver_zi[zi_cerere]
este_ver_ad <- runif(total) < p_ver_per_cerere

# --- Strategie 3: geografică (p_verif al regiunii cererii) ---
este_ver_geo <- runif(total) < config_regiuni$p_verif[regiune]

# Detecții pentru fiecare strategie = suspect ŞI verificat
det_al  <- este_suspect & este_ver_al
det_ad  <- este_suspect & este_ver_ad
det_geo <- este_suspect & este_ver_geo

# Agregare pe zi pentru fiecare strategie
agreg_pe_zi <- function(vec_logic) {
  as.integer(tapply(vec_logic, zi_cerere, sum))
}

n_sus_zi   <- agreg_pe_zi(este_suspect)
n_ver_al_z  <- agreg_pe_zi(este_ver_al)
n_ver_ad_z  <- agreg_pe_zi(este_ver_ad)
n_ver_geo_z <- agreg_pe_zi(este_ver_geo)
det_al_z   <- agreg_pe_zi(det_al)
det_ad_z   <- agreg_pe_zi(det_ad)
det_geo_z  <- agreg_pe_zi(det_geo)

# Indicator de eficiență (cerința 5)
eficienta <- function(detect, verif) {
  (sum(detect) / sum(este_suspect)) / (sum(verif) / total)
}

rezumat <- data.frame(
  Strategie     = c("aleatoare", "adaptiva", "geografica"),
  Cereri        = total,
  Suspecti      = sum(este_suspect),
  Detectate     = c(sum(det_al), sum(det_ad), sum(det_geo)),
  Verificari    = c(sum(este_ver_al), sum(este_ver_ad),
                    sum(este_ver_geo)),
  Rata_detectie = c(sum(det_al), sum(det_ad), sum(det_geo)) /
                  sum(este_suspect),
  Eficienta     = c(eficienta(det_al,  este_ver_al),
                    eficienta(det_ad,  este_ver_ad),
                    eficienta(det_geo, este_ver_geo))
)

cat("=== Comparație finală — același set de date, 3 strategii ===\n")
print(rezumat)

# Graficul cumulativ: 1 linie suspecte + 3 linii detectate
cumul <- data.frame(
  zi = 1:n_zile,
  Suspecte   = cumsum(n_sus_zi),
  aleatoare  = cumsum(det_al_z),
  adaptiva   = cumsum(det_ad_z),
  geografica = cumsum(det_geo_z)
) |>
  pivot_longer(c(aleatoare, adaptiva, geografica),
               names_to = "strategie", values_to = "Detectate")

etichete <- cumul |>
  group_by(strategie) |>
  filter(zi == max(zi)) |>
  mutate(label = sprintf("%s: %d (%.1f%%)",
                         strategie, Detectate,
                         100 * Detectate / Suspecte))

culori <- c("aleatoare"  = "firebrick",
            "adaptiva"   = "darkorange",
            "geografica" = "darkgreen")

g <- ggplot(cumul, aes(x = zi)) +
  geom_line(aes(y = Suspecte), color = "steelblue",
            linewidth = 1.3, linetype = "dashed") +
  geom_line(aes(y = Detectate, color = strategie), linewidth = 1.4) +
  geom_text(data = etichete,
            aes(x = zi + 3, y = Detectate, label = label,
                color = strategie),
            hjust = 0, fontface = "bold", size = 5,
            show.legend = FALSE) +
  annotate("text", x = 300, y = max(cumul$Suspecte) * 1.03,
           label = sprintf("Total suspecte: %d", sum(este_suspect)),
           color = "steelblue", fontface = "bold", size = 4.5) +
  scale_color_manual(values = culori) +
  expand_limits(x = max(cumul$zi) + 80) +
  labs(title = "Cumulativ: 3 strategii pe ACEEAȘI date",
       subtitle = "Linia albastră întreruptă = suspecte; cu cât linia colorată e mai aproape de ea, cu atât e mai bună strategia",
       x = "Ziua din an", y = "Număr cumulativ",
       color = "Strategie") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12))

# Histogramele specifice strategiei GEOGRAFICE (cerința 6)
an_geo_df <- data.frame(n_sus = n_sus_zi, detectate = det_geo_z)

g_hist_sus <- ggplot(an_geo_df, aes(x = n_sus)) +
  geom_histogram(binwidth = 1, fill = "steelblue",
                 color = "white", alpha = 0.85) +
  labs(title = "Distribuția zilnică a cererilor suspecte",
       subtitle = "Strategia geografică",
       x = "Suspecte într-o zi", y = "Număr de zile") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

g_hist_det <- ggplot(an_geo_df, aes(x = detectate)) +
  geom_histogram(binwidth = 1, fill = "darkgreen",
                 color = "white", alpha = 0.85) +
  labs(title = "Distribuția zilnică a cererilor suspecte detectate",
       subtitle = sprintf("Strategia geografică — %d zile fără detecție",
                          sum(det_geo_z == 0)),
       x = "Detectate într-o zi", y = "Număr de zile") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

# Combinăm toate graficele într-o singură figură
print(g / (g_hist_sus | g_hist_det))
