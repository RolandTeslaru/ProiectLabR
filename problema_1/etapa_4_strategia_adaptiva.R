# Etapa 4: Strategie adaptivă (5% sub prag, 20% peste prag)

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

source("/Users/rolandteslaru/Desktop/ProiectLabR/problema_1/functii.R")

set.seed(42)

# La fel
n_zile  <- 365
lambda  <- 1000
p_sus   <- 0.005

n_req <- rpois(n_zile, lambda)
n_sus <- rbinom(n_zile, size = n_req, prob = p_sus)

# nr de verificaro pentru strategia aleatoare (10% din cereri) (la fel ca in e3)
n_ver_aleator   <- round(n_req * 0.10)
det_aleator     <- rhyper(n_zile, m = n_sus, n = n_req - n_sus, # detectate aleator
                          k = n_ver_aleator)

# Strategia adaptiva: 5% sub prag, 20% peste prag
prag <- 1000 # pragul este lambda poisson
# tot ce trece peste meidia lambda = 1000, e considerat o zi aglomerata, deci verificam mai mult (20%)
p_ver_adaptiv   <- ifelse(n_req > prag, 0.20, 0.05)
n_ver_adaptiv   <- round(n_req * p_ver_adaptiv)
det_adaptiv     <- rhyper(n_zile, m = n_sus, n = n_req - n_sus,
                          k = n_ver_adaptiv)

an_aleator <- data.frame(zi = 1:n_zile, n_req, n_sus,
                         n_verificate = n_ver_aleator,
                         detectate = det_aleator,
                         nedetectate = n_sus - det_aleator,
                         strategie = "aleatoare")

an_adaptiv <- data.frame(zi = 1:n_zile, n_req, n_sus,
                         n_verificate = n_ver_adaptiv,
                         detectate = det_adaptiv,
                         nedetectate = n_sus - det_adaptiv,
                         strategie = "adaptiva")

# O simpla functie, folosita pt dataframeurile an_aleator si an_adaptiv, care calculeaza indicatorii de performanta (cerinta 5)
sumar <- function(df, eticheta) {
  # Indicatori de performanata, la fel ca in e3
  data.frame(
    Strategie              = eticheta,
    P_detectie_zi          = round(mean(df$detectate >= 1), 3),
    Proportie_detectate    = round(sum(df$detectate) / sum(df$n_sus), 3),
    Proportie_nedetectate  = round(sum(df$nedetectate) / sum(df$n_sus), 3),
    Verificari_zilnice_med = round(mean(df$n_verificate), 1),
    Eficienta              = round(
      (sum(df$detectate) / sum(df$n_sus)) /
      (sum(df$n_verificate) / sum(df$n_req)), 3)
  )
}

cat("=== COMPARAȚIE Aleatoare vs Adaptivă ===\n")
print(rbind(
  sumar(an_aleator, "Aleatoare 10%"),
  sumar(an_adaptiv, "Adaptivă 5%/20%")
))

# Grafic cumulativ — aceeași logică ca în etapa 3, dar cu 2 strategii.
# O singură curbă pentru suspecte (e comună), două curbe pentru detectate.
# Zonele colorate = ce a ratat fiecare strategie.
cumulativ <- data.frame(
  zi = 1:n_zile,
  Suspecte = cumsum(n_sus),
  Det_aleatoare = cumsum(det_aleator),
  Det_adaptiva  = cumsum(det_adaptiv)
)

g <- ggplot(cumulativ, aes(x = zi)) +
  # Aria ratată de aleatoare (mai mare)
  geom_ribbon(aes(ymin = Det_aleatoare, ymax = Suspecte),
              fill = "tomato", alpha = 0.18) +
  # Aria ratată de adaptivă (mai mică) — desenată peste, ceea ce rămâne
  # vizibil din portocaliu = câștigul net al adaptivei
  geom_ribbon(aes(ymin = Det_adaptiva, ymax = Suspecte),
              fill = "orange", alpha = 0.35) +
  geom_line(aes(y = Suspecte, color = "Suspecte (cumulativ)"),
            linewidth = 1.2) +
  geom_line(aes(y = Det_aleatoare, color = "Detectate — aleatoare 10%"),
            linewidth = 1.1) +
  geom_line(aes(y = Det_adaptiva, color = "Detectate — adaptivă 5/20%"),
            linewidth = 1.1) +
  scale_color_manual(values = c(
    "Suspecte (cumulativ)"        = "steelblue",
    "Detectate — aleatoare 10%"   = "darkred",
    "Detectate — adaptivă 5/20%"  = "darkgreen"
  )) +
  annotate("text",
           x = n_zile * 0.55,
           y = max(cumulativ$Suspecte) * 0.55,
           label = sprintf("zona portocalie =\ncâștig adaptivă (+%d)",
                           sum(det_adaptiv) - sum(det_aleator)),
           color = "darkorange", fontface = "italic", size = 4) +
  labs(title = "Aleatoare vs Adaptivă — evoluție cumulativă",
       subtitle = sprintf("Aleatoare: %d/%d   Adaptivă: %d/%d   (același set de date)",
                          sum(det_aleator), sum(n_sus),
                          sum(det_adaptiv), sum(n_sus)),
       x = "Ziua din an", y = "Număr cumulativ",
       color = NULL) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom")

# Histogramele specifice strategiei ADAPTIVE (cerința 6)
an_adaptiv_df <- data.frame(n_sus = n_sus, detectate = det_adaptiv)

g_hist_sus <- ggplot(an_adaptiv_df, aes(x = n_sus)) +
  geom_histogram(binwidth = 1, fill = "steelblue",
                 color = "white", alpha = 0.85) +
  labs(title = "Distribuția zilnică a cererilor suspecte",
       subtitle = "Strategia adaptivă 5/20%",
       x = "Suspecte într-o zi", y = "Număr de zile") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

g_hist_det <- ggplot(an_adaptiv_df, aes(x = detectate)) +
  geom_histogram(binwidth = 1, fill = "darkgreen",
                 color = "white", alpha = 0.85) +
  labs(title = "Distribuția zilnică a cererilor suspecte detectate",
       subtitle = sprintf("Strategia adaptivă — %d zile fără detecție",
                          sum(det_adaptiv == 0)),
       x = "Detectate într-o zi", y = "Număr de zile") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

# Combinăm toate graficele într-o singură figură
print(g / (g_hist_sus | g_hist_det))
