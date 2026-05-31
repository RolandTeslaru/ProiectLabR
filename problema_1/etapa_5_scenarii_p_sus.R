# E5 - Scenarii p_sus (strategia aleatoare, 10%, 1000 iterații)
#   - p = 0.001  (foarte rare,  ~1 la mie)
#   - p = 0.005  (rare,         ~5 la mie)
#   - p = 0.02   (relativ rare, 2%)

library(dplyr)
library(ggplot2)

source("/Users/rolandteslaru/Desktop/ProiectLabR/problema_1/functii.R")

set.seed(42)

scenarii <- studiu_scenarii_p_sus(
  valori_p_sus = c(0.001, 0.005, 0.02),
  n_iteratii = 1000,
  p_verif = 0.10
)

cat("=== Scenarii p_sus (strategia aleatoare, 10%, 1000 iterații) ===\n")
print(scenarii)

# Grafic: probabilitatea de detecție pe zi pentru fiecare p_sus
g <- ggplot(scenarii, aes(x = factor(p_sus),
                          y = prob_detectie_zi_medie,
                          fill = factor(p_sus))) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(prob_detectie_zi_medie, 0.1)),
            vjust = -0.5, size = 4) +
  labs(title = "P(detecție ≥ 1 / zi) pentru diferite niveluri p_sus",
       subtitle = "Cu cât evenimentul e mai rar, cu atât e mai greu de prins într-o zi",
       x = "p_sus (probabilitatea unei cereri suspecte)",
       y = "P(detecție ≥ 1 într-o zi)") +
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))

print(g)
