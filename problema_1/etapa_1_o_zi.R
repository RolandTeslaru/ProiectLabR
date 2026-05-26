# ==========================================================
# ETAPA 1 — Simulăm O SINGURĂ ZI
# ----------------------------------------------------------
# Pornim de la cea mai simplă întrebare:
# "Cum arată activitatea sistemului într-o zi?"
#
# Avem nevoie de:
#   - numărul total de cereri  -> Poisson(lambda)
#   - cereri suspecte           -> Binomial(n_req, p_sus)
#   - cereri verificate (10% din total)
#   - detectate                 -> Hipergeometric(suspecte, normale, verificate)
# ==========================================================

set.seed(42)

lambda  <- 1000     # rata medie de cereri pe zi
p_sus   <- 0.005    # 0.5% dintre cereri sunt suspecte
p_verif <- 0.10     # verificăm 10% dintre cereri

# 1. Numărul total de cereri într-o zi
n_req <- rpois(1, lambda)

# 2. Câte dintre ele sunt suspecte
n_sus <- rbinom(1, size = n_req, prob = p_sus)

# 3. Câte verificăm (10% din total)
n_verificate <- round(n_req * p_verif)

# 4. Câte din cele suspecte cad sub verificare
#    (selectăm n_verificate cereri din n_req, dintre care n_sus sunt suspecte)
detectate <- rhyper(nn = 1, m = n_sus, n = n_req - n_sus, k = n_verificate)
nedetectate <- n_sus - detectate

# Afișăm rezultatul
cat("=== ZIUA SIMULATĂ ===\n")
cat(sprintf("Cereri totale:       %d\n", n_req))
cat(sprintf("Cereri suspecte:     %d  (din care detectate: %d, nedetectate: %d)\n",
            n_sus, detectate, nedetectate))
cat(sprintf("Cereri verificate:   %d\n", n_verificate))
