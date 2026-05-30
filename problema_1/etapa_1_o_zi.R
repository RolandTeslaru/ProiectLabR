# Etapa 1: Simularea unei zile de cereri


set.seed(42)

lambda  <- 1000     # rata medie de cereri pe zi
p_sus   <- 0.005    # 0.5% dintre cereri sunt suspecte
p_verif <- 0.10     # verificam 10% dintre cereri

# folsoim distributia Poisson pentru a genera numarl de cereri intro-o zi.
# poission e cel mai realistic, pentru ca numarul de cereri poate varia de la o zi la alta, dar are o rata medie stabila (lambda).
n_req <- rpois(1, lambda)

# generam cate din nreq sunt suspected (doar pt 1 zile)
n_sus <- rbinom(1, size = n_req, prob = p_sus)

# cate verifcam din cele n_req cereri 
n_verificate <- round(n_req * p_verif)

# extragem cate dintre cele n_sus suspecte sunt detectate, folosind distributia hipergeometrica
detectate <- rhyper(nn = 1, m = n_sus, n = n_req - n_sus, k = n_verificate)
nedetectate <- n_sus - detectate

# afisam
cat("=== ZIUA SIMULATA ===\n")
cat(sprintf("Cereri totale:       %d\n", n_req))
cat(sprintf("Cereri suspecte:     %d  (din care detectate: %d, nedetectate: %d)\n",
            n_sus, detectate, nedetectate))
cat(sprintf("Cereri verificate:   %d\n", n_verificate))
