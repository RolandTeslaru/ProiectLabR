simuleaza_aleatoare_full <- function(n_iteratii = 1000, n_zile = 365,
                                      lambda = 1000, p_sus = 0.001,
                                      p_verif = 0.10) {
  
  total <- n_iteratii * n_zile   # 365.000 zile în total
  
  # TOATE numerele de cereri, dintr-un singur apel
  n_req <- rpois(total, lambda)
  
  # TOATE numerele de suspecte, dintr-un singur apel
  n_sus <- rbinom(total, size = n_req, prob = p_sus)
  
  # TOATE numerele de verificate
  n_verificate <- round(n_req * p_verif)
  
  # TOATE detectatele (hipergeometrică vectorizată)
 detectate <- rhyper(nn = total, m = n_sus, n = n_req - n_sus, k = n_verificate)
  
  data.frame(
    iteratie = rep(1:n_iteratii, each = n_zile),
    zi = rep(1:n_zile, times = n_iteratii),
    n_req = n_req,
    n_sus = n_sus,
    n_verificate = n_verificate,
    detectate = detectate,
    nedetectate = n_sus - detectate,
    strategie = "aleatoare"
  )
}

config_regiuni <- data.frame(
  regiune        = c("EU", "NA", "India", "Pakistan", "Bangladesh", "Rusia", "Alte"),
  pondere_trafic = c(0.35, 0.30, 0.15, 0.05, 0.05, 0.05, 0.05),
  p_sus          = c(0.0005, 0.0005, 0.002, 0.008, 0.006, 0.015, 0.010),
  p_verif        = c(0.02, 0.02, 0.10, 0.25, 0.20, 0.40, 0.30)
)

simuleaza_adaptiva_full <- function(n_iteratii = 1000, n_zile = 365,
                                     lambda = 1000, p_sus = 0.001,
                                     prag = 1000,
                                     p_verif_mic = 0.05, p_verif_mare = 0.20) {
  
  total <- n_iteratii * n_zile
  
  n_req <- rpois(total, lambda)
  n_sus <- rbinom(total, n_req, p_sus)
  
  # ifelse vectorizat pe toate cele 365.000 de zile
  procent <- ifelse(n_req > prag, p_verif_mare, p_verif_mic)
  
  n_verificate <- round(n_req * procent)
  detectate <- rhyper(nn = total, m = n_sus, n = n_req - n_sus, k = n_verificate)
  
  data.frame(
    iteratie = rep(1:n_iteratii, each = n_zile),
    zi = rep(1:n_zile, times = n_iteratii),
    n_req = n_req,
    n_sus = n_sus,
    n_verificate = n_verificate,
    detectate = detectate,
    nedetectate = n_sus - detectate,
    strategie = "adaptiva"
  )
}



simuleaza_geografica_full <- function(n_iteratii = 1000, n_zile = 365,
                                       lambda = 1000, config = config_regiuni) {
  
  total_zile <- n_iteratii * n_zile
  
  # Cereri pe zi pentru toate cele 365.000 de zile
  n_req_zile <- rpois(total_zile, lambda)
  total_cereri <- sum(n_req_zile)   # ~365 de milioane de cereri
  
  # ATENȚIE: cu lambda=1000, asta înseamnă ~365 milioane de cereri în memorie.
  # Vezi mai jos discuția despre memorie.
  
  idx_regiune <- sample.int(nrow(config), total_cereri, replace = TRUE,
                            prob = config$pondere_trafic)
  
  este_sus  <- runif(total_cereri) < config$p_sus[idx_regiune]
  este_ver  <- runif(total_cereri) < config$p_verif[idx_regiune]
  este_det  <- este_sus & este_ver
  
  # Agregare pe zi
  zi_per_cerere <- rep(1:total_zile, times = n_req_zile)
  
  n_sus <- as.integer(tapply(este_sus, zi_per_cerere, sum))
  n_ver <- as.integer(tapply(este_ver, zi_per_cerere, sum))
  n_det <- as.integer(tapply(este_det, zi_per_cerere, sum))
  
  data.frame(
    iteratie = rep(1:n_iteratii, each = n_zile),
    zi = rep(1:n_zile, times = n_iteratii),
    n_req = n_req_zile,
    n_sus = n_sus,
    n_verificate = n_ver,
    detectate = n_det,
    nedetectate = n_sus - n_det,
    strategie = "geografica"
  )
}



# ==========================================================
# Studiu pe scenarii de p_sus (cerința 3)
# Rulează strategia aleatoare cu mai multe rate de suspiciune
# ==========================================================
studiu_scenarii_p_sus <- function(
    valori_p_sus = c(0.001, 0.005, 0.02),
    n_iteratii = 1000, n_zile = 365,
    lambda = 1000, p_verif = 0.10) {

  rezultate <- lapply(valori_p_sus, function(p) {
    sim <- simuleaza_aleatoare_full(n_iteratii = n_iteratii, n_zile = n_zile,
                                    lambda = lambda, p_sus = p,
                                    p_verif = p_verif)

    ind <- sim |>
      dplyr::group_by(iteratie) |>
      dplyr::summarise(
        rata_detectie = sum(detectate) / pmax(sum(n_sus), 1),
        prob_detectie_zi = mean(detectate >= 1),
        total_suspecte = sum(n_sus),
        .groups = "drop"
      )

    data.frame(
      p_sus = p,
      total_suspecte_mediu = mean(ind$total_suspecte),
      rata_detectie_medie = mean(ind$rata_detectie),
      rata_detectie_sd = sd(ind$rata_detectie),
      prob_detectie_zi_medie = mean(ind$prob_detectie_zi)
    )
  })

  dplyr::bind_rows(rezultate)
}


# ==========================================================
# Studiu pe procente de verificare (cerința 7)
# Rulează strategia aleatoare cu mai multe procente și
# întoarce indicatorii agregați (medie pe 1000 iterații).
# ==========================================================
studiu_procente_verificare <- function(
    procente = c(0.01, 0.05, 0.10, 0.20, 0.30),
    n_iteratii = 1000, n_zile = 365,
    lambda = 1000, p_sus = 0.005,
    c1 = 1, c2 = 100) {

  rezultate <- lapply(procente, function(p) {
    sim <- simuleaza_aleatoare_full(n_iteratii = n_iteratii, n_zile = n_zile,
                                    lambda = lambda, p_sus = p_sus,
                                    p_verif = p)

    ind <- sim |>
      dplyr::group_by(iteratie) |>
      dplyr::summarise(
        rata_detectie = sum(detectate) / pmax(sum(n_sus), 1),
        prob_detectie_zi = mean(detectate >= 1),
        verificari_zilnice_medii = mean(n_verificate),
        cost_total = c1 * sum(n_verificate) + c2 * sum(nedetectate),
        .groups = "drop"
      )

    data.frame(
      p_verif = p,
      rata_detectie_medie = mean(ind$rata_detectie),
      rata_detectie_sd = sd(ind$rata_detectie),
      prob_detectie_zi_medie = mean(ind$prob_detectie_zi),
      prob_detectie_zi_sd = sd(ind$prob_detectie_zi),
      verificari_zilnice_medii = mean(ind$verificari_zilnice_medii),
      cost_mediu = mean(ind$cost_total),
      cost_sd = sd(ind$cost_total)
    )
  })

  dplyr::bind_rows(rezultate)
}


simuleaza_geografica_chunks <- function(n_iteratii = 1000, chunk = 50, ...) {
  chunks <- split(1:n_iteratii, ceiling(seq_along(1:n_iteratii) / chunk))
  
  rezultate <- lapply(chunks, function(idx) {
    rez <- simuleaza_geografica_full(n_iteratii = length(idx), ...)
    rez$iteratie <- rep(idx, each = 365)
    rez
  })
  dplyr::bind_rows(rezultate)
}