# =============================================================================
# TP Profilage Force-Vitesse – Partie 2
# Sujets : Emma Menon & Jeremy Vertueux
# Auteurs : Arthur Deletang, Maxence Goualou, Clémence Pinsard, Lucas Servant
# =============================================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

# ---- 0. Paramètres généraux -------------------------------------------------

# Fichiers de données (à placer dans le répertoire de travail)
fichier_emma   <- "Emma Menon - Running.xlsx"
fichier_jeremy <- "Jeremy Vertueux - Running.xlsx"

# Charge nominale par numéro de répétition (extraite du champ Machine Settings)
charges_emma   <- c(0.1, 0.1,  5,  5, 10, 10, 15, 15, 20, 20, 25)
charges_jeremy <- c(0.1, 0.1,  5,  5, 10, 10, 15, 15, 20, 20, 25, 25)

g <- 9.81  # accélération de la pesanteur [m/s²]

# Taille de la fenêtre de régime stable (en mètres depuis la fin du sprint)
WINDOW_STABLE_M <- 5

# Seuil de CV (%) au-delà duquel la vitesse stable est considérée peu fiable
CV_SEUIL <- 10

# Thème graphique commun
theme_fv <- theme_bw(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title    = element_blank()
  )

couleurs <- c("Emma Menon" = "#E05C2A", "Jeremy Vertueux" = "#2A7AE0")

# ---- 1. Chargement des données ----------------------------------------------

load_raw <- function(filepath, charges_vec) {
  df <- read_excel(filepath)
  df %>%
    mutate(
      charge_kg = charges_vec[RepNumber],
      force_N   = charge_kg * g
    )
}

emma_raw   <- load_raw(fichier_emma,   charges_emma)
jeremy_raw <- load_raw(fichier_jeremy, charges_jeremy)

# ---- 2. Détermination de la vitesse en régime stable (Q2) ------------------
#
# Méthode retenue :
#   On sélectionne les WINDOW_STABLE_M derniers mètres de chaque essai
#   (phase où l'athlète a atteint sa vitesse maximale).
#   La vitesse stable est estimée par la médiane des vitesses instantanées
#   sur cette fenêtre.
#
# Critère de validité :
#   CV < CV_SEUIL % -> vitesse stable considérée fiable.
#   CV >= CV_SEUIL % -> signal de vigilance (charge trop élevée ou distance
#   insuffisante pour atteindre le régime stable).
#
# Limite :
#   La médiane est robuste aux pics parasites mais ne corrige pas un régime
#   stable jamais atteint (ex. charges très élevées). Une distance de sprint
#   plus longue (20 m) améliorerait la détection.

get_stable_speed <- function(df_raw) {
  df_raw %>%
    group_by(RepNumber, charge_kg, force_N) %>%
    mutate(dist_max = max(`Distance [m]`)) %>%
    filter(`Distance [m]` >= dist_max - WINDOW_STABLE_M,
           `Speed [m/s]`  > 0) %>%
    summarise(
      v_stable  = median(`Speed [m/s]`, na.rm = TRUE),
      v_mean    = mean(`Speed [m/s]`,   na.rm = TRUE),
      v_sd      = sd(`Speed [m/s]`,     na.rm = TRUE),
      cv_pct    = 100 * v_sd / v_mean,
      n_pts     = n(),
      valide    = cv_pct < CV_SEUIL,
      .groups   = "drop"
    )
}

emma_stable   <- get_stable_speed(emma_raw)
jeremy_stable <- get_stable_speed(jeremy_raw)

cat("=== Q2 - Vitesses en regime stable ===\n")
cat("\n--- Emma Menon ---\n")
print(emma_stable %>% select(RepNumber, charge_kg, v_stable, cv_pct, valide))
cat("\n--- Jeremy Vertueux ---\n")
print(jeremy_stable %>% select(RepNumber, charge_kg, v_stable, cv_pct, valide))

# ---- 3. Assignation des series A / B ----------------------------------------
# Série A = 1er essai de chaque charge, Série B = 2e essai

assign_serie <- function(stable_df) {
  stable_df %>%
    arrange(charge_kg, RepNumber) %>%
    group_by(charge_kg) %>%
    mutate(serie = c("A", "B")[seq_len(n())]) %>%
    ungroup()
}

emma_series   <- assign_serie(emma_stable)
jeremy_series <- assign_serie(jeremy_stable)

# ---- 4. Fonction de régression F-V ------------------------------------------

fit_fv_lm <- function(df_serie) {
  d <- filter(df_serie, valide == TRUE, v_stable > 0)
  if (nrow(d) < 3) {
    warning("Moins de 3 points valides - regression peu fiable.")
  }
  lm(force_N ~ v_stable, data = d)
}

extract_params <- function(fit) {
  co    <- coef(fit)
  F0    <- co["(Intercept)"]
  slope <- co["v_stable"]
  V0    <- -F0 / slope
  Pmax  <- F0 * V0 / 4
  Vopt  <- V0 / 2
  Fopt  <- F0 / 2
  charge_opt_kg <- Fopt / g
  r2    <- summary(fit)$r.squared
  list(F0 = F0, V0 = V0, slope = slope, Pmax = Pmax,
       Vopt = Vopt, Fopt = Fopt, charge_opt_kg = charge_opt_kg, r2 = r2)
}

# ---- 5. Identification de la meilleure serie (Q8) ---------------------------

fit_A_emma   <- fit_fv_lm(filter(emma_series,   serie == "A"))
fit_B_emma   <- fit_fv_lm(filter(emma_series,   serie == "B"))
fit_A_jeremy <- fit_fv_lm(filter(jeremy_series, serie == "A"))
fit_B_jeremy <- fit_fv_lm(filter(jeremy_series, serie == "B"))

r2_emma_A   <- summary(fit_A_emma)$r.squared
r2_emma_B   <- summary(fit_B_emma)$r.squared
r2_jeremy_A <- summary(fit_A_jeremy)$r.squared
r2_jeremy_B <- summary(fit_B_jeremy)$r.squared

best_emma   <- if (r2_emma_A   >= r2_emma_B)   "A" else "B"
best_jeremy <- if (r2_jeremy_A >= r2_jeremy_B) "A" else "B"

fit_emma_best   <- if (best_emma   == "A") fit_A_emma   else fit_B_emma
fit_jeremy_best <- if (best_jeremy == "A") fit_A_jeremy else fit_B_jeremy

params_emma   <- extract_params(fit_emma_best)
params_jeremy <- extract_params(fit_jeremy_best)

cat("\n=== Q3/Q4 - Parametres mecaniques (meilleure serie) ===\n")
cat(sprintf("Emma   (serie %s) : F0=%.1f N | V0=%.2f m/s | Pmax=%.1f W | Pente=%.2f N.s/m | R2=%.3f\n",
            best_emma,
            params_emma$F0, params_emma$V0, params_emma$Pmax,
            params_emma$slope, params_emma$r2))
cat(sprintf("Jeremy (serie %s) : F0=%.1f N | V0=%.2f m/s | Pmax=%.1f W | Pente=%.2f N.s/m | R2=%.3f\n",
            best_jeremy,
            params_jeremy$F0, params_jeremy$V0, params_jeremy$Pmax,
            params_jeremy$slope, params_jeremy$r2))

cat(sprintf("\nQ5 - Vopt / charge optimale :\n"))
cat(sprintf("Emma   : Vopt=%.2f m/s | Fopt=%.1f N | Charge opt~%.1f kg\n",
            params_emma$Vopt, params_emma$Fopt, params_emma$charge_opt_kg))
cat(sprintf("Jeremy : Vopt=%.2f m/s | Fopt=%.1f N | Charge opt~%.1f kg\n",
            params_jeremy$Vopt, params_jeremy$Fopt, params_jeremy$charge_opt_kg))

# ---- 6. Reproductibilite (Q9) -----------------------------------------------

compare_params <- function(pA, pB, nom) {
  cat(sprintf("\n--- %s ---\n", nom))
  params_tab <- data.frame(
    Parametre = c("F0 (N)", "V0 (m/s)", "Pente (N.s/m)", "Pmax (W)", "R2"),
    Serie_A   = c(pA$F0, pA$V0, pA$slope, pA$Pmax, pA$r2),
    Serie_B   = c(pB$F0, pB$V0, pB$slope, pB$Pmax, pB$r2)
  )
  params_tab$Delta  <- abs(params_tab$Serie_A - params_tab$Serie_B)
  params_tab$CV_pct <- 100 * params_tab$Delta /
    rowMeans(abs(params_tab[, c("Serie_A","Serie_B")]))
  print(params_tab, digits = 3, row.names = FALSE)
}

pA_e <- extract_params(fit_A_emma)
pB_e <- extract_params(fit_B_emma)
pA_j <- extract_params(fit_A_jeremy)
pB_j <- extract_params(fit_B_jeremy)

cat("\n=== Q8/Q9 - Reproductibilite intra-sujet ===")
compare_params(pA_e, pB_e, "Emma Menon")
compare_params(pA_j, pB_j, "Jeremy Vertueux")

# ---- 7. Influence du nombre de points (Q6) ----------------------------------

influence_npts <- function(series_df, best_serie) {
  d_best    <- filter(series_df, serie == best_serie, valide == TRUE, v_stable > 0)
  charges_u <- sort(unique(d_best$charge_kg))
  n_max     <- length(charges_u)
  
  out <- lapply(2:n_max, function(n) {
    idx   <- unique(round(seq(1, n_max, length.out = n)))
    sel   <- charges_u[idx]
    d_sub <- filter(d_best, charge_kg %in% sel)
    if (nrow(d_sub) < 2) return(NULL)
    fit   <- lm(force_N ~ v_stable, data = d_sub)
    p     <- extract_params(fit)
    data.frame(n_pts = n, F0 = p$F0, V0 = p$V0, Pmax = p$Pmax, r2 = p$r2)
  })
  bind_rows(out)
}

infl_emma   <- influence_npts(emma_series,   best_emma)
infl_jeremy <- influence_npts(jeremy_series, best_jeremy)

cat("\n=== Q6 - Influence du nombre de points ===\n")
cat("Emma :\n");   print(infl_emma,   digits = 3, row.names = FALSE)
cat("Jeremy :\n"); print(infl_jeremy, digits = 3, row.names = FALSE)

# ---- 8. Pertinence des charges elevees (Q7) ---------------------------------

cat("\n=== Q7 - CV des charges elevees ===\n")
cat("Emma :\n")
print(filter(emma_series, charge_kg >= 20) %>%
        select(serie, charge_kg, v_stable, cv_pct, valide))
cat("Jeremy :\n")
print(filter(jeremy_series, charge_kg >= 20) %>%
        select(serie, charge_kg, v_stable, cv_pct, valide))

# ---- 9. GRAPHIQUES -----------------------------------------------------------

## 9a. Vitesse en fonction de la distance - toutes les répétitions (Q2)

plot_toutes_reps <- function(df_raw, stable_df, nom, couleur) {
  
  charges_uniq <- sort(unique(df_raw$charge_kg))
  n_charges    <- length(charges_uniq)
  palette      <- colorRampPalette(c("grey75", couleur))(n_charges)
  noms_charges <- paste0(charges_uniq, " kg")
  
  df_plot <- df_raw %>%
    filter(`Speed [m/s]` >= 0) %>%
    mutate(charge_label = factor(paste0(charge_kg, " kg"), levels = noms_charges))
  
  # Zones stables : uniquement les reps avec une vitesse valide
  reps_valides <- stable_df %>%
    filter(valide == TRUE, v_stable > 0) %>%
    pull(RepNumber)
  
  df_zones <- df_raw %>%
    filter(RepNumber %in% reps_valides) %>%
    group_by(RepNumber, charge_kg) %>%
    summarise(dist_max = max(`Distance [m]`), .groups = "drop") %>%
    mutate(
      xmin         = dist_max - WINDOW_STABLE_M,
      xmax         = dist_max,
      charge_label = factor(paste0(charge_kg, " kg"), levels = noms_charges)
    )
  
  # Médiane : segment limité à la fenêtre stable, reps valides seulement
  df_mediane <- stable_df %>%
    filter(valide == TRUE, v_stable > 0) %>%
    left_join(
      df_raw %>%
        group_by(RepNumber) %>%
        summarise(dist_max = max(`Distance [m]`), .groups = "drop"),
      by = "RepNumber"
    ) %>%
    mutate(
      xmin         = dist_max - WINDOW_STABLE_M,
      xmax         = dist_max,
      charge_label = factor(paste0(charge_kg, " kg"), levels = noms_charges)
    )
  
  ggplot(df_plot, aes(x = `Distance [m]`, y = `Speed [m/s]`,
                      colour = charge_label, group = RepNumber)) +
    geom_rect(data = df_zones,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                  fill = charge_label),
              inherit.aes = FALSE, alpha = 0.15) +
    geom_line(linewidth = 0.7) +
    geom_segment(data = df_mediane,
                 aes(x = xmin, xend = xmax,
                     y = v_stable, yend = v_stable,
                     colour = charge_label),
                 linetype = "dashed", linewidth = 1.1,
                 inherit.aes = FALSE) +
    scale_colour_manual(values = setNames(palette, noms_charges)) +
    scale_fill_manual(values  = setNames(palette, noms_charges),
                      guide   = "none") +
    labs(
      title    = nom,
      subtitle = sprintf(
        "Zone colorée = fenêtre de régime stable (derniers %d m) | tiret = médiane",
        WINDOW_STABLE_M),
      x      = "Distance [m]",
      y      = "Vitesse [m/s]",
      colour = "Charge"
    ) +
    theme_fv
}

p_vit_emma   <- plot_toutes_reps(emma_raw,   emma_stable,
                                 "Emma Menon",      couleurs["Emma Menon"])
p_vit_jeremy <- plot_toutes_reps(jeremy_raw, jeremy_stable,
                                 "Jeremy Vertueux", couleurs["Jeremy Vertueux"])

p_vitesses <- p_vit_emma / p_vit_jeremy
ggsave("FV_vitesse_stable.pdf", p_vitesses, width = 11, height = 10)

## 9b. Profil F-V superpose - meilleure serie (Q3/Q4)

v_max_global <- max(params_emma$V0, params_jeremy$V0) * 1.08
v_seq        <- seq(0, v_max_global, length.out = 300)

fv_pred <- function(params, v) pmax(0, params$F0 + params$slope * v)

df_fv_lines <- bind_rows(
  data.frame(v = v_seq, F = fv_pred(params_emma, v_seq),   sujet = "Emma Menon"),
  data.frame(v = v_seq, F = fv_pred(params_jeremy, v_seq), sujet = "Jeremy Vertueux")
)

df_fv_pts <- bind_rows(
  filter(emma_series,   serie == best_emma,   valide == TRUE, v_stable > 0) %>%
    mutate(sujet = "Emma Menon"),
  filter(jeremy_series, serie == best_jeremy, valide == TRUE, v_stable > 0) %>%
    mutate(sujet = "Jeremy Vertueux")
)

df_fv_nv <- bind_rows(
  filter(emma_series,   serie == best_emma,   valide == FALSE) %>% mutate(sujet = "Emma Menon"),
  filter(jeremy_series, serie == best_jeremy, valide == FALSE) %>% mutate(sujet = "Jeremy Vertueux")
)

p_fv <- ggplot() +
  geom_line(data = df_fv_lines, aes(x = v, y = F, colour = sujet), linewidth = 1.3) +
  geom_point(data = df_fv_pts,
             aes(x = v_stable, y = force_N, colour = sujet, shape = sujet),
             size = 3.5, stroke = 1.4) +
  geom_point(data = df_fv_nv,
             aes(x = v_stable, y = force_N, colour = sujet),
             size = 3.5, shape = 4, stroke = 1.5, alpha = 0.5) +
  geom_text(data = df_fv_pts,
            aes(x = v_stable, y = force_N,
                label = paste0(charge_kg, " kg"), colour = sujet),
            vjust = -0.9, size = 2.7, show.legend = FALSE) +
  scale_colour_manual(values = couleurs) +
  scale_shape_manual(values = c("Emma Menon" = 21, "Jeremy Vertueux" = 22)) +
  labs(title    = "Profil Force-Vitesse (meilleure serie par sujet)",
       subtitle = "x = point exclu de la regression (CV > 10 %)",
       x = "Vitesse [m/s]", y = "Force resistante [N]",
       colour = NULL, shape = NULL) +
  theme_fv

ggsave("FV_profil_FV.pdf", p_fv, width = 9, height = 6)

## 9c. Profil Puissance-Vitesse (Q5)

pv_pred <- function(params, v) {
  F <- pmax(0, params$F0 + params$slope * v)
  F * v
}

df_pv <- bind_rows(
  data.frame(v = v_seq, P = pv_pred(params_emma, v_seq),   sujet = "Emma Menon"),
  data.frame(v = v_seq, P = pv_pred(params_jeremy, v_seq), sujet = "Jeremy Vertueux")
)

df_pmax <- data.frame(
  sujet = c("Emma Menon", "Jeremy Vertueux"),
  Vopt  = c(params_emma$Vopt, params_jeremy$Vopt),
  Pmax  = c(params_emma$Pmax, params_jeremy$Pmax),
  label = c(
    sprintf("Pmax = %.0f W\nVopt = %.2f m/s\nCharge ~ %.1f kg",
            params_emma$Pmax, params_emma$Vopt, params_emma$charge_opt_kg),
    sprintf("Pmax = %.0f W\nVopt = %.2f m/s\nCharge ~ %.1f kg",
            params_jeremy$Pmax, params_jeremy$Vopt, params_jeremy$charge_opt_kg)
  )
)

p_pv <- ggplot() +
  geom_line(data = df_pv, aes(x = v, y = P, colour = sujet), linewidth = 1.3) +
  geom_point(data = df_pmax, aes(x = Vopt, y = Pmax, colour = sujet),
             size = 5, shape = 8, stroke = 1.5) +
  geom_label(data = df_pmax,
             aes(x = Vopt, y = Pmax, label = label, colour = sujet),
             vjust = 2, size = 3, show.legend = FALSE,
             fill = "white", alpha = 0.9, label.size = 0.3) +
  scale_colour_manual(values = couleurs) +
  labs(title = "Profil Puissance-Vitesse",
       x = "Vitesse [m/s]", y = "Puissance [W]", colour = NULL) +
  theme_fv

ggsave("FV_profil_PV.pdf", p_pv, width = 9, height = 6)

## 9d. Reproductibilite : serie A vs B (Q8)

make_repro_plot <- function(series_df, best_s, nom, col_best, col_other) {
  other_s  <- if (best_s == "A") "B" else "A"
  fit_best  <- fit_fv_lm(filter(series_df, serie == best_s))
  fit_other <- fit_fv_lm(filter(series_df, serie == other_s))
  p_best    <- extract_params(fit_best)
  p_other   <- extract_params(fit_other)
  
  best_lbl  <- sprintf("Serie %s - meilleure (R2=%.3f)", best_s,  p_best$r2)
  other_lbl <- sprintf("Serie %s (R2=%.3f)",             other_s, p_other$r2)
  
  v_r <- seq(0, max(p_best$V0, p_other$V0) * 1.06, length.out = 300)
  
  df_lines <- bind_rows(
    data.frame(v = v_r, F = pmax(0, p_best$F0  + p_best$slope  * v_r), serie = best_lbl),
    data.frame(v = v_r, F = pmax(0, p_other$F0 + p_other$slope * v_r), serie = other_lbl)
  )
  df_pts <- series_df %>%
    filter(v_stable > 0) %>%
    mutate(serie_lbl = ifelse(serie == best_s, best_lbl, other_lbl))
  
  col_map <- setNames(c(col_best, col_other), c(best_lbl, other_lbl))
  
  ggplot() +
    geom_line(data = df_lines, aes(x = v, y = F, colour = serie), linewidth = 1.1) +
    geom_point(data = filter(df_pts, valide == TRUE),
               aes(x = v_stable, y = force_N, colour = serie_lbl),
               size = 3, shape = 21, fill = "white", stroke = 1.3) +
    geom_point(data = filter(df_pts, valide == FALSE),
               aes(x = v_stable, y = force_N, colour = serie_lbl),
               size = 3, shape = 4, stroke = 1.3, alpha = 0.5) +
    scale_colour_manual(values = col_map) +
    labs(title = nom, x = "Vitesse [m/s]", y = "Force [N]", colour = NULL) +
    theme_fv
}

p_repro_emma   <- make_repro_plot(emma_series,   best_emma,
                                  "Emma Menon - Reproductibilite",
                                  couleurs["Emma Menon"], "#F5A07A")
p_repro_jeremy <- make_repro_plot(jeremy_series, best_jeremy,
                                  "Jeremy Vertueux - Reproductibilite",
                                  couleurs["Jeremy Vertueux"], "#7AB5F5")

p_repro <- p_repro_emma / p_repro_jeremy
ggsave("FV_reproductibilite.pdf", p_repro, width = 9, height = 10)

## 9e. Influence du nombre de points (Q6)

make_npts_plot <- function(infl_df, nom, col) {
  n_ref <- max(infl_df$n_pts)
  V0_ref <- filter(infl_df, n_pts == n_ref)$V0
  
  p1 <- ggplot(infl_df, aes(x = n_pts, y = V0)) +
    geom_line(colour = col, linewidth = 1) +
    geom_point(colour = col, size = 3) +
    geom_hline(yintercept = V0_ref, linetype = "dashed", colour = "grey50") +
    scale_x_continuous(breaks = infl_df$n_pts) +
    labs(x = "Nombre de points", y = "V0 estime [m/s]") +
    theme_fv
  
  p2 <- ggplot(infl_df, aes(x = n_pts, y = r2)) +
    geom_line(colour = col, linewidth = 1) +
    geom_point(colour = col, size = 3) +
    scale_x_continuous(breaks = infl_df$n_pts) +
    ylim(0.7, 1) +
    labs(x = "Nombre de points", y = "R2") +
    theme_fv
  
  (p1 / p2) + plot_annotation(title = nom)
}

p_npts_emma   <- make_npts_plot(infl_emma,   "Emma - Influence du nombre de points",   couleurs["Emma Menon"])
p_npts_jeremy <- make_npts_plot(infl_jeremy, "Jeremy - Influence du nombre de points", couleurs["Jeremy Vertueux"])

ggsave("FV_npts_emma.pdf",   p_npts_emma,   width = 7, height = 7)
ggsave("FV_npts_jeremy.pdf", p_npts_jeremy, width = 7, height = 7)

# ---- 10. Tableau recapitulatif ----------------------------------------------

cat("\n========== TABLEAU RECAPITULATIF ==========\n")
cat(sprintf("%-16s | %-5s | %-7s | %-8s | %-8s | %-9s | %-6s | %-5s\n",
            "Sujet", "Serie", "F0 (N)", "V0 (m/s)",
            "Pmax (W)", "Vopt (m/s)", "Pente", "R2"))
cat(strrep("-", 80), "\n")

for (s in c("A","B")) {
  for (cfg in list(
    list(df = emma_series,   nom = "Emma Menon"),
    list(df = jeremy_series, nom = "Jeremy Vertueux")
  )) {
    d_s <- filter(cfg$df, serie == s)
    if (nrow(d_s) == 0) next
    fit_s <- fit_fv_lm(d_s)
    p_s   <- extract_params(fit_s)
    best_flag <- if ((cfg$nom == "Emma Menon"      && s == best_emma) ||
                     (cfg$nom == "Jeremy Vertueux" && s == best_jeremy)) " *" else ""
    cat(sprintf("%-16s | %-5s | %-7.1f | %-8.2f | %-8.1f | %-9.2f | %-6.2f | %-5.3f%s\n",
                cfg$nom, s, p_s$F0, p_s$V0, p_s$Pmax,
                p_s$Vopt, p_s$slope, p_s$r2, best_flag))
  }
}
cat("\n* = meilleure serie (R2 le plus eleve)\n")
cat("\nFichiers PDF generes :\n")
for (f in c("FV_vitesse_stable.pdf", "FV_profil_FV.pdf", "FV_profil_PV.pdf",
            "FV_reproductibilite.pdf", "FV_npts_emma.pdf", "FV_npts_jeremy.pdf")) {
  cat(sprintf("  %s\n", f))
}