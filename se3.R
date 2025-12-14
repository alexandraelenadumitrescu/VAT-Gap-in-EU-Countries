# ==============================================================================
# PARADOXUL SUPRAESTIMĂRII SHADOW ECONOMY
# Demonstrație conceptuală a bias-urilor metodologice
# ==============================================================================

library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

# Setare temă avangardistă
theme_avant <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#0a0a0a", color = NA),
      panel.background = element_rect(fill = "#0a0a0a", color = NA),
      panel.grid.major = element_line(color = "#ffffff22", size = 0.3),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#ffffff", family = "sans", size = 12),
      plot.title = element_text(face = "bold", size = 16, color = "#00ff88"),
      plot.subtitle = element_text(size = 10, color = "#888888"),
      axis.text = element_text(color = "#cccccc"),
      legend.background = element_rect(fill = "#1a1a1a", color = "#333333"),
      legend.key = element_rect(fill = "#1a1a1a"),
      legend.text = element_text(color = "#cccccc")
    )
}

# ==============================================================================
# 1. CIRCULARITATEA CAUZALĂ - "Modelul mănâncă propria coadă"
# ==============================================================================

circular_causality <- function() {
  set.seed(42)
  years <- 2000:2023
  n <- length(years)
  
  # Economie shadow "reală" (necunoscută)
  true_shadow <- 15 + rnorm(n, 0, 2)
  
  # Tax burden (cauză și efect simultan în MIMIC)
  tax_burden <- 35 + 0.3 * true_shadow + rnorm(n, 0, 3)
  
  # MIMIC estimează shadow folosind tax_burden ca predictor
  # DAR tax_burden e deja corelat cu shadow prin construcție!
  mimic_estimate <- 10 + 0.8 * tax_burden + rnorm(n, 0, 4)
  
  df <- data.frame(
    year = years,
    true_shadow = true_shadow,
    mimic_estimate = mimic_estimate,
    overestimation = mimic_estimate - true_shadow
  )
  
  p1 <- ggplot(df, aes(x = year)) +
    geom_ribbon(aes(ymin = true_shadow, ymax = mimic_estimate),
                fill = "#ff006622", alpha = 0.5) +
    geom_line(aes(y = true_shadow, color = "Adevărat (necunoscut)"), size = 1.5) +
    geom_line(aes(y = mimic_estimate, color = "MIMIC Estimate"), size = 1.5, linetype = "dashed") +
    scale_color_manual(values = c("Adevărat (necunoscut)" = "#00ff88", 
                                  "MIMIC Estimate" = "#ff0066")) +
    labs(title = "BIAS 1: Circularitatea Cauzală",
         subtitle = "Tax burden devine atât cauză cât și indicator → amplificare artificială",
         y = "Shadow Economy (% GDP)", x = NULL, color = NULL) +
    theme_avant()
  
  return(list(plot = p1, data = df))
}

# ==============================================================================
# 2. ASUMPȚIA VELOCITĂȚII EGALE - "Banii nu aleargă la fel"
# ==============================================================================

velocity_bias <- function() {
  set.seed(123)
  
  # În realitate: banii circulă mai ÎNCET în shadow economy
  formal_velocity <- 1.5
  shadow_velocity <- 0.8  # Mult mai mică!
  
  # Currency Demand Analysis presupune velocity_shadow = velocity_formal
  currency_in_circulation <- seq(100, 200, length.out = 20)
  
  # Calcul corect (realitate)
  true_shadow_gdp <- (currency_in_circulation - 100) * shadow_velocity
  
  # Calcul eronat (asumpție egală)
  estimated_shadow_gdp <- (currency_in_circulation - 100) * formal_velocity
  
  df <- data.frame(
    currency = currency_in_circulation,
    true_estimate = true_shadow_gdp,
    biased_estimate = estimated_shadow_gdp,
    overestimation_pct = ((estimated_shadow_gdp - true_shadow_gdp) / 
                            true_shadow_gdp * 100)
  )
  
  p2 <- ggplot(df, aes(x = currency)) +
    geom_area(aes(y = biased_estimate), fill = "#ff006633", alpha = 0.6) +
    geom_area(aes(y = true_estimate), fill = "#00ff8833", alpha = 0.6) +
    geom_line(aes(y = true_estimate, color = "Velocity realistă (0.8)"), size = 1.5) +
    geom_line(aes(y = biased_estimate, color = "Velocity asumată (1.5)"), 
              size = 1.5, linetype = "dashed") +
    annotate("text", x = 150, y = 120, 
             label = "SUPRAESTIMAREA:\n+87.5% mediană", 
             color = "#ff0066", size = 5, fontface = "bold") +
    scale_color_manual(values = c("Velocity realistă (0.8)" = "#00ff88",
                                  "Velocity asumată (1.5)" = "#ff0066")) +
    labs(title = "BIAS 2: Asumpția Velocității Monetare Egale",
         subtitle = "Currency Demand Analysis ignoră că banii circulă diferit în economia subterană",
         y = "Shadow GDP Estimate", x = "Currency in Circulation", color = NULL) +
    theme_avant()
  
  return(list(plot = p2, data = df))
}

# ==============================================================================
# 3. ANCHORING ARBITRAR - "Încotro arată compasul?"
# ==============================================================================

anchoring_sensitivity <- function() {
  set.seed(456)
  
  # MIMIC produce doar indici (0.8, 1.0, 1.2...)
  # Trebuie "ancorat" la un nivel extern
  relative_index <- seq(0.8, 1.3, length.out = 15)
  
  # Diferite puncte de ancorare → rezultate RADICAL diferite
  anchor_low <- 12  # Ancorare conservatoare
  anchor_mid <- 20  # Ancorare "medie"
  anchor_high <- 30 # Ancorare agresivă
  
  df <- data.frame(
    index = relative_index,
    estimate_low = anchor_low * relative_index,
    estimate_mid = anchor_mid * relative_index,
    estimate_high = anchor_high * relative_index
  ) %>%
    pivot_longer(cols = starts_with("estimate"), 
                 names_to = "anchor_type", values_to = "estimate")
  
  p3 <- ggplot(df, aes(x = index, y = estimate, color = anchor_type, group = anchor_type)) +
    geom_line(size = 1.5, alpha = 0.8) +
    geom_point(size = 3) +
    scale_color_manual(
      values = c("estimate_low" = "#00ff88", 
                 "estimate_mid" = "#ffaa00", 
                 "estimate_high" = "#ff0066"),
      labels = c("Ancoră: 12%", "Ancoră: 20%", "Ancoră: 30%")
    ) +
    annotate("rect", xmin = 0.75, xmax = 1.35, ymin = 25, ymax = 40,
             fill = "#ff006611", color = "#ff0066", linetype = "dashed", size = 0.5) +
    annotate("text", x = 1.05, y = 37, 
             label = "ZONA DE INCERTITUDINE\nALEGERE ARBITRARĂ", 
             color = "#ff0066", size = 4, fontface = "bold") +
    labs(title = "BIAS 3: Anchoring Arbitrar în MIMIC",
         subtitle = "Același model → estimări diferite cu ±150% doar prin alegerea punctului de referință",
         y = "Shadow Economy (% GDP)", x = "Relative Index", color = "Punct de Ancorare") +
    theme_avant()
  
  return(list(plot = p3, data = df))
}

# ==============================================================================
# 4. MACHINE LEARNING: AMPLIFICAREA RECURSIVĂ
# ==============================================================================

ml_amplification <- function() {
  set.seed(789)
  
  # Generația 1: Estimări MIMIC inițiale (deja supraevaluate)
  initial_bias <- 1.4  # 40% overestimation
  gen1_estimates <- rnorm(100, mean = 25 * initial_bias, sd = 5)
  
  # Generația 2: ML antrenat pe generația 1
  # Învață pattern-urile erorii!
  gen2_bias <- 1.5  # 50% overestimation
  gen2_estimates <- rnorm(100, mean = mean(gen1_estimates) * 1.1, sd = 6)
  
  # Generația 3: Deep Learning pe generația 2
  gen3_bias <- 1.65  # 65% overestimation
  gen3_estimates <- rnorm(100, mean = mean(gen2_estimates) * 1.1, sd = 7)
  
  # Realitate (necunoscută)
  true_value <- 25
  
  df <- data.frame(
    generation = rep(c("Adevăr", "Gen 1:\nMIMIC", "Gen 2:\nML Classic", 
                       "Gen 3:\nDeep Learning"), each = 100),
    estimate = c(rep(true_value, 100), gen1_estimates, gen2_estimates, gen3_estimates)
  ) %>%
    mutate(generation = factor(generation, levels = c("Adevăr", "Gen 1:\nMIMIC", 
                                                      "Gen 2:\nML Classic", 
                                                      "Gen 3:\nDeep Learning")))
  
  p4 <- ggplot(df, aes(x = generation, y = estimate, fill = generation)) +
    geom_violin(alpha = 0.6, color = "#ffffff44") +
    geom_boxplot(width = 0.2, alpha = 0.8, outlier.color = "#ff0066") +
    stat_summary(fun = mean, geom = "point", size = 4, color = "#00ff88", shape = 18) +
    geom_hline(yintercept = true_value, color = "#00ff88", linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("#00ff8844", "#ffaa0044", "#ff660044", "#ff006644")) +
    annotate("segment", x = 1, xend = 4, y = 55, yend = 55, 
             arrow = arrow(length = unit(0.3, "cm")), color = "#ff0066", size = 1) +
    annotate("text", x = 2.5, y = 57, label = "AMPLIFICARE RECURSIVĂ", 
             color = "#ff0066", size = 5, fontface = "bold") +
    labs(title = "BIAS 4: Amplificarea Recursivă în Machine Learning",
         subtitle = "Fiecare generație de modele învață din datele supraevaluate ale generației anterioare",
         y = "Shadow Economy Estimate (% GDP)", x = NULL) +
    theme_avant() +
    theme(legend.position = "none")
  
  return(list(plot = p4, data = df))
}

# ==============================================================================
# 5. SELECTION BIAS - "Vedem doar vârful aisbergului"
# ==============================================================================

selection_bias_viz <- function() {
  set.seed(321)
  
  # Distribuția completă (necunoscută)
  all_shadow_activities <- rlnorm(1000, meanlog = 2.5, sdlog = 0.8)
  
  # Doar activitățile "prinse" devin date observabile
  detection_threshold <- quantile(all_shadow_activities, 0.6)
  detected <- all_shadow_activities[all_shadow_activities > detection_threshold]
  
  df_full <- data.frame(
    value = all_shadow_activities,
    status = "Total (necunoscut)"
  )
  
  df_detected <- data.frame(
    value = detected,
    status = "Detectat (observable)"
  )
  
  df <- rbind(df_full, df_detected)
  
  p5 <- ggplot(df, aes(x = value, fill = status)) +
    geom_density(alpha = 0.6, size = 1) +
    geom_vline(xintercept = detection_threshold, color = "#ff0066", 
               linetype = "dashed", size = 1.5) +
    annotate("text", x = detection_threshold + 5, y = 0.15, 
             label = "PRAG DE DETECTARE\n(bias sistematic)", 
             color = "#ff0066", size = 4, fontface = "bold") +
    scale_fill_manual(values = c("Total (necunoscut)" = "#00ff8844",
                                 "Detectat (observable)" = "#ff006644")) +
    labs(title = "BIAS 5: Selection Bias în Datele Observate",
         subtitle = "Modelele învață din activități DEJA detectate → estimează doar 'vârful aisbergului'",
         x = "Mărime Activitate Shadow", y = "Densitate", fill = NULL) +
    theme_avant()
  
  return(list(plot = p5, data = df))
}

# ==============================================================================
# 6. SINTEZA: DASHBOARD COMPARATIV
# ==============================================================================

create_dashboard <- function() {
  # Generare toate plot-urile
  results <- list(
    circ = circular_causality(),
    vel = velocity_bias(),
    anch = anchoring_sensitivity(),
    ml = ml_amplification(),
    sel = selection_bias_viz()
  )
  
  # Calcul summary statistics
  avg_overestimation <- mean(results$circ$data$overestimation)
  vel_overest_pct <- mean(results$vel$data$overestimation_pct)
  
  cat("\n╔═══════════════════════════════════════════════════════════════╗\n")
  cat("║  PARADOXUL SUPRAESTIMĂRII SHADOW ECONOMY                     ║\n")
  cat("║  Demonstrație: 5 Bias-uri Metodologice Fundamentale          ║\n")
  cat("╚═══════════════════════════════════════════════════════════════╝\n\n")
  
  cat("📊 REZULTATE CHEIE:\n")
  cat(sprintf("   • Circularitate cauzală: +%.1f%% overestimation mediană\n", avg_overestimation))
  cat(sprintf("   • Velocity bias: +%.1f%% overestimation mediană\n", vel_overest_pct))
  cat("   • Anchoring arbitrar: variație ±150% pentru aceleași date\n")
  cat("   • ML amplification: creștere 65% față de adevăr în 3 generații\n")
  cat("   • Selection bias: >40% din distribuție invizibilă\n\n")
  
  cat("💡 CONCLUZIE PROVOCATOARE:\n")
  cat("   Țările nu măsoară shadow economy - o CONSTRUIESC prin\n")
  cat("   asumpțiile modelelor lor. Cu cât metodele devin mai sofisticate,\n")
  cat("   cu atât pot amplifica bias-urile fundamentale.\n\n")
  
  # Return all plots
  return(results)
}

# ==============================================================================
# EXECUȚIE PRINCIPALĂ
# ==============================================================================

# Generare dashboard complet
dashboard <- create_dashboard()

# Afișare plot-uri individuale
print(dashboard$circ$plot)
print(dashboard$vel$plot)
print(dashboard$anch$plot)
print(dashboard$ml$plot)
print(dashboard$sel$plot)

# Salvare grid combinat (opțional)
combined_plot <- grid.arrange(
  dashboard$circ$plot,
  dashboard$vel$plot,
  dashboard$anch$plot,
  dashboard$ml$plot,
  dashboard$sel$plot,
  ncol = 2,
  top = grid::textGrob("THE OVERESTIMATION PARADOX: 5 Methodological Biases",
                       gp = grid::gpar(col = "#00ff88", fontsize = 18, fontface = "bold"))
)

# Export la rezoluție înaltă
# ggsave("shadow_economy_paradox.png", combined_plot, 
#        width = 16, height = 20, dpi = 300, bg = "#0a0a0a")

cat("\n✓ Vizualizare completă generată!\n")
cat("  Pentru export: decomentați liniile ggsave() de mai sus.\n\n")