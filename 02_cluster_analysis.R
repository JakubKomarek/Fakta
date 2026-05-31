library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(qs2)
library(cluster)      # silhouette
library(factoextra)   # fviz_nbclust, fviz_cluster


# ── 1. Load saved data ────────────────────────────────────────────────────────
kaya_all <- qs_read("data/kaya_top50.qs2")


kaya_21 <- map(kaya_all, function(entry) {
  entry$long  <- entry$long  %>% filter(year >= 2000, year < 2024)
  entry$total <- entry$total %>% filter(year >= 2000, year < 2024)
  entry$kaya  <- entry$kaya  %>% filter(year >= 2000, year < 2024)
  entry
})

# ── 2. Build feature matrix: mean factor contributions per country ────────────
factor_means <- map_dfr(kaya_21, function(entry) {
  entry$long %>%
    group_by(factor) %>%
    summarise(mean_pct = mean(pct_change, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = factor, values_from = mean_pct) %>%
    mutate(
      country    = entry$country,
      total_chg  = mean(entry$total$pct_total, na.rm = TRUE)
    )
}) %>%
  relocate(country) %>%
  # clean column names to ASCII-safe
  rename(
    pop   = "Populace",
    gdpc  = "HDP na obyvatele",
    ei    = "Energetická náročnost",
    ci    = "Uhlíková náročnost",
    inter = "Interakce"
  )

# matrix for clustering (drop country label, scale; exclude outliers)
X <- factor_means %>%
  filter(country != "Libyan Arab Jamahiriya") %>%
  select(pop, gdpc, ei, ci, inter, total_chg) %>%
  as.matrix()

rownames(X) <- factor_means %>%
  filter(country != "Libyan Arab Jamahiriya") %>%
  pull(country)

X_scaled <- scale(X)

# ── 3. Elbow diagnostics: WSS + silhouette for k = 2..10 ─────────────────────
k_max <- 10

wss <- map_dbl(2:k_max, function(k) {
  km <- kmeans(X_scaled, centers = k, nstart = 50, iter.max = 300)
  km$tot.withinss
})

sil <- map_dbl(2:k_max, function(k) {
  km  <- kmeans(X_scaled, centers = k, nstart = 50, iter.max = 300)
  ss  <- silhouette(km$cluster, dist(X_scaled))
  mean(ss[, "sil_width"])
})

diag_df <- tibble(
  k         = 2:k_max,
  wss       = wss,
  silhouette = sil
)

# ── 4. Elbow plot ─────────────────────────────────────────────────────────────
p_wss <- ggplot(diag_df, aes(x = k, y = wss)) +
  geom_line(colour = "#4C72B0", linewidth = 0.9) +
  geom_point(colour = "#4C72B0", size = 3) +
  scale_x_continuous(breaks = 2:k_max) +
  labs(
    title    = "Elbow — celková vnitro-skupinová suma čtverců (WSS)",
    subtitle = "Hledejte koleno: kde přírůstek WSS začíná klesat nejrychleji",
    x        = "Počet shluků k",
    y        = "WSS",
    caption  = "Zdroje: EDGAR 2025, World Bank WDI • faktaoklimatu.cz"
  ) +
  theme_minimal(base_size = 12, base_family = "Helvetica Neue") +
  theme(
    plot.background    = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background   = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks         = element_blank(),
    plot.title         = element_text(colour = "#2D2D2D", face = "bold",
                                      size = 14, hjust = 0),
    plot.subtitle      = element_text(colour = "#666666", size = 10, hjust = 0),
    plot.caption       = element_text(colour = "#999999", size = 8.5, hjust = 1),
    plot.margin        = margin(16, 20, 12, 16)
  )

p_sil <- ggplot(diag_df, aes(x = k, y = silhouette)) +
  geom_line(colour = "#C44E52", linewidth = 0.9) +
  geom_point(colour = "#C44E52", size = 3) +
  geom_point(
    data = diag_df %>% filter(silhouette == max(silhouette)),
    colour = "#C44E52", size = 5, shape = 21, fill = "white", stroke = 1.5
  ) +
  scale_x_continuous(breaks = 2:k_max) +
  labs(
    title    = "Průměrná šířka silhouette",
    subtitle = "Vyšší = lépe oddělené shluky; kroužek označuje maximum",
    x        = "Počet shluků k",
    y        = "Průměrná silhouette",
    caption  = "Zdroje: EDGAR 2025, World Bank WDI • faktaoklimatu.cz"
  ) +
  theme_minimal(base_size = 12, base_family = "Helvetica Neue") +
  theme(
    plot.background    = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background   = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.ticks         = element_blank(),
    plot.title         = element_text(colour = "#2D2D2D", face = "bold",
                                      size = 14, hjust = 0),
    plot.subtitle      = element_text(colour = "#666666", size = 10, hjust = 0),
    plot.caption       = element_text(colour = "#999999", size = 8.5, hjust = 1),
    plot.margin        = margin(16, 20, 12, 16)
  )


print(p_wss)
print(p_sil)
message("Elbow plots saved — choose k and re-run from step 5 onwards.")

print(diag_df)

# ── 5. !! SET k HERE after inspecting the elbow plots !!  ────────────────────
k_chosen <- 3   # <-- change this

# ── 6. Final k-means clustering ───────────────────────────────────────────────
set.seed(42)
km_final <- kmeans(X_scaled, centers = k_chosen, nstart = 100, iter.max = 500)

# ── 7. Cluster assignment table ───────────────────────────────────────────────
assignments <- factor_means %>%
  filter(country != "Libyan Arab Jamahiriya") %>%
  mutate(cluster = km_final$cluster) %>%
  arrange(cluster, country)

message("\n── Cluster assignments ──")
print(assignments %>% select(cluster, country))

# ── 8. Cluster summary stats ──────────────────────────────────────────────────
cluster_summary <- assignments %>%
  group_by(cluster) %>%
  summarise(
    n_countries   = n(),
    countries     = paste(sort(country), collapse = " | "),
    mean_pop      = round(mean(pop),       2),
    mean_gdpc     = round(mean(gdpc),      2),
    mean_ei       = round(mean(ei),        2),
    mean_ci       = round(mean(ci),        2),
    mean_inter    = round(mean(inter),     2),
    mean_total    = round(mean(total_chg), 2),
    .groups = "drop"
  )

message("\n── Cluster summary (mean factor contributions, %) ──")
print(cluster_summary %>% select(-countries), n = Inf)

message("\n── Countries per cluster ──")
cluster_summary %>%
  select(cluster, n_countries, countries) %>%
  print(n = Inf)

# ── 9. Cluster profile plot: radar-style bar facets ──────────────────────────
profile_long <- assignments %>%
  select(cluster, country, pop, gdpc, ei, ci, inter, total_chg) %>%
  pivot_longer(c(pop, gdpc, ei, ci, inter, total_chg),
               names_to = "factor", values_to = "pct") %>%
  group_by(cluster, factor) %>%
  summarise(mean_pct = mean(pct), .groups = "drop") %>%
  mutate(
    factor = recode(factor,
      pop       = "Populace",
      gdpc      = "HDP na obyvatele",
      ei        = "Energetická náročnost",
      ci        = "Uhlíková náročnost",
      inter     = "Interakce",
      total_chg = "Celková změna"
    ),
    cluster_label = recode(as.character(cluster),
      `1` = "Exportéři fosilních surovin",
      `2` = "Rozvoj průmyslu",
      `3` = "Decoupling"
    )
  )

fok_colors <- c(
  "Populace"              = "#4C72B0",
  "HDP na obyvatele"      = "#C44E52",
  "Energetická náročnost" = "#55A868",
  "Uhlíková náročnost"    = "#DD8452",
  "Interakce"             = "#8172B3",
  "Celková změna"         = "#937860"
)

p_profile <- ggplot(profile_long,
                    aes(x = factor, y = mean_pct, fill = factor)) +
  geom_col(width = 0.65, colour = NA) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "#2D2D2D") +
  facet_wrap(~ cluster_label, nrow = 1) +
  scale_fill_manual(values = fok_colors, name = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(labels = \(x) paste0(x, " p. b.")) +
  labs(
    title    = paste0("Profily shluků Kayovy dekompozice"),
    subtitle = "Průměrný příspěvek každého faktoru v rámci shluku zemí",
    x        = NULL,
    y        = "Příspěvek k roční změně CO₂",
    caption  = "Zdroje: EDGAR 2025, World Bank WDI"
  ) +
  theme_minimal(base_size = 12, base_family = "Helvetica Neue") +
  theme(
    plot.background    = element_rect(fill = "#FFFFFF", colour = NA),
    panel.background   = element_rect(fill = "#FFFFFF", colour = NA),
    panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_blank(),
    axis.ticks.x       = element_blank(),
    axis.text.y        = element_text(colour = "#555555", size = 9),
    axis.ticks         = element_blank(),
    strip.text         = element_text(colour = "#2D2D2D", face = "bold", size = 11),
    legend.position    = "bottom",
    legend.text        = element_text(colour = "#555555", size = 9),
    legend.key.size    = unit(0.75, "lines"),
    plot.title         = element_text(colour = "#2D2D2D", face = "bold",
                                      size = 14, hjust = 0),
    plot.subtitle      = element_text(colour = "#666666", size = 10, hjust = 0),
    plot.caption       = element_text(colour = "#999999", size = 8.5, hjust = 1),
    plot.margin        = margin(16, 20, 12, 16)
  )

p_profile





print(map_dfr(kaya_all, function(entry) {
  tibble(
    country   = entry$country,
    na_co2    = sum(is.na(entry$co2$co2_mt)),
    na_pop    = sum(is.na(entry$kaya$pop)),
    na_gdp    = sum(is.na(entry$kaya$gdp)),
    na_energy = sum(is.na(entry$kaya$energy_total)),
    na_long   = sum(is.na(entry$long$pct_change)),
    n_years   = nrow(entry$kaya)
  )
}), n = 50)
