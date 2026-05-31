library(dplyr)
library(readxl)
library(WDI)
library(tidyr)
library(ggplot2)
library(countrycode)
library(purrr)
library(qs2)

dir.create("plots", showWarnings = FALSE)


# ── 1. EDGAR – top 70 emitters in 2024 (pool to ensure 50 after drops) ───────
EDGAR <- read_excel("data/IEA_EDGAR_CO2_1970_2024.xlsx", sheet = 4, skip = 9)

top50_names <- EDGAR %>%
  filter(!is.na(Y_2024)) %>%
  filter(!grepl("^Int\\.|World|Global|EU", Name)) %>%
  slice_max(Y_2024, n = 70) %>%
  pull(Name)

# ── 2. Helper: parse EDGAR for one country ───────────────────────────────────
parse_edgar <- function(cn) {
  EDGAR %>%
    filter(Name == cn) %>%
    select(starts_with("Y_")) %>%
    pivot_longer(everything(), names_to = "year", values_to = "co2_mt") %>%
    mutate(year = as.integer(sub("Y_", "", year))) %>%
    filter(year >= 1990, year <= 2024)
}

# ── 3. Helper: fetch WDI for one ISO2 ────────────────────────────────────────
fetch_wdi <- function(iso2) {
  tryCatch(
    WDI(
      country   = iso2,
      indicator = c(
        pop    = "SP.POP.TOTL",
        gdp    = "NY.GDP.MKTP.KD",
        energy = "EG.USE.PCAP.KG.OE"
      ),
      start = 1990, end = 2024
    ) %>%
      select(year, pop, gdp, energy) %>%
      mutate(energy_total = energy * pop / 1e6),
    error = function(e) NULL
  )
}

# ── 4. Helper: Kaya decomposition ────────────────────────────────────────────
kaya_yoy <- function(co2_data, wdi_data) {
  left_join(co2_data, wdi_data, by = "year") %>%
    filter(!is.na(pop), !is.na(gdp), !is.na(energy_total)) %>%
    mutate(
      gdp_per_cap      = gdp / pop,
      energy_intensity = energy_total / gdp,
      carbon_intensity = co2_mt / energy_total
    ) %>%
    arrange(year) %>%
    mutate(
      g_pop    = pop              / lag(pop)              - 1,
      g_gdpc   = gdp_per_cap      / lag(gdp_per_cap)      - 1,
      g_ei     = energy_intensity / lag(energy_intensity) - 1,
      g_ci     = carbon_intensity / lag(carbon_intensity) - 1,
      co2_prev = lag(co2_mt)
    ) %>%
    filter(!is.na(g_pop)) %>%
    mutate(
      d_pop   = g_pop  * co2_prev,
      d_gdpc  = g_gdpc * co2_prev,
      d_ei    = g_ei   * co2_prev,
      d_ci    = g_ci   * co2_prev,
      d_inter = co2_mt - co2_prev - (d_pop + d_gdpc + d_ei + d_ci)
    )
}

# ── 5. Helper: long % format ─────────────────────────────────────────────────
kaya_long_pct <- function(kaya) {
  kaya %>%
    mutate(
      pct_pop   = g_pop  * 100,
      pct_gdpc  = g_gdpc * 100,
      pct_ei    = g_ei   * 100,
      pct_ci    = g_ci   * 100,
      pct_inter = d_inter / co2_prev * 100
    ) %>%
    select(year, pct_pop, pct_gdpc, pct_ei, pct_ci, pct_inter) %>%
    pivot_longer(-year, names_to = "factor", values_to = "pct_change") %>%
    mutate(factor = recode(factor,
      pct_pop   = "Populace",
      pct_gdpc  = "HDP na obyvatele",
      pct_ei    = "Energetická náročnost",
      pct_ci    = "Uhlíková náročnost",
      pct_inter = "Interakce"
    ))
}

# ── 6. Helper: plot (faktaoklimatu.cz visual style) ──────────────────────────
kaya_plot <- function(long_data, total_data, country_name) {

  country_name_cz <- countrycode(country_name,
    origin      = "country.name",
    destination = "cldr.name.cs",
    warn        = FALSE
  )
  if (is.na(country_name_cz)) country_name_cz <- country_name

  fok_colors <- c(
    "Populace"              = "#4C72B0",
    "HDP na obyvatele"      = "#C44E52",
    "Energetická náročnost" = "#55A868",
    "Uhlíková náročnost"    = "#DD8452",
    "Interakce"             = "#8172B3",
    "Celkov\u00e1 zm\u011bna" = "white"
  )

  ggplot(long_data, aes(x = year, y = pct_change, fill = factor)) +
    geom_col(
      position = "stack",
      width     = 0.75,
      colour    = NA
    ) +
    geom_hline(
      yintercept = 0,
      linewidth  = 0.5,
      colour     = "#2D2D2D"
    ) +
    geom_point(
      data        = total_data,
      aes(x = year, y = pct_total, fill = "Celkov\u00e1 zm\u011bna"),
      inherit.aes = FALSE,
      shape       = 21,
      size        = 2.2,
      colour      = "#2D2D2D",
      stroke      = 1.2
    ) +
    scale_fill_manual(
      values = fok_colors,
      breaks = c(
        "Celkov\u00e1 zm\u011bna",
        "Populace",
        "HDP na obyvatele",
        "Energetick\u00e1 n\u00e1ro\u010dnost",
        "Uhl\u00edkov\u00e1 n\u00e1ro\u010dnost",
        "Interakce"
      )
    ) +
    scale_x_continuous(
      breaks = seq(1992, 2024, by = 2),
      expand = expansion(add = c(0.5, 0.5))
    ) +
    scale_y_continuous(
      labels = \(x) paste0(x, "\u00a0p.\u00a0b."),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(
      title    = paste0("Dekompozice Kayovy identity \u2013 ", country_name_cz),
      subtitle = paste0(
        "Meziro\u010dn\u00ed zm\u011bna CO\u2082 rozlo\u017een\u00e1 podle hnac\u00edch sil"
      ),
      x        = NULL,
      y        = NULL,
      fill     = NULL,
      caption  = "Zdroje: EDGAR 2025, World Bank WDI"
    ) +
    theme_minimal(base_size = 12, base_family = "Inter") +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", colour = NA),
      panel.background = element_rect(fill = "#FFFFFF", colour = NA),
      panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text        = element_text(colour = "#555555", size = 10),
      axis.ticks       = element_blank(),
      plot.title       = element_text(
        colour   = "#2D2D2D",
        face     = "bold",
        size     = 15,
        hjust    = 0,
        margin   = margin(b = 4)
      ),
      plot.subtitle    = element_text(
        colour   = "#666666",
        size     = 10,
        hjust    = 0,
        margin   = margin(b = 12)
      ),
      plot.caption     = element_text(
        colour   = "#999999",
        size     = 8.5,
        hjust    = 1,
        margin   = margin(t = 12)
      ),
      plot.margin      = margin(16, 20, 12, 16),
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.text        = element_text(colour = "#444444", size = 9.5),
      legend.key.size    = unit(0.45, "cm"),
      legend.key.spacing.x = unit(0.3, "cm"),
      legend.margin      = margin(t = 4, b = 0),
      legend.title       = element_blank()
    ) +
    guides(
      fill = guide_legend(
        nrow  = 2, byrow = TRUE, order = 1,
        override.aes = list(
          shape  = c(21, 22, 22, 22, 22, 22),
          colour = c("#2D2D2D", NA, NA, NA, NA, NA),
          size   = c(2.5,  4,  4,  4,  4,  4)
        )
      )
    )
}


# ══════════════════════════════════════════════════════════════════════════════
# PHASE 1 — Build and save all data
# ══════════════════════════════════════════════════════════════════════════════

kaya_all <- map(top50, function(cn) {
  iso2 <- case_match(cn,
    "Libyan Arab Jamahiriya" ~ "LY",
    "Singapore"              ~ "SG",
    .default = countrycode(cn, origin = "country.name", destination = "iso2c", warn = FALSE)
  )
  if (is.na(iso2)) {
    message("Skipping (no ISO2): ", cn)
    return(NULL)
  }

  co2 <- parse_edgar(cn)
  wdi <- fetch_wdi(iso2)

  if (is.null(wdi) || nrow(wdi) == 0) {
    message("Skipping (no WDI): ", cn)
    return(NULL)
  }

  kaya <- kaya_yoy(co2, wdi)

  if (nrow(kaya) < 5) {
    message("Skipping (too few rows): ", cn)
    return(NULL)
  }

  long  <- kaya_long_pct(kaya)
  total <- kaya %>% mutate(pct_total = (co2_mt - co2_prev) / co2_prev * 100)

  message("Data ready: ", cn)
  list(
    country = cn,
    co2     = co2,
    wdi     = wdi,
    kaya    = kaya,
    long    = long,
    total   = total
  )
}) |>
  setNames(top50) |>
  purrr::compact()

# ── Trim to top 50 by 2024 CO2 ───────────────────────────────────────────────
kaya_all <- kaya_all[
  order(
    sapply(kaya_all, \(e) tail(e$co2$co2_mt[!is.na(e$co2$co2_mt)], 1)),
    decreasing = TRUE
  )
] |>
  head(50)

message("Final country count: ", length(kaya_all))

qs_save(kaya_all, "data/kaya_top50.qs2")
message("All data saved to data/kaya_top50.qs2")

# ── NA check ─────────────────────────────────────────────────────────────────



# ══════════════════════════════════════════════════════════════════════════════
# PHASE 2 — Generate plots from saved data
# ══════════════════════════════════════════════════════════════════════════════

walk(kaya_all, function(entry) {
  p    <- kaya_plot(entry$long, entry$total, entry$country)
  slug <- gsub("[^A-Za-z0-9]", "_", entry$country)
  ggsave(
    filename = paste0("plots/kaya_", slug, ".png"),
    plot     = p,
    width    = 10, height = 6, dpi = 150
  )
  message("Saved plot: ", entry$country)
})






qs_save(kaya_all, "data/kaya_top50.qs2")
message("Done — ", length(kaya_all), " countries")