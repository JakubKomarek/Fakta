# Kayova dekompozice emisí CO₂

Analýza vývoje emisí CO₂ pro 50 zemí s nejvyššími emisemi v roce 2024 pomocí Kayovy identity a shlukové analýzy.

## Data

- **EDGAR 2025** — emise CO₂ na úrovni zemí (1970–2024)
- **World Bank WDI** — HDP, populace, spotřeba energie

Data jsou stahována automaticky při spuštění skriptů, není třeba nic instalovat ručně.

## Skripty

### `01_kaya_decomposition.R`
Načte data, provede Kayovu dekompozici pro každou zemi a uloží grafy do `plots/`. Kayova identita rozkládá meziroční změnu CO₂ na příspěvky pěti faktorů:

$$\Delta CO_2 = \Delta \text{Populace} + \Delta \frac{\text{HDP}}{\text{obyvatele}} + \Delta \frac{\text{Energie}}{\text{HDP}} + \Delta \frac{CO_2}{\text{Energie}} + \text{Interakce}$$

Hodnoty jsou vyjádřeny v procentních bodech (p. b.) — tj. příspěvek každého faktoru k roční změně emisí.

### `02_cluster_analysis.R`
Nad maticí průměrných ročních příspěvků (2000–2023) provede k-means shlukovou analýzu. Optimální počet shluků je zvolen na základě WSS elbow metody a průměrné silhouette šířky. Libye je vyřazena jako statistická anomálie způsobená občanskými válkami.

**Výsledné shluky (k = 3):**

| Shluk | Charakteristika | Příklad zemí |
|---|---|---|
| Vývozci fosilních surovin | Vysoký populační růst, rostoucí energetická náročnost | Katar, SAE, Saúdská Arábie aj.|
| Rozvoj průmyslu| Silný růst HDP, emise rostou | Čína, Indie, Vietnam aj.|
| Decoupling | Růst HDP kompenzován poklesem energetické náročnosti, emise stagnují | ČR, Německo, USA, Rusko aj.|

## Výstupy

Grafy dekompozice pro jednotlivé země jsou uloženy v `plots/`, grafy shlukové analýzy jsou generovány přímo v `02_cluster_analysis.R`.

## Závislosti (R balíčky)

```r
install.packages(c("dplyr", "tidyr", "purrr", "ggplot2", "qs2",
                   "cluster", "factoextra"))
```
