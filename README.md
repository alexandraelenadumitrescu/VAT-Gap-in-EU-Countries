# VAT Gap in EU Countries

Proiect de econometrie (2025-2026) despre determinanții VAT Compliance Gap
(diferența dintre TVA teoretic colectabil și TVA efectiv colectat) în țările
Uniunii Europene. Analiza combină date cross-section și panel privind economia
subterană (Shadow Economy / MIMIC), digitalizarea (DESI, eGovernment), guvernanța
(WGI) și capitalul uman, folosind regresie liniară, modele cu efecte fixe,
regularizare (Lasso/Ridge) și clustering.

## Structură

```
data/
  raw/            Seturile de date originale (indicatori pe țară/an)
  processed/      Seturi de date derivate, generate de scripturile de analiză
scripts/
  main_analysis_pipeline.R   Pipeline-ul final și complet: curățare date, EDA,
                              regresie, Lasso/Ridge, clustering, efecte fixe,
                              forecast. Generează tot ce se află în data/processed/,
                              tables/, figures/ și output/.
  descriptive_statistics/    Analiza statistică descriptivă (contribuție separată)
  exploratory/               Istoricul de lucru: versiuni iterative ale modelelor,
                              păstrate pentru transparența procesului, grupate pe teme:
    01_data_collection/        Descărcare date brute (Eurostat)
    02_univariate_screening/   Testarea individuală a fiecărui indicator candidat
    03_cross_sectional_models/ Iterații ale modelului de regresie cross-section
    04_shadow_economy_analysis/ Analize dedicate economiei subterane / MIMIC
    05_interaction_effects/    Modele cu termeni de interacțiune și transformări log
    06_panel_data/             Modele cu date de tip panel (efecte fixe)
    07_drafts/                 Fragmente de cod nefinalizate
  visualization/              Script Python pentru harta VAT Gap
figures/          Toate graficele generate (PNG)
tables/           Tabele de rezultate (CSV/HTML/TXT)
output/           Raportul final de analiză
```

Scripturile din `scripts/exploratory/` documentează procesul iterativ de
construire a modelului și pot conține căi de fișier absolute din mediile
locale ale autorilor inițiali — pipeline-ul de referință, gata de rulat, este
`scripts/main_analysis_pipeline.R`.

## Rulare

Deschide `vat-gap-eu.Rproj` în RStudio (working directory = rădăcina
proiectului) și rulează `scripts/main_analysis_pipeline.R`.
