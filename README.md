# Party Discourse in German Political Communication (R / quanteda / LDA)

**Course project · Computational Social Science · TUM**  
**Stack:** R · quanteda · seededlda · topicmodels (Gibbs LDA) · dplyr · ggplot2

Large-scale text analysis of German party tweets (**~1.59 million** documents, **2012–2018**, six parties). Two-stage design:

1. **LDA topic discovery** on the pooled corpus (`textmodel_lda` k=5/10 + Gibbs LDA K=5)
2. **Longitudinal tracking** of LDA-derived top terms by party and month

> Method note: Stage 2 uses **keyword lists taken from LDA `terms()` output**, then counts hits over time. That is a common CSS workflow (discover → interpret → compare), not pure document-level LDA classification alone.

See `docs/METHOD_PIPELINE.md` for how the original course scripts map to this repo.

---

## Research question

Which latent themes structure German party communication on social media, and how do parties differ in **emphasis on those themes over time**?

---

## Pipeline

1. **Ingest** party CSVs from `data/raw/<party>/` (~1.6M tweets in the original study)
2. **Preprocess** with quanteda (URLs, @/#, DE+EN stopwords, noise dictionaries, optional stemming)
3. **LDA** on pooled DFM (`analysis/02_lda_stage.R`) — top terms → `output/lda/`
4. **Track** topic keywords per party, monthly (`analysis/01_analysis.R`)
5. **Visualize** time series + party shares → `output/figures/`

### Example topics after LDA (course script)

| Topic | Focus | Example keywords |
|-------|--------|------------------|
| 1 | Law / critique | gesetz, fordert, entscheidung, kritik |
| 2 | DE / Euro | deutschland, euro |
| 3 | Government | bundesregierung, fordert |
| 4 | Europe / migration | europa, flüchtlinge, türkei, eu |
| 5 | Parliament | bundestag, thema |
| 7 | Democracy / violence | demokratie, polizei, gewalt, freiheit |

---

## Project structure

```text
populist-party-discourse-r/
├── README.md
├── docs/METHOD_PIPELINE.md   # maps original LDA_*.R + CSS_FinalCode.R
├── analysis/
│   ├── 00_original_course_script.R   # archive: CSS_FinalCode.R
│   ├── 01_analysis.R                 # stage 2: topic tracking + plots
│   ├── 02_lda_stage.R                # stage 1: LDA fit + export top terms
│   └── original_lda/                 # archived course LDA scripts
├── data/raw/                 # gitignored — place CSVs here
├── output/figures/           # plots
├── output/lda/               # terms tables + model RDS
└── portfolio/                # GitHub Pages / CV snippets
```

---

## How to run

```r
install.packages(c(
  "ggplot2", "dplyr", "tidyr", "lubridate", "tibble", "stringr",
  "quanteda", "quanteda.textmodels", "quanteda.textstats",
  "readtext", "seededlda", "topicmodels"
))

setwd("path/to/populist-party-discourse-r")

# Stage 1 — LDA (needs full or large sample under data/raw/)
source("analysis/02_lda_stage.R")

# Stage 2 — track dictionaries (update topic lists from output/lda/ if re-fit)
source("analysis/01_analysis.R")
```

---

## Original course files (what did what)

| Original file | Role |
|---------------|------|
| `LDA_Final_DictionaryWords-2.R` | **Main:** N≈1.59M, preprocess, **LDA**, then keyword time series |
| `LDA_Final-3.R` / `LDA_Final_Dic_T3.R` | Partial stage-2 branches |
| `CSS_FinalCode.R` | Alternate stage-2 with different top-word freeze |

---

## Portfolio

- `portfolio/projects-entry.md` — paste into R section on GitHub Pages  
- `portfolio/cv-teaser.md` — one CV line  
- `portfolio/recruiter-talking-points.md` — only if asked about cleanup  

---

## Limitations (honest)

- LDA topics are stochastic; seed fixed in Gibbs run (`seed = 1`) but re-fits can differ slightly.
- Stage 2 keyword hits ≠ full θ (document-topic) time series — simpler and interpretable.
- Absolute frequencies scale with party activity; shares help within-topic comparison.

---

## Course poster (graded)

![Course poster](output/poster/Poster_CSS_Final.png)

- Preview (PNG): [`output/figures/Poster_CSS_Final.png`](output/figures/Poster_CSS_Final.png)

## Author

Augusto Teixeira · TUM Computational Social Science course project
