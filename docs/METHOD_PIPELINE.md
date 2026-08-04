# How the pieces fit together

You did **not** forget LDA. It lives in a **different script** than the final party time-series plots.

## Big picture (what you actually did)

```text
 ~1.59M party tweets (2012–2018)
              │
              ▼
   Preprocess (quanteda): tokenize, stopwords,
   noise dictionaries, optional stemming
              │
              ▼
   Build DFM + trim rare/common terms
              │
              ├─► LDA #1  seededlda::textmodel_lda  (k = 10, k = 5)
              │
              └─► LDA #2  topicmodels::LDA Gibbs   (K = 5, 500 iter, seed = 1)
                        → terms(), beta, theta
              │
              ▼
   Interpret top terms per topic
   → hand-copy keyword lists (topic1…topic5 / topic7)
              │
              ▼
   Per party: tokens_keep(topic_words)
   → monthly hit counts → ggplot time series + party shares
```

That two-step design is standard in CSS:

1. **Unsupervised discovery** (LDA on the full corpus)  
2. **Comparative tracking** (dictionary hits by party × month)

---

## Which file is which

| File | Role |
|------|------|
| **`LDA_Final_DictionaryWords-2.R`** | **Main pipeline.** Header: tweets 2012/2018, **N = 1,588,399**. Preprocess all parties → bind corpus → DFM trim → **real LDA** → then topic keyword tracking + plots. |
| **`LDA_Final-3.R`** | Shorter branch: preprocess + keyword tracking only (topics like Polizei/Deutschland). Title says LDA; model fit not in this file. |
| **`LDA_Final_Dic_T3.R`** | Snippet for **Topic 3 only** (depends on objects from a previous session). |
| **`CSS_FinalCode.R`** | Later / alternate **stage-2** script: five different top-word lists + full party comparison. **No `textmodel_lda` / `LDA()` call** — assumes topics already chosen. |

So: LDA was run in **`LDA_Final_DictionaryWords-2.R`**.  
`CSS_FinalCode.R` is the **downstream** analysis you kept refining.

---

## Exact LDA calls (from DictionaryWords script)

```r
# quanteda / seededlda
library(seededlda)
tmod_lda_e1 <- textmodel_lda(Parteien_seq_e1, k = 10)
terms(tmod_lda_e1, 10)

tmod_lda_e2 <- textmodel_lda(Parteien_seq_e1, k = 5)
terms(tmod_lda_e2, 10)

# classic topicmodels Gibbs (K = 5)
library(topicmodels)   # provides LDA()
K <- 5
tmod_lda <- LDA(DTM_1, K, method = "Gibbs",
                control = list(iter = 500, seed = 1, verbose = 25))
terms(tmod_lda, 15)
posterior(tmod_lda)    # beta (terms|topic), theta (topics|doc)
```

DFM prep before LDA: `dfm_trim` (quantile / docfreq filters) on the **pooled** party corpus.

---

## Topics after LDA (DictionaryWords stage-2 lists)

These are the lists you tracked after reading `terms()`:

| Topic | Keywords (as in script) | Plot title you used |
|-------|-------------------------|---------------------|
| 1 | gesetz, fordert, fall, entscheidung, kritik, antwort | Gesetz / Kritik / Entscheidung |
| 2 | deutschland, euro | Deutschland und Euro |
| 3 | bundesregierung, fordert | Bundesregierung |
| 4 | deutschland, europa, deutsche, eu, welt, flüchtlinge, land, deutschen, türkei | DE/EU/Migration |
| 5 | thema, bundestag | Bundestag |
| 7 | frauen, leben, deutschland, demokratie, gewalt, opfer, polizei, freiheit, nazis, gesellschaft | Demokratie / Gewalt |

`CSS_FinalCode.R` uses **another** set of 5×10-word lists (likely from a different `terms()` run or k=10 cut). Same **method**, different keyword freeze.

---

## “Big Data” — what you can say honestly

- **Scale:** ~**1.6 million** political tweets, multi-year (2012–2018), six parties.  
- **Compute:** full-corpus DFM + LDA (Gibbs 500 iter) + per-party longitudinal aggregation.  
- In a TUM CSS course that **is** the big-data / computational component — not Spark, but large-scale text.

---

## Portfolio wording (safe)

> Built an NLP pipeline on ~1.6M German party tweets (2012–2018): quanteda preprocessing, LDA topic discovery (seededlda + Gibbs LDA, K=5/10), then tracked LDA-derived topic keywords over time by party.

Avoid claiming pure end-to-end LDA document classification unless you also export θ by party/month (you mostly used top-term dictionaries after LDA).
