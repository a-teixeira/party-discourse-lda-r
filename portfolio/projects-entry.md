<!--
Paste into the R section of projects.md on a-teixeira.github.io
Replace the placeholder block under ## R
Update the GitHub URL after you push the repo.
-->

### Party Discourse in German Political Communication (TUM CSS)
**R · quanteda · LDA · large-scale text · political communication** · TUM course project

**Problem:** Which latent themes structure German party tweets, and how do AfD, Linke, CDU, CSU, Grüne and SPD differ in emphasis over time?

**Approach:** Built a two-stage NLP pipeline on **~1.59M tweets (2012–2018)**: (1) quanteda preprocessing and **LDA topic discovery** (`seededlda::textmodel_lda` k=5/10 and Gibbs LDA K=5, 500 iterations); (2) longitudinal tracking of LDA-derived top terms by party and month, with ggplot2 comparisons of frequencies and party shares.

**Result:** Interpretable topic structure (e.g. law/critique, Euro, government, Europe/migration, parliament, democracy/violence) and cross-party time series showing differential thematic emphasis — computational social science / big-text analysis at multi-million document scale.

**Stack:** R, quanteda, seededlda, topicmodels, dplyr, lubridate, ggplot2, readtext

**Link:** [GitHub — populist-party-discourse-r](https://github.com/a-teixeira/populist-party-discourse-r)
