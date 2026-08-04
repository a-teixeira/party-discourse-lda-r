# If a recruiter / interviewer asks about the cleanup

Use these only if they open the repo or ask “what did you improve?”.
Do **not** volunteer a laundry list of bugs unprompted.

---

## Short version (20 seconds)

> “The course project was a two-stage NLP pipeline on about 1.6 million German party tweets: LDA for topic discovery, then tracking those topic keywords by party over time. The first version was split across notebook-style scripts with local paths. For the portfolio I cleaned that into a reproducible repo and documented which file did LDA versus longitudinal comparison.”

---

## If they dig into specifics

### Was it really LDA / big data?
**They might ask:** “Is this topic modeling?”

> “Yes. Stage one: LDA on the pooled corpus with seededlda textmodel_lda (k=5 and k=10) and Gibbs LDA (K=5, 500 iterations). Stage two: take top terms from each topic and track hit frequencies by party and month. Discovery is unsupervised LDA; the party comparison uses those LDA-derived terms.”

**They might ask:** “Why keyword tracking after LDA?”

> “After inspecting terms() and the beta distribution, top-term trajectories were the clearest way to compare six parties over 2012–2018 for the course deliverable. Full theta-by-month would be the natural extension.”

**They might ask:** “Big data?”

> “About 1.59 million tweets, multi-year, six parties — full-corpus DFM plus LDA. In a CSS course sense that’s the large-scale text component.”

### Paths / reproducibility
**They might ask:** “Why relative paths / a data/raw layout?”

> “The course scripts pointed at my local Documents folder and misused system.file with readtext. That only ran on my laptop. I switched to a project-relative layout so anyone can drop CSVs under data/raw/<party>/ and re-run.”

### Dead / debug code
**They might ask:** “What was wrong in the old scripts?”

> “Leftover scratch lines from debugging, install.packages mid-script, and heavy copy-paste across parties. Fine in a private notebook; I stripped that for a public analysis repo.”

### Copy-paste across parties × topics
**They might ask:** “Why functions instead of repeating blocks?”

> “Six parties and several topics meant the same block dozens of times. Shared functions keep the logic identical and reviewable.”

### Fragile column drops
**They might ask:** “How do you handle messy CSV schemas?”

> “The course code dropped columns by position. That breaks when the export schema changes. The cleaned loader selects Date/Text by name with a few aliases.”

---

## What not to say

- Don’t call the course version “bad code” or blame the course.
- Don’t list every typo in the stopword list.
- Don’t claim you only did pure LDA document classification if they open stage-2 plots — explain the two stages.
- Don’t oversell: strong CSS course project + portfolio cleanup, not a production ML system.
