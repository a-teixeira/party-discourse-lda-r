# =============================================================================
# Stage 1 — LDA topic discovery (reconstructed from LDA_Final_DictionaryWords-2.R)
#
# Original course notes:
#   Data: tweets from political actors, 2012–2018
#   N:    1,588,399
#
# This script expects cleaned party data under data/raw/<party>/
# and writes top terms to output/lda/
# =============================================================================

suppressPackageStartupMessages({
  library(quanteda)
  library(quanteda.textmodels) # textmodel_lda (via seededlda backend)
  library(dplyr)
  library(readtext)
  library(seededlda)
})

# topicmodels::LDA is optional second backend (Gibbs)
has_topicmodels <- requireNamespace("topicmodels", quietly = TRUE)

root <- if (basename(getwd()) == "analysis") dirname(getwd()) else getwd()
data_dir <- file.path(root, "data", "raw")
out_dir  <- file.path(root, "output", "lda")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

parties <- c("AfD", "Linke", "CDU", "CSU", "Grüne", "SPD")

noise <- c(
  "rt", "https", "http", "htt", "ht", "amp", "via",
  "danke", "bitte", "dr", "prof"
)

load_party <- function(party) {
  path <- file.path(data_dir, party)
  if (!dir.exists(path)) stop("Missing: ", path, call. = FALSE)
  raw <- readtext(path, encoding = "UTF-8")
  nm <- tolower(names(raw))
  date_col <- names(raw)[match(TRUE, nm %in% c("date", "created_at", "datum"))]
  text_col <- names(raw)[match(TRUE, nm %in% c("text", "tweet", "content", "body"))]
  if (is.na(date_col) || is.na(text_col)) {
    stop("Need Date + Text columns in ", path, call. = FALSE)
  }
  data.frame(
    Date   = as.Date(raw[[date_col]]),
    Text   = as.character(raw[[text_col]]),
    Partei = party,
    stringsAsFactors = FALSE
  )
}

message("Loading parties…")
all_df <- bind_rows(lapply(parties, load_party))
all_df <- all_df[!is.na(all_df$Text) & all_df$Text != "", ]

message("Documents loaded: ", nrow(all_df))

corp <- corpus(all_df, text_field = "Text")
toks <- tokens(
  corp,
  remove_punct = TRUE, remove_numbers = TRUE,
  remove_symbols = TRUE, remove_url = TRUE
)
toks <- tokens_remove(toks, pattern = c("@*", "#*"))
toks <- tokens_tolower(toks)
toks <- tokens_remove(toks, stopwords("de", source = "stopwords-iso"))
toks <- tokens_remove(toks, stopwords("en", source = "stopwords-iso"))
toks <- tokens_remove(toks, noise)

# DFM + trim (mirrors course script intent)
dfm_full <- dfm(toks)
dfm_trim1 <- dfm_trim(
  dfm_full,
  min_termfreq  = 0.075, termfreq_type = "quantile",
  max_docfreq   = 0.9,   docfreq_type  = "prop"
)
# drop empty docs
dfm_trim1 <- dfm_trim1[rowSums(dfm_trim1) > 0, ]

message("DFM dims after trim: ", paste(dim(dfm_trim1), collapse = " x "))

# ---- LDA via quanteda.textmodels / seededlda (as in course) ----------------
set.seed(1)
message("Fitting textmodel_lda k = 5…")
lda5 <- textmodel_lda(dfm_trim1, k = 5)
terms5 <- terms(lda5, 10)
print(terms5)
write.csv(terms5, file.path(out_dir, "lda_seededlda_k5_top10.csv"), row.names = FALSE)

message("Fitting textmodel_lda k = 10…")
lda10 <- textmodel_lda(dfm_trim1, k = 10)
terms10 <- terms(lda10, 10)
print(terms10)
write.csv(terms10, file.path(out_dir, "lda_seededlda_k10_top10.csv"), row.names = FALSE)

saveRDS(lda5,  file.path(out_dir, "lda_seededlda_k5.rds"))
saveRDS(lda10, file.path(out_dir, "lda_seededlda_k10.rds"))

# ---- Optional Gibbs LDA (topicmodels), as in course ------------------------
if (has_topicmodels) {
  message("Fitting topicmodels::LDA Gibbs K = 5, iter = 500…")
  # topicmodels expects a dfm convertible via convert()
  dtm <- convert(dfm_trim1, to = "topicmodels")
  lda_gibbs <- topicmodels::LDA(
    dtm, k = 5, method = "Gibbs",
    control = list(iter = 500, seed = 1, verbose = 25)
  )
  top_gibbs <- topicmodels::terms(lda_gibbs, 15)
  print(top_gibbs)
  write.csv(top_gibbs, file.path(out_dir, "lda_gibbs_k5_top15.csv"), row.names = FALSE)
  saveRDS(lda_gibbs, file.path(out_dir, "lda_gibbs_k5.rds"))
} else {
  message("Package 'topicmodels' not installed — skipping Gibbs LDA.")
}

message("LDA stage done. Outputs in: ", out_dir)
message("Next: copy top terms into topic dictionaries and run 01_analysis.R / stage-2 tracking.")
