# =============================================================================
# Populist / Party Discourse in German Political Communication
# TUM — Computational Social Science (course project)
#
# Method: dictionary-based topic tracking with quanteda
# Parties: AfD, Linke, CDU, CSU, Grüne, SPD
# =============================================================================

# ---- Setup -------------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(quanteda)
  library(quanteda.textstats)
  library(readtext)
  library(stringr)
})

# Project root = parent of analysis/ when sourced from that folder
root <- if (basename(getwd()) == "analysis") dirname(getwd()) else getwd()
data_dir   <- file.path(root, "data", "raw")
fig_dir    <- file.path(root, "output", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

parties <- c("AfD", "Linke", "CDU", "CSU", "Grüne", "SPD")

party_colors <- c(
  AfD    = "#0489DB",
  Linke  = "#BE3075",
  CDU    = "#000000",
  CSU    = "#008AC5",
  Grüne  = "#46962B",
  SPD    = "#E3000F"
)

# ---- Topic dictionaries (substantive labels) ---------------------------------
# These are hand-coded keyword sets used as *dictionary topics*, not LDA topics.
topics <- list(
  media_international = c(
    "twitter", "berlin", "eu", "german", "trump", "germany",
    "video", "live", "spaß", "minister"
  ),
  parliamentary_routine = c(
    "berlin", "gespräch", "deutschen", "bundestag", "rede",
    "woche", "cdu", "unterwegs", "mdb", "veranstaltung"
  ),
  domestic_policy = c(
    "deutschland", "euro", "debatte", "bundestag", "bundesregierung",
    "geld", "zukunft", "arbeit", "bildung", "antrag"
  ),
  democracy_security = c(
    "deutschland", "demokratie", "europa", "polizei", "flüchtlinge",
    "welt", "land", "gewalt", "deutschen", "bundesregierung"
  ),
  party_politics = c(
    "spd", "cdu", "merkel", "afd", "csu", "fdp",
    "partei", "linke", "grünen", "politik"
  )
)

topic_labels <- c(
  media_international   = "Media / international",
  parliamentary_routine = "Parliamentary routine",
  domestic_policy       = "Domestic policy",
  democracy_security    = "Democracy / security / migration",
  party_politics        = "Party politics"
)

# Extra noise terms beyond DE/EN stopwords (social-media specific)
noise_terms <- c(
  "rt", "https", "http", "htt", "ht", "amp", "via", "t.c", "mio", "min",
  "danke", "bitte", "dr", "prof", "mdb",
  "liebe", "schön", "schöne", "schönen", "schönes", "leider", "gerne",
  "guten", "eigentlich", "sehen", "sagen", "sehe", "schauen", "finden",
  "wünsche", "wünschen", "frage", "fragen", "lesen", "tolle", "super",
  "herzlichen", "glückwunsch", "freue", "freuen", "geburtstag",
  "diskussion", "besuch", "thema", "gestern", "neu", "neuer", "neues",
  "einfach", "herr", "frau", "fall", "antwort", "genau", "fordert",
  "abend", "u.a", "weiß", "nix", "läuft", "leute", "nacht", "zug",
  "lassen", "finde", "geht's", "gibt's", "berliner", "letzten", "paar",
  "fast", "raus", "stimmt", "eher", "klar", "sicher", "halt", "falsch",
  "echt", "leben", "reden", "hoffe", "schnell", "scheint", "sogar",
  "schade", "hoffentlich", "irgendwie", "voll", "vorbei", "gesehen",
  "stunden", "minuten", "wichtig", "nächste", "beste", "völlig",
  "grüne", "offenbar", "tut", "day", "spiel", "tor", "glaube",
  "bleiben", "nochmal", "deutsche", "spricht", "mal", "wer",
  "haben", "gehen", "mittlerweile", "bloß", "umso",
  "weihnachten", "weihnachtlich", "vorweihnachtlich", "kennen", "gehören",
  "treten", "januar", "februar", "märz", "april", "mai", "juni", "juli",
  "august", "september", "oktober", "november", "dezember",
  "ansteigen", "bekommen", "treffen", "angela", "sieht", "bleibt",
  "brauchen", "braucht", "dass"
)

# ---- Helpers -----------------------------------------------------------------

#' Load party CSVs from data/raw/<party>/
load_party <- function(party, data_dir) {
  path <- file.path(data_dir, party)
  if (!dir.exists(path)) {
    stop("Missing data folder: ", path,
         "\nPlace CSV files under data/raw/<party>/ (see data/README.md).",
         call. = FALSE)
  }

  raw <- readtext(path, encoding = "UTF-8")

  # Keep flexible column detection: prefer Date/Text if present
  nm <- tolower(names(raw))
  date_col <- names(raw)[match(TRUE, nm %in% c("date", "created_at", "datum"))]
  text_col <- names(raw)[match(TRUE, nm %in% c("text", "tweet", "content", "body"))]

  if (is.na(date_col) || is.na(text_col)) {
    # Fallback to original course layout: col2 = Date, col3 = Text after drops
    # Expect at least doc_id + date + text somewhere in the file
    stop("Could not find Date/Text columns in ", path,
         ". Columns found: ", paste(names(raw), collapse = ", "),
         call. = FALSE)
  }

  tibble(
    Date   = as.Date(raw[[date_col]]),
    Text   = as.character(raw[[text_col]]),
    Partei = party
  ) %>%
    filter(!is.na(Date), !is.na(Text), Text != "")
}

#' Tokenize one party corpus with shared cleaning rules
make_tokens <- function(df) {
  corp <- corpus(df, text_field = "Text")
  toks <- tokens(
    corp,
    remove_punct   = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url     = TRUE
  )
  toks <- tokens_remove(toks, pattern = c("@*", "#*"))
  toks <- tokens_remove(toks, pattern = stopwords("de", source = "stopwords-iso"))
  toks <- tokens_remove(toks, pattern = stopwords("en", source = "stopwords-iso"))
  toks <- tokens_remove(toks, pattern = noise_terms, case_insensitive = TRUE)
  toks <- tokens_tolower(toks)
  toks
}

#' Monthly keyword-hit counts for one party × one topic dictionary
topic_monthly <- function(toks, topic_words, party_name) {
  kept <- tokens_keep(toks, pattern = topic_words, case_insensitive = TRUE)
  d <- dfm(kept)
  if (nfeat(d) == 0 || ndoc(d) == 0) {
    return(tibble(month = as.Date(character()), !!party_name := numeric()))
  }

  # Group by document Date (docvar)
  d <- dfm_group(d, groups = Date)
  tab <- convert(d, to = "data.frame")

  # Sum all dictionary features present in this dfm
  feat_cols <- setdiff(names(tab), "doc_id")
  tab$total <- if (length(feat_cols)) {
    rowSums(tab[feat_cols], na.rm = TRUE)
  } else {
    0
  }

  tab %>%
    transmute(
      month = floor_date(ymd(doc_id), "month"),
      value = total
    ) %>%
    group_by(month) %>%
    summarise(!!party_name := sum(value, na.rm = TRUE), .groups = "drop")
}

#' Full party × month table for one topic
build_topic_table <- function(tokens_list, topic_words) {
  parts <- lapply(names(tokens_list), function(p) {
    topic_monthly(tokens_list[[p]], topic_words, p)
  })
  Reduce(function(a, b) full_join(a, b, by = "month"), parts) %>%
    arrange(month) %>%
    mutate(across(-month, ~ replace_na(.x, 0)))
}

#' Party share (%) of total dictionary hits for a topic table
party_shares <- function(topic_tbl) {
  party_cols <- setdiff(names(topic_tbl), "month")
  totals <- colSums(topic_tbl[party_cols], na.rm = TRUE)
  grand  <- sum(totals)
  if (grand == 0) return(setNames(rep(0, length(totals)), names(totals)))
  100 * totals / grand
}

#' Line chart: monthly frequency by party
plot_topic_lines <- function(topic_tbl, title, outfile = NULL) {
  long <- topic_tbl %>%
    pivot_longer(-month, names_to = "party", values_to = "freq")

  p <- ggplot(long, aes(x = month, y = freq, color = party)) +
    geom_line(linewidth = 0.5) +
    scale_color_manual(values = party_colors) +
    labs(
      title = title,
      x = "Month",
      y = "Dictionary hit frequency",
      color = "Party"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  if (!is.null(outfile)) {
    ggsave(outfile, p, width = 9, height = 5, dpi = 150)
  }
  p
}

# ---- Pipeline ----------------------------------------------------------------

message("Loading party data from: ", data_dir)
party_dfs <- setNames(lapply(parties, load_party, data_dir = data_dir), parties)

message("Tokenizing…")
tokens_list <- lapply(party_dfs, make_tokens)

message("Building topic tables…")
topic_tables <- lapply(topics, function(words) {
  build_topic_table(tokens_list, words)
})

# Shares matrix: rows = topics, cols = parties
share_mat <- sapply(topic_tables, party_shares)
# sapply may transpose depending on structure — normalize to topics × parties
if (!is.null(rownames(share_mat)) && all(rownames(share_mat) %in% parties)) {
  share_mat <- t(share_mat)
}
share_df <- as.data.frame(share_mat) %>%
  tibble::rownames_to_column("topic") %>%
  mutate(topic_label = topic_labels[topic])

print(share_df)

# ---- Visualizations ----------------------------------------------------------

for (nm in names(topic_tables)) {
  outfile <- file.path(fig_dir, paste0("topic_", nm, ".png"))
  print(plot_topic_lines(
    topic_tables[[nm]],
    title   = paste0("Topic: ", topic_labels[[nm]]),
    outfile = outfile
  ))
  message("Saved ", outfile)
}

# Stacked bar: party share per topic
share_long <- share_df %>%
  select(-topic_label) %>%
  pivot_longer(-topic, names_to = "party", values_to = "share") %>%
  mutate(
    topic_label = factor(topic_labels[topic], levels = unname(topic_labels)),
    party = factor(party, levels = parties)
  )

p_shares <- ggplot(share_long, aes(x = topic_label, y = share, fill = party)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = party_colors) +
  labs(
    title = "Party share of dictionary hits by topic",
    x = NULL,
    y = "Share (%)",
    fill = "Party"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

ggsave(
  file.path(fig_dir, "party_shares_by_topic.png"),
  p_shares, width = 10, height = 5.5, dpi = 150
)
print(p_shares)

message("Done. Figures in: ", fig_dir)
