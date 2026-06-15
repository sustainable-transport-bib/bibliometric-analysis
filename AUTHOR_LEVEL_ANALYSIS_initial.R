library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

path  <- "C:/Users/MY/Downloads/full record/full.xlsx"
sheet <- "Sheet1"

# ---- 1) Load + IDs ----
df <- read_excel(path, sheet = sheet) %>%
  mutate(
    paper_id = case_when(
      "UT (Unique WOS ID)" %in% names(.) ~ as.character(`UT (Unique WOS ID)`),
      "DOI" %in% names(.) ~ as.character(DOI),
      TRUE ~ as.character(row_number())
    ),
    times_cited = suppressWarnings(as.numeric(`Times Cited, All Databases`)),
    Authors = as.character(Authors),
    `Cited References` = as.character(`Cited References`)
  )

# ---- 2) h-index helper ----
h_index <- function(v) {
  v <- sort(replace(v, is.na(v), 0), decreasing = TRUE)
  sum(v >= seq_along(v))
}

# ---- 3) Long authors + first author ----
authors_long <- df %>%
  mutate(
    first_author = str_squish(str_extract(Authors, "^[^;]+"))
  ) %>%
  separate_rows(Authors, sep = ";") %>%
  mutate(
    author = str_squish(Authors),
    is_first_author = (author == first_author)
  ) %>%
  select(paper_id, author, is_first_author, times_cited, `Cited References`)

# ---- 4) Self-citing in reference lists (heuristic) ----
# Rule: count a cited reference as "self-cited" for this paper if it contains any author token like "LASTNAME <initial>"
make_tokens <- function(author_vec) {
  # From "Smith J" or "Smith, John" -> "SMITH J"
  author_vec <- str_replace_all(author_vec, ",", " ")
  author_vec <- str_squish(author_vec)
  
  last <- str_to_upper(word(author_vec, 1))
  init <- str_to_upper(str_sub(word(author_vec, 2), 1, 1))
  
  token <- ifelse(is.na(last) | last == "", NA_character_,
                  ifelse(is.na(init) | init == "", last, paste(last, init)))
  unique(na.omit(token))
}

paper_tokens <- authors_long %>%
  group_by(paper_id) %>%
  summarise(tokens = list(make_tokens(author)), .groups = "drop")

paper_selfref <- df %>%
  select(paper_id, `Cited References`) %>%
  left_join(paper_tokens, by = "paper_id") %>%
  mutate(ref_text = str_to_upper(coalesce(`Cited References`, ""))) %>%
  rowwise() %>%
  mutate(
    # crude count: number of tokens that appear at least once in reference string
    selfref_hits = sum(str_detect(ref_text, fixed(tokens, ignore_case = TRUE))),
    has_any_selfref = selfref_hits > 0
  ) %>%
  ungroup() %>%
  select(paper_id, selfref_hits, has_any_selfref)

authors_long2 <- authors_long %>%
  left_join(paper_selfref, by = "paper_id")

# ---- 5) Author-level stats ----
author_stats <- authors_long2 %>%
  group_by(author) %>%
  summarise(
    article_numbers = n_distinct(paper_id),
    citation_numbers = sum(times_cited, na.rm = TRUE),
    h_index = h_index(times_cited),
    times_as_first_author = sum(is_first_author, na.rm = TRUE),
    
    # reference-list self-citation metrics
    papers_with_any_selfref = sum(has_any_selfref, na.rm = TRUE),
    selfref_hits_total = sum(selfref_hits, na.rm = TRUE),
    selfref_paper_rate = papers_with_any_selfref / article_numbers,
    
    .groups = "drop"
  ) %>%
  arrange(desc(h_index), desc(citation_numbers), desc(article_numbers))

write.xlsx(author_stats,
           "C:/Users/MY/Downloads/author_stats_with_reference_selfcite.xlsx",
           rowNames = FALSE)
