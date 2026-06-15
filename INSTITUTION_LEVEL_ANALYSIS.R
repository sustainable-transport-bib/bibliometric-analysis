# ============================================================
# INSTITUTION LEVEL ANALYSIS 
# ============================================================
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

data <- read_excel("C:/Users/MY/Downloads/full record/full.xlsx", sheet = "Sheet1")

inst_col  <- "Affiliations"
cites_col <- "Times Cited, All Databases"
id_col    <- "DOI"

data <- data %>%
  mutate(
    times_cited = suppressWarnings(as.numeric(.data[[cites_col]])),
    article_id  = if (id_col %in% names(.)) .data[[id_col]] else row_number()
  )

inst_long <- data %>%
  mutate(all_institutions = str_split(.data[[inst_col]], ";")) %>%
  unnest(all_institutions) %>%
  mutate(
    institution = all_institutions %>%
      str_squish() %>%
      str_to_lower() %>%
      str_replace_all("\\.", "") %>%
      str_replace_all("&", " and ") %>%
      str_replace_all(",", "") %>%
      str_replace_all("-", " ") %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_replace_all("univ\\b", "university") %>%
      str_replace_all("\\binst\\b", "institute") %>%
      str_replace_all("\\bdept\\b", "department") %>%
      str_replace_all("\\btech\\b", "technology") %>%
      str_squish()
  )

inst_long <- inst_long %>%
  mutate(institution = case_when(
    institution %in% c(
      "ec jrc ispra site",
      "european commission joint research centre",
      "jrc ispra site",
      "joint research centre european commission",
      "european commission jrc ispra"
    ) ~ "european commission joint research centre",
    TRUE ~ institution
  )) %>%
  filter(
    !str_detect(institution, "system"),
    institution != "helmholtz association"
  )

inst_long_unique <- inst_long %>%
  distinct(article_id, institution, .keep_all = TRUE)

institution_stats <- inst_long_unique %>%
  group_by(institution) %>%
  summarise(
    number_of_articles = n(),
    total_citations    = sum(times_cited, na.rm = TRUE),
    h_index = {
      v <- sort(replace(times_cited, is.na(times_cited), 0), decreasing = TRUE)
      sum(v >= seq_along(v))
    },
    hot_articles = sum(times_cited > 50, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(number_of_articles), desc(h_index), desc(total_citations))

print(institution_stats, n = 50)

write.xlsx(
  institution_stats,
  "C:/Users/MY/Downloads/institution_analysis.xlsx",
  rowNames = FALSE
)

cat("Done.\n")
