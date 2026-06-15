# ============================================================
#country level analysis
# ============================================================

install.packages(c("readxl","countrycode","stringr","openxlsx","dplyr","tidyr"))
library(readxl); library(countrycode); library(stringr)
library(openxlsx); library(dplyr); library(tidyr)

# ============================================================
# PATH
# ============================================================
full_path   <- "C:/Users/MY/Downloads/full record/full.xlsx"
output_path <- "C:/Users/MY/Downloads/full record/country_full_summary.xlsx"

# ============================================================
# READ ORIGINAL DATA (Sheet1 only)
# ============================================================
raw_data <- read_excel(full_path, sheet = "Sheet1") %>%
  mutate(
    paper_id   = row_number(),
    citations  = suppressWarnings(as.numeric(`Times Cited, WoS Core`)),
    Addresses  = as.character(Addresses)
  )

# ============================================================
# SHARED DICTIONARY
# ============================================================
canon <- unique(countrycode::codelist$country.name.en)

extra_aliases <- data.frame(
  alias_raw = c("USA","US","U.S.","U.S.A.",
                "UK","U.K.","GB","England","Scotland","Wales","Northern Ireland",
                "China","PRC","P.R.C.","Peoples R China","People R China",
                "Hong Kong","Hong Kong SAR","Hong Kong SAR China","Macao","Macau",
                "Türkiye","Turkiye","Holland","The Netherlands",
                "Hamburg","Hanover","Brunswick","Tibet","Trinidad & Tobago",
                "Korea","South Korea","Republic of Korea",
                "Saudi Arabia","KSA","UAE","U.A.E.","United Arab Emirates",
                "Russia","Russian Federation","Iran","Viet Nam","Vietnam",
                "Czech Republic","Czechia","DR Congo","Democratic Republic of the Congo"),
  standard  = c("United States","United States","United States","United States",
                "United Kingdom","United Kingdom","United Kingdom","United Kingdom","United Kingdom","United Kingdom","United Kingdom",
                "China","China","China","China","China",
                "China","China","China","China","China",
                "Turkey","Turkey","Netherlands","Netherlands",
                "Germany","Germany","Germany","China","Trinidad and Tobago",
                "South Korea","South Korea","South Korea",
                "Saudi Arabia","Saudi Arabia","United Arab Emirates","United Arab Emirates","United Arab Emirates",
                "Russia","Russia","Iran","Vietnam","Vietnam",
                "Czechia","Czechia","Democratic Republic of the Congo","Democratic Republic of the Congo"),
  stringsAsFactors = FALSE
)

shared_dict <- bind_rows(
  data.frame(alias_raw = canon, standard = canon, stringsAsFactors = FALSE),
  extra_aliases  # ← use the correct object directly, column names already match
) %>% distinct(alias_raw, .keep_all = TRUE)


shared_dict2 <- shared_dict %>%
  mutate(
    alias_esc = str_replace_all(alias_raw, "([\\\\.$|()\\[\\]{}*+?^])", "\\\\\\1"),
    alias_esc = str_replace_all(alias_esc, "\\s+", "\\\\s+")
  ) %>%
  arrange(desc(nchar(alias_raw)))

shared_pattern <- paste0("(?i)(?<![A-Za-z])(",
                         paste(shared_dict2$alias_esc, collapse = "|"),
                         ")(?![A-Za-z])")

# ============================================================
# STEP 1: DETECT FIRST COUNTRY PER PAPER (replaces Sheet2 logic)
# ============================================================
first_alias   <- str_extract(raw_data$Addresses, shared_pattern)
first_country <- shared_dict2$standard[match(tolower(first_alias), tolower(shared_dict2$alias_raw))]
raw_data$detected_country <- first_country

# ============================================================
# STEP 2: ARTICLE COUNT + VOS CITATIONS
# ============================================================
article_counts <- raw_data %>%
  filter(!is.na(detected_country), detected_country != "") %>%
  count(detected_country, name = "article count") %>%
  rename(country = detected_country) %>%
  arrange(desc(`article count`))

paper_countries_vos <- raw_data %>%
  transmute(paper_id, citations, hits = str_extract_all(replace_na(Addresses, ""), shared_pattern)) %>%
  unnest(hits) %>%
  mutate(
    hits    = str_squish(hits),
    country = shared_dict2$standard[match(tolower(hits), tolower(shared_dict2$alias_raw))]
  ) %>%
  filter(!is.na(country), country != "") %>%
  distinct(paper_id, country, citations)

vos_citations <- paper_countries_vos %>%
  group_by(country) %>%
  summarise(`total citations (VOS, WoS Core)` = sum(citations, na.rm = TRUE), .groups = "drop")

part1_result <- article_counts %>%
  left_join(vos_citations, by = "country") %>%
  mutate(`total citations (VOS, WoS Core)` = replace_na(`total citations (VOS, WoS Core)`, 0)) %>%
  arrange(desc(`article count`))

# ============================================================
# STEP 3: INSTITUTION COUNT PER COUNTRY
# ============================================================
inst_blocks <- raw_data %>%
  filter(!is.na(Addresses)) %>%
  mutate(block = str_split(Addresses, ";")) %>%
  unnest(block) %>%
  mutate(
    block       = str_squish(block),
    affil       = str_squish(str_remove(block, "\\[[^\\]]*\\]")),
    inst_country_raw   = str_trim(str_extract(affil, "[^,]+$")),
    inst_country_clean = str_trim(str_remove(inst_country_raw, "^[A-Z]{2}\\s*\\d*\\s*")),
    inst_country       = shared_dict2$standard[match(tolower(inst_country_clean), tolower(shared_dict2$alias_raw))],
    institution        = str_trim(str_split_fixed(affil, ",", 2)[,1])
  ) %>%
  filter(block != "")

part2_result <- inst_blocks %>%
  filter(!is.na(inst_country), inst_country != "", institution != "") %>%
  distinct(institution, inst_country) %>%
  group_by(country = inst_country) %>%
  summarise(`institution count` = n(), .groups = "drop") %>%
  arrange(desc(`institution count`))

# ============================================================
# STEP 4: AUTHOR COUNT PER COUNTRY
# ============================================================
auth_blocks <- raw_data %>%
  filter(!is.na(Addresses)) %>%
  mutate(block = str_split(Addresses, ";")) %>%
  unnest(block) %>%
  mutate(
    block       = str_squish(block),
    authors_raw = str_extract(block, "(?<=\\[)[^\\]]+(?=\\])"),
    affil       = str_squish(str_remove(block, "\\[[^\\]]*\\]")),
    auth_country_raw   = str_trim(str_extract(affil, "[^,]+$")),
    auth_country_clean = str_trim(str_remove(auth_country_raw, "^[A-Z]{2}\\s*\\d*\\s*")),
    auth_country       = shared_dict2$standard[match(tolower(auth_country_clean), tolower(shared_dict2$alias_raw))]
  ) %>%
  filter(block != "")

auth_country_unique <- auth_blocks %>%
  filter(!is.na(authors_raw), !is.na(auth_country)) %>%
  mutate(author = str_split(authors_raw, ";")) %>%
  unnest(author) %>%
  mutate(author = str_squish(author)) %>%
  filter(author != "") %>%
  distinct(author, auth_country)

part3_result <- auth_country_unique %>%
  group_by(country = auth_country) %>%
  summarise(`author count` = n(), .groups = "drop") %>%
  arrange(desc(`author count`))

# ============================================================
# STEP 5: MERGE ALL
# ============================================================
merged_result <- part1_result %>%
  full_join(part2_result, by = "country") %>%
  full_join(part3_result, by = "country") %>%
  arrange(desc(`article count`))

# ============================================================
# STEP 6: EXPORT ONE CLEAN EXCEL FILE
# ============================================================
write.xlsx(
  list(
    "Article_Citations"  = part1_result,
    "Institution_Count"  = part2_result,
    "Author_Count"       = part3_result,
    "Merged_Summary"     = merged_result,
    "Detected_Raw"       = raw_data
  ),
  output_path,
  overwrite = TRUE
)

cat("Pipeline complete. File saved to:\n", output_path, "\n")

# ============================================================
# STEP 7: CITATION ANALYSIS + HOT ARTICLES
# ============================================================
library(ggplot2)

citations <- raw_data$`Times Cited, All Databases`

# Summary statistics
summary_stats <- data.frame(
  Min    = min(citations, na.rm = TRUE),
  Q1     = quantile(citations, 0.25, na.rm = TRUE),
  Median = median(citations, na.rm = TRUE),
  Mean   = mean(citations, na.rm = TRUE),
  Q3     = quantile(citations, 0.75, na.rm = TRUE),
  Max    = max(citations, na.rm = TRUE),
  SD     = sd(citations, na.rm = TRUE)
)
print(summary_stats)

# Citation bins
bins <- cut(citations, breaks = c(-1,0,10,50,100,500,10000),
            labels = c("0","1-10","11-50","51-100","101-500",">500"))
print(table(bins))

# Histogram
ggplot(raw_data, aes(x = `Times Cited, All Databases`)) +
  geom_histogram(binwidth = 5, fill = "#4a90e2", color = "white") +
  labs(title = "Citation Distribution", x = "Number of Citations", y = "Count of Articles") +
  theme_minimal()

# Boxplot
ggplot(raw_data, aes(y = `Times Cited, All Databases`)) +
  geom_boxplot(fill = "#e2a94a") +
  labs(title = "Citation Count Boxplot", y = "Number of Citations") +
  theme_minimal()

# Hot articles (>50 citations)
hot_articles <- raw_data %>%
  filter(`Times Cited, All Databases` > 50)

hot_articles_summary <- hot_articles %>%
  select(
    Author = `Author Full Names`,
    Country = detected_country,
    Institution = Affiliations,
    Journal = `Source Title`,
    Year = `Publication Year`,
    Citations = `Times Cited, All Databases`
  )

print(hot_articles_summary)

calc_h_index <- function(citations) {
  c_sorted <- sort(citations, decreasing = TRUE)
  sum(c_sorted >= seq_along(c_sorted))
}

all_country_citations <- raw_data %>%
  select(
    Country = detected_country,
    Citations = `Times Cited, All Databases`
  ) %>%
  filter(!is.na(Country), Country != "", !is.na(Citations))

hot_country_stats <- hot_articles_summary %>%
  filter(!is.na(Country), Country != "") %>%
  group_by(Country) %>%
  summarise(
    num_hot_articles = n(),
    institution_count = n_distinct(Institution),
    total_citations = sum(Citations, na.rm = TRUE),
    .groups = "drop"
  )

all_country_hindex <- all_country_citations %>%
  group_by(Country) %>%
  summarise(
    h_index = calc_h_index(Citations),
    .groups = "drop"
  )

country_stats <- hot_country_stats %>%
  left_join(all_country_hindex, by = "Country") %>%
  arrange(desc(num_hot_articles))

print(country_stats)

# ============================================================
# EXPORT: Add hot articles sheets to final Excel file
# ============================================================
write.xlsx(
  list(
    "Article_Citations"  = part1_result,
    "Institution_Count"  = part2_result,
    "Author_Count"       = part3_result,
    "Merged_Summary"     = merged_result,
    "Detected_Raw"       = raw_data,
    "Hot_Articles"       = hot_articles_summary,
    "Country_Hot_Stats"  = country_stats
  ),
  output_path,
  overwrite = TRUE
)

cat("Pipeline complete. File saved to:\n", output_path, "\n")
