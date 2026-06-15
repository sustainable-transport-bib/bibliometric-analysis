# ============================================================
# JOURNAL LEVEL ANALYSIS
# ============================================================
install.packages("tidyr")
library(dplyr)
library(tidyr)
library(openxlsx)
library(readxl)

data <- read_excel("C:/Users/MY/Downloads/full record/full.xlsx", sheet = "Sheet1")

# ============================================================
# TABLE 1: Publications & Citations by Journal × Year (wide)
# ============================================================
journal_year_summary <- data %>%
  group_by(`Source Title`, `Publication Year`) %>%
  summarise(
    publications = n(),
    citations    = sum(`Times Cited, All Databases`, na.rm = TRUE),
    .groups      = "drop"
  )

wide_summary <- pivot_wider(
  journal_year_summary,
  id_cols     = `Source Title`,
  names_from  = `Publication Year`,
  values_from = c(publications, citations),
  names_glue  = "{.value}_{`Publication Year`}"
)

# ============================================================
# TABLE 2: Journal Summary with correct H-Index
# ============================================================
calc_h_index <- function(citations) {
  c_sorted <- sort(citations, decreasing = TRUE)
  sum(c_sorted >= seq_along(c_sorted))
}

journal_summary <- data %>%
  group_by(`Source Title`) %>%
  summarise(
    Published_Papers = n(),
    Citations        = sum(`Times Cited, All Databases`, na.rm = TRUE),
    Hot_Articles     = sum(`Times Cited, All Databases` > 50, na.rm = TRUE),
    H_Index          = calc_h_index(`Times Cited, All Databases`),
    IF               = NA_real_
  ) %>%
  arrange(desc(Citations)) %>%
  mutate(JournalAbbr = substr(`Source Title`, 1, 50)) %>%
  select(JournalAbbr, Published_Papers, Citations, Hot_Articles, H_Index, IF)

# ============================================================
# EXPORT
# ============================================================
write.xlsx(
  list(
    "Journal_Year_Wide" = wide_summary,
    "Journal_Summary"   = journal_summary
  ),
  "C:/Users/MY/Downloads/journal_full_analysis.xlsx",
  overwrite = TRUE
)

cat("Done.\n")

#bubble plot journals
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Load raw data
data <- read_excel("C:/Users/MY/Downloads/full record/full.xlsx", sheet = "Sheet1")

# 2. Summarise by journal and year
journal_year_summary <- data %>%
  group_by(`Source Title`, `Publication Year`) %>%
  summarise(
    publications = n(),
    citations    = sum(`Times Cited, All Databases`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(`Publication Year` = as.numeric(`Publication Year`))

# 3. Find top 10 journals by total publications
top_journals <- journal_year_summary %>%
  group_by(`Source Title`) %>%
  summarise(total_pub = sum(publications), .groups = "drop") %>%
  arrange(desc(total_pub)) %>%
  slice_head(n = 10) %>%
  pull(`Source Title`)

# 4. Filter only top 10 journals
plot_data <- journal_year_summary %>%
  filter(`Source Title` %in% top_journals) %>%
  mutate(
    JournalAbbr = substr(`Source Title`, 1, 40),
    JournalAbbr = factor(JournalAbbr, levels = rev(substr(top_journals, 1, 40)))
  )

# 5. Plot
p <- ggplot(plot_data, aes(x = `Publication Year`, y = JournalAbbr)) +
  geom_tile(aes(fill = citations),
            color  = NA,
            width  = 1,
            height = 0.2,
            alpha  = 0.9) +
  geom_point(aes(size = publications),
             shape  = 21,
             fill   = "darkblue",
             color  = "darkblue",
             stroke = 0.2,
             alpha  = 0.5) +
  scale_fill_gradient(low = "mistyrose", high = "red", name = "Citations") +
  scale_size(range = c(0, 8.5), name = "Publications") +
  theme_bw(base_size = 12) +
  labs(x = "Year", y = "Journal") +
  theme(
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey60", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y        = element_text(size = 9),
    legend.position    = "bottom",
    legend.box         = "horizontal"
  )

print(p)

ggsave("C:/Users/MY/Downloads/journals_year_bubble_final.png",
       plot = p, width = 12, height = 7, dpi = 1200)


