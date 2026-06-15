# ============================================================
# AUTHOR LEVEL ANALYSIS 
# ============================================================
library(readxl)
library(openxlsx)

# ── 1. Define the 10 files and their sheet names ─────────────────────────────────
files <- list(
  "Axsen"          = "C:/Users/MY/Downloads/citation_report_axsen.xlsx",
  "Shi"            = "C:/Users/MY/Downloads/citation_report_shi.xlsx",
  "Ramachandra"    = "C:/Users/MY/Downloads/citation_report_ramachandra.xlsx",
  "Long"           = "C:/Users/MY/Downloads/citation_report_long.xlsx",
  "Zhao"           = "C:/Users/MY/Downloads/citation_report_zhao.xlsx",
  "Du"             = "C:/Users/MY/Downloads/citation_report_du.xlsx",
  "Haselbach"      = "C:/Users/MY/Downloads/citation_report_haselbach.xlsx",
  "Setturu"        = "C:/Users/MY/Downloads/citation_report_setturu.xlsx",
  "Banister"       = "C:/Users/MY/Downloads/citation_report_banister.xlsx",
  "Suarez-Bertoa"  = "C:/Users/MY/Downloads/citation_report_suarez_bertoa.xlsx"
)

# ── 2. Create workbook and load each file into its own sheet ─────────────────────
wb <- createWorkbook()

for (sheet_name in names(files)) {
  file_path <- files[[sheet_name]]
  
  # Read with skip=10 as your files have headers on row 11
  df <- read_excel(file_path, sheet = 1, skip = 10)
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df)
  
  message("✓ Added sheet: ", sheet_name, " (", nrow(df), " rows)")
}

# ── 3. Save combined file ─────────────────────────────────────────────────────────
saveWorkbook(wb, "C:/Users/MY/Downloads/all_authors_combined.xlsx", overwrite = TRUE)

message("\n✓ Done! all_authors_combined.xlsx saved with 10 sheets.")


#=================================================================================================

  
  
  
cat(colnames(combined)[grepl("202", colnames(combined))])
# Remove old sheets and redo
wb <- loadWorkbook(file_path)

for (s in c("Per Year Stats", "Publications Wide", "Citations Wide")) {
  if (s %in% names(wb)) removeWorksheet(wb, s)
}

# Citations wide with explicit 2026.0 included
summary_wide_cites <- combined %>%
  select(author, matches("^20[0-9]{2}\\.0$")) %>%
  pivot_longer(-author, names_to = "year", values_to = "citations") %>%
  mutate(
    year      = as.integer(sub("\\.0$", "", year)),
    citations = replace_na(as.numeric(citations), 0)
  ) %>%
  filter(year >= 2010, year <= 2026) %>%
  group_by(author, year) %>%
  summarise(citations = sum(citations, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = citations) %>%
  mutate(total_citations = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
  arrange(desc(total_citations))

# Publications wide 2010-2025
summary_wide_pubs <- combined %>%
  mutate(publication_year = as.integer(.[[year_col]])) %>%
  filter(publication_year >= 2010, publication_year <= 2025) %>%
  group_by(author, publication_year) %>%
  summarise(publications = n(), .groups = "drop") %>%
  pivot_wider(names_from = publication_year, values_from = publications, values_fill = 0) %>%
  mutate(total_publications = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
  arrange(desc(total_publications))

# Per year long
summary_long <- expand.grid(author = sheet_names, publication_year = 2010:2026,
                            stringsAsFactors = FALSE) %>%
  left_join(
    combined %>%
      mutate(publication_year = as.integer(.[[year_col]])) %>%
      filter(publication_year >= 2010, publication_year <= 2025) %>%
      group_by(author, publication_year) %>%
      summarise(publications = n(), .groups = "drop"),
    by = c("author", "publication_year")
  ) %>%
  left_join(
    combined %>%
      select(author, matches("^20[0-9]{2}\\.0$")) %>%
      pivot_longer(-author, names_to = "year", values_to = "citations") %>%
      mutate(publication_year = as.integer(sub("\\.0$", "", year)),
             citations = replace_na(as.numeric(citations), 0)) %>%
      filter(publication_year >= 2010, publication_year <= 2026) %>%
      group_by(author, publication_year) %>%
      summarise(citations = sum(citations), .groups = "drop"),
    by = c("author", "publication_year")
  ) %>%
  mutate(publications = replace_na(publications, 0),
         citations    = replace_na(citations, 0)) %>%
  arrange(author, publication_year)

addWorksheet(wb, "Per Year Stats")
writeData(wb, "Per Year Stats", summary_long)
addWorksheet(wb, "Publications Wide")
writeData(wb, "Publications Wide", summary_wide_pubs)
addWorksheet(wb, "Citations Wide")
writeData(wb, "Citations Wide", summary_wide_cites)

saveWorkbook(wb, file_path, overwrite = TRUE)
message("✓ Done!")


#================================================================================


bubble graph

library(readxl)
library(ggplot2)
library(dplyr)
library(scales)

# ── 1. Load Per Year Stats ────────────────────────────────────────────────────────
file_path <- "C:/Users/MY/Downloads/all_authors_combined.xlsx"
plot_data <- read_excel(file_path, sheet = "Per Year Stats")

# ── 2. Prepare data ───────────────────────────────────────────────────────────────
plot_data <- plot_data %>%
  filter(publication_year >= 2010, publication_year <= 2025) %>%
  mutate(
    publication_year = as.numeric(publication_year),
    publications     = as.numeric(publications),
    citations        = as.numeric(citations)
  ) %>%
  filter(publications > 0)

# ── 3. Author order — most publications at top ────────────────────────────────────
author_order <- plot_data %>%
  group_by(author) %>%
  summarise(total_pubs = sum(publications), .groups = "drop") %>%
  arrange(total_pubs) %>%
  pull(author)

plot_data <- plot_data %>%
  mutate(author = factor(author, levels = author_order))

# ── 4. Color scale setup ──────────────────────────────────────────────────────────
max_cite <- max(plot_data$citations, na.rm = TRUE)

breaks <- c(0, 10, 30, 80, 150, 200, max_cite)
colors <- c(
  "#FFB6C1",  # 0–10    light pink
  "#FF8C69",  # 10–30   orangeish pink
  "#CC0000",  # 30–80   red
  "#8B0000",  # 80–150  dark red
  "#4A0000",  # 150–200 deeper dark red
  "#8B008B"   # 200+    dark magenta
)

# ── 5. Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(plot_data, aes(x = publication_year, y = author)) +
  
  geom_tile(
    aes(fill = citations),
    color  = NA,
    width  = 1,
    height = 0.2,
    alpha  = 0.9
  ) +
  
  geom_point(
    aes(size = publications),
    shape  = 21,
    fill   = "darkblue",
    color  = "darkblue",
    stroke = 0.2,
    alpha  = 0.5
  ) +
  
  scale_fill_gradientn(
    colours = colors,
    values  = rescale(breaks),
    limits  = c(0, max_cite),
    name    = "Citations",
    breaks  = c(0, 50, 100, 150, 200),
    labels  = c("0", "50", "100", "150", "200+")
  ) +
  
  scale_size(
    range  = c(2.5, 11),
    name   = "publications",
    breaks = c(1, 2, 3, 4, 5, 6)
  ) +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2025)) +
  
  theme_bw(base_size = 12) +
  labs(x = "Year", y = "Authors") +
  theme(
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey60", linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.text.y        = element_text(size = 10),
    legend.position    = "bottom",
    legend.box         = "horizontal"
  ) +
  guides(
    fill = guide_colorbar(barwidth = 12, barheight = 0.8, title.position = "top", order = 1),
    size = guide_legend(title.position = "top", nrow = 1, order = 2)
  )

print(p)

ggsave("C:/Users/MY/Downloads/authors_bubble_timeline.png",
       plot = p, width = 12, height = 7, dpi = 1200)

message("✓ Saved: authors_bubble_timeline.png")

