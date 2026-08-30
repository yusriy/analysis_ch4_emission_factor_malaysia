#### Reproducible preparation of revised manuscript Figures 6 and 7 ####
#
# This script intentionally does not modify R/ch4_emission_factor.R, the
# manuscript-era analysis recorded at commit 14bdd43. It uses a frozen export
# of the corrected TIDBRepo table and explicit row/statistic rules so that the
# plotted values are auditable.

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
  library(stringr)
})

snapshot_path <- "data/raw/tidbrepo_methane_emission_factor_2026-08-28.json"
derived_dir <- "data/derived"
figure_dir <- "figs/revised"
manuscript_figure_dir <- "figs/manuscript"

dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manuscript_figure_dir, recursive = TRUE, showWarnings = FALSE)

snapshot_sha256 <- "f75a22d2a07e2fae777bd9a639c2b91ab21d6b07e0d1bfd2b46a46a27286c830"

if (!file.exists(snapshot_path)) {
  stop("Frozen TIDBRepo snapshot is missing: ", snapshot_path)
}

df <- fromJSON(snapshot_path, flatten = TRUE)
if (!is.data.frame(df) || nrow(df) != 131) {
  stop("Expected a 131-row frozen table; found ", nrow(df), " rows.")
}

numeric_fields <- c("tier", "min", "max", "median", "mean", "stdev", "scaling_factor")
df[numeric_fields] <- lapply(df[numeric_fields], function(x) {
  x[x == ""] <- NA
  suppressWarnings(as.numeric(x))
})

assert_rows <- function(data, expected_ids, label) {
  id_field <- if ("source_row_id" %in% names(data)) "source_row_id" else "id"
  found <- sort(data[[id_field]])
  expected <- sort(expected_ids)
  if (!identical(found, expected)) {
    stop(label, " row selection changed. Expected IDs ",
         paste(expected, collapse = ", "), "; found ",
         paste(found, collapse = ", "), ".")
  }
}

tier_colours <- c("Tier 1" = "#6B7280", "Tier 2" = "#1F77B4")
tier_shapes <- c("Tier 1" = 16, "Tier 2" = 17)

theme_manuscript <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(colour = "#222222"),
      plot.title = element_text(face = "bold", size = rel(1.25)),
      plot.subtitle = element_text(colour = "#444444", margin = margin(b = 8)),
      plot.caption = element_text(colour = "#555555", hjust = 0, size = rel(0.82),
                                  margin = margin(t = 8)),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0, colour = "#222222"),
      strip.background = element_rect(fill = "#F3F4F6", colour = NA)
    )
}

save_plot_set <- function(plot, stem, width_mm, height_mm,
                          output_dir = figure_dir) {
  ggsave(file.path(output_dir, paste0(stem, ".png")), plot,
         width = width_mm, height = height_mm, units = "mm", dpi = 400,
         device = ragg::agg_png, bg = "white")
  ggsave(file.path(output_dir, paste0(stem, ".svg")), plot,
         width = width_mm, height = height_mm, units = "mm",
         device = svglite::svglite, bg = "white")
  ggsave(file.path(output_dir, paste0(stem, ".tiff")), plot,
         width = width_mm, height = height_mm, units = "mm", dpi = 600,
         device = ragg::agg_tiff, compression = "lzw", bg = "white")
}

# -------------------------------------------------------------------------
# Figure 6: paired comparison plus the full Energy-table range
# -------------------------------------------------------------------------

paired_energy_ids <- c(4L, 5L, 6L, 7L, 8L, 9L)
all_energy_ids <- c(1L:11L, 20L:52L, 55L:68L)

energy_context <- df %>%
  filter(
    id %in% all_energy_ids,
    sectors == "Energy",
    tier %in% c(1, 2),
    unit == "kg CH₄ TJ⁻¹"
  ) %>%
  transmute(
    source_row_id = id,
    sector = sectors,
    category,
    condition = recode(condition, "Natural Gas" = "Natural gas"),
    tier = paste0("Tier ", tier),
    representative_value = min,
    statistic_basis = "scalar value recorded in min field",
    included_in_paired_panel = source_row_id %in% paired_energy_ids,
    unit,
    countries,
    doi_1, doi_2, doi_3, doi_4, doi_5, doi_6,
    snapshot_sha256
  )

assert_rows(energy_context, all_energy_ids, "Figure 6 full Energy context")

if (any(is.na(energy_context$representative_value)) ||
    any(energy_context$representative_value <= 0) ||
    !all(energy_context$unit == "kg CH₄ TJ⁻¹")) {
  stop("Figure 6 contains missing values or unexpected units.")
}

energy_paired <- energy_context %>%
  filter(included_in_paired_panel)

assert_rows(energy_paired, paired_energy_ids, "Figure 6 paired comparison")

paired_conditions <- energy_paired %>%
  count(condition, tier) %>%
  count(condition, name = "n_tiers")
if (any(paired_conditions$n_tiers != 2)) {
  stop("Figure 6 must contain exactly one Tier 1 and one Tier 2 value per condition.")
}

energy_paired <- energy_paired %>%
  mutate(condition = factor(condition,
                            levels = rev(c("Sub-bituminous coal", "Lignite", "Natural gas"))))

write.csv(energy_context,
          file.path(derived_dir, "figure_6_source_data.csv"),
          row.names = FALSE, na = "")

p6a <- ggplot(energy_paired,
              aes(x = representative_value, y = condition, fill = tier)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.58) +
  geom_text(aes(label = format(representative_value, trim = TRUE, nsmall = 2)),
            position = position_dodge(width = 0.72), hjust = -0.18,
            size = 3, colour = "#222222") +
  scale_fill_manual(values = tier_colours) +
  scale_x_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, 0.2),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    x = expression(paste("Methane emission factor (kg ", CH[4], " ", TJ^{-1}, ")"))
  ) +
  coord_cartesian(clip = "off") +
  theme_manuscript() +
  theme(plot.margin = margin(6, 16, 6, 6))

energy_context <- energy_context %>%
  mutate(tier = factor(tier, levels = c("Tier 1", "Tier 2")))

energy_max <- energy_context %>%
  slice_max(representative_value, n = 1, with_ties = FALSE)

if (energy_max$representative_value != 92) {
  stop("The expected full-table Energy maximum (92 kg CH₄ TJ⁻¹) has changed.")
}

p6b <- ggplot(energy_context,
              aes(x = representative_value, y = tier,
                  colour = tier, shape = tier)) +
  geom_point(position = position_jitter(width = 0, height = 0.09),
             alpha = 0.58, size = 2.2) +
  geom_point(data = energy_max, size = 3.2, alpha = 1) +
  geom_text(
    data = energy_max,
    aes(label = "92 (maximum)"),
    hjust = 1.03, vjust = -1.15, size = 3,
    colour = "#222222",
    show.legend = FALSE
  ) +
  scale_colour_manual(values = tier_colours, breaks = c("Tier 1", "Tier 2")) +
  scale_shape_manual(values = tier_shapes, breaks = c("Tier 1", "Tier 2")) +
  scale_x_log10(
    limits = c(3e-7, 150),
    breaks = c(1e-6, 1e-4, 1e-2, 1, 10, 100),
    labels = c("0.000001", "0.0001", "0.01", "1", "10", "100")
  ) +
  labs(
    title = "b) Full range recorded in the Energy table (logarithmic scale)",
    subtitle = "All 58 positive records are shown; maximum = 92 for Tier 1 natural gas in road transportation",
    x = expression(paste("Methane emission factor (kg ", CH[4], " ", TJ^{-1}, ")"))
  ) +
  theme_manuscript() +
  theme(legend.position = "none")

p6 <- patchwork::wrap_plots(p6a, p6b, ncol = 1, heights = c(1.45, 1)) +
  patchwork::plot_annotation(
    title = "Energy-sector methane emission factors",
    caption = str_wrap(
      paste0(
        "Source: frozen corrected TIDBRepo export (28 August 2026; SHA-256 ",
        substr(snapshot_sha256, 1, 12), "...). Panel a retains the readable paired comparison; ",
        "panel b supplies the full-table range without compressing the paired values."
      ),
      width = 120
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.caption = element_text(colour = "#555555", hjust = 0, size = 7.5)
    )
  )

save_plot_set(
  p6a,
  "figure_6_energy_emission_factors",
  180, 95,
  output_dir = manuscript_figure_dir
)

# -------------------------------------------------------------------------
# Figure 7: AFOLU measurements/factors and Waste BOD emission factors
# -------------------------------------------------------------------------

afolu_ids <- c(95L, 97L, 99L, 100L, 101L, 102L, 103L,
               108L, 109L, 110L, 112L, 113L, 114L, 115L)

afolu_labels <- c(
  `95` = "Pineapple: open field (dry and wet seasons)",
  `97` = "Peat: sago plantation",
  `99` = "Peat: drained oil-palm plantation",
  `100` = "Peat: bare soil",
  `101` = "Peat forest: mixed swamp",
  `102` = "Peat forest: Alan Batu",
  `103` = "Peat forest: Alan Bunga",
  `108` = "Rice: irrigated, continuously flooded",
  `109` = "Rice: irrigated, continuously flooded",
  `110` = "Rice: irrigated, multiple drainage",
  `112` = "Rice: rainfed",
  `113` = "Rice: upland",
  `114` = "River basins: disturbed",
  `115` = "River basins: undisturbed"
)

afolu <- df %>%
  filter(id %in% afolu_ids) %>%
  rowwise() %>%
  mutate(
    representative_value = case_when(
      tier == 1 & !is.na(mean) & !is.na(scaling_factor) ~ mean * scaling_factor,
      !is.na(mean) ~ mean,
      !is.na(median) ~ median,
      !is.na(min) & !is.na(max) ~ (min + max) / 2,
      !is.na(min) ~ min,
      TRUE ~ NA_real_
    ),
    statistic_basis = case_when(
      tier == 1 & !is.na(mean) & !is.na(scaling_factor) ~ "mean × scaling factor",
      !is.na(mean) ~ "mean",
      !is.na(median) ~ "median",
      !is.na(min) & !is.na(max) ~ "derived midpoint of reported range",
      !is.na(min) ~ "scalar value recorded in min field",
      TRUE ~ "unresolved"
    ),
    range_low = if_else(!is.na(min) & !is.na(max), min, NA_real_),
    range_high = if_else(!is.na(min) & !is.na(max), max, NA_real_)
  ) %>%
  ungroup() %>%
  transmute(
    source_row_id = id,
    panel = "a) AFOLU — kg CH₄ ha⁻¹ day⁻¹",
    sector = sectors,
    category,
    condition = unname(afolu_labels[as.character(id)]),
    tier = paste0("Tier ", tier),
    representative_value,
    range_low,
    range_high,
    statistic_basis,
    source_min = min,
    source_max = max,
    source_mean = mean,
    source_median = median,
    source_stdev = stdev,
    scaling_factor,
    unit,
    countries,
    doi_1, doi_2, doi_3, doi_4, doi_5, doi_6,
    snapshot_sha256
  )

assert_rows(afolu, afolu_ids, "Figure 7 AFOLU")

if (any(is.na(afolu$representative_value)) ||
    !all(afolu$unit == "kg CH₄ ha⁻¹ day⁻¹")) {
  stop("Figure 7 AFOLU contains unresolved values or unexpected units.")
}

waste_ids <- 122L:131L
waste <- df %>%
  filter(id %in% waste_ids) %>%
  transmute(
    source_row_id = id,
    panel = "b) Waste — kg CH₄ kg⁻¹ BOD",
    sector = sectors,
    category,
    condition,
    tier = paste0("Tier ", tier),
    representative_value = min,
    range_low = NA_real_,
    range_high = NA_real_,
    statistic_basis = "scalar value recorded in min field",
    source_min = min,
    source_max = max,
    source_mean = mean,
    source_median = median,
    source_stdev = stdev,
    scaling_factor,
    unit,
    countries,
    doi_1, doi_2, doi_3, doi_4, doi_5, doi_6,
    snapshot_sha256
  )

assert_rows(waste, waste_ids, "Figure 7 Waste")

if (any(is.na(waste$representative_value)) ||
    !all(waste$unit == "kg CH₄ kg⁻¹ BOD")) {
  stop("Figure 7 Waste contains missing values or unexpected units.")
}

fig7_data <- bind_rows(afolu, waste) %>%
  mutate(
    tier = factor(tier, levels = c("Tier 1", "Tier 2")),
    display_condition = str_wrap(condition, width = 43)
  )

afolu_order <- rev(unique(afolu$condition))
waste_order <- rev(waste$condition)
fig7_levels <- unique(c(str_wrap(afolu_order, width = 43),
                        str_wrap(waste_order, width = 43)))
fig7_data$display_condition <- factor(fig7_data$display_condition,
                                      levels = fig7_levels)

write.csv(fig7_data,
          file.path(derived_dir, "figure_7_source_data.csv"),
          row.names = FALSE, na = "")

p7 <- ggplot(fig7_data,
             aes(x = representative_value, y = display_condition,
                 colour = tier, shape = tier, group = tier)) +
  geom_blank() +
  geom_segment(
    data = fig7_data %>% filter(!is.na(range_low), !is.na(range_high)),
    aes(x = range_low, xend = range_high,
        y = display_condition, yend = display_condition),
    position = position_dodge(width = 0.5), linewidth = 0.65,
    alpha = 0.8, show.legend = FALSE
  ) +
  geom_point(position = position_dodge(width = 0.5), size = 2.6) +
  facet_wrap(~ panel, ncol = 1, scales = "free") +
  scale_colour_manual(values = tier_colours, breaks = c("Tier 1", "Tier 2")) +
  scale_shape_manual(values = tier_shapes, breaks = c("Tier 1", "Tier 2")) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  labs(
    title = "Selected AFOLU methane factors and fluxes, and Waste emission factors",
    subtitle = "Points are representative standardized values; horizontal lines show reported min–max ranges",
    x = "Representative value (panel-specific unit)",
    caption = str_wrap(
      paste0(
        "Source: frozen corrected TIDBRepo export (28 August 2026; SHA-256 ",
        substr(snapshot_sha256, 1, 12), "...). Representative-value rules: mean, then median, then scalar value; ",
        "a range midpoint is used only when no centre is reported. Tier 1 rice values apply the recorded scaling factor. ",
        "Conditions are heterogeneous and should not be interpreted as matched comparisons unless labels coincide."
      ),
      width = 105
    )
  ) +
  theme_manuscript(base_size = 8.5) +
  theme(
    strip.text = element_text(size = 9.5, margin = margin(5, 5, 5, 5)),
    panel.spacing = unit(10, "pt")
  )

# Separate panels are exported as insertion options for manuscripts that place
# Figure 7a and Figure 7b as two image objects under one caption.
make_panel_plot <- function(data, panel_label, x_label) {
  ggplot(data,
         aes(x = representative_value, y = display_condition,
             colour = tier, shape = tier, group = tier)) +
    geom_blank() +
    geom_segment(
      data = data %>% filter(!is.na(range_low), !is.na(range_high)),
      aes(x = range_low, xend = range_high,
          y = display_condition, yend = display_condition),
      position = position_dodge(width = 0.5), linewidth = 0.65,
      alpha = 0.8, show.legend = FALSE
    ) +
    geom_point(position = position_dodge(width = 0.5), size = 2.6) +
    scale_colour_manual(values = tier_colours, breaks = c("Tier 1", "Tier 2")) +
    scale_shape_manual(values = tier_shapes, breaks = c("Tier 1", "Tier 2")) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    labs(tag = panel_label, x = x_label) +
    theme_manuscript(base_size = 10.5) +
    theme(
      plot.tag = element_text(face = "bold", size = 11),
      plot.tag.position = c(0.01, 0.99)
    )
}

p7a <- make_panel_plot(
  fig7_data %>% filter(substr(panel, 1, 2) == "a)"),
  "a)",
  expression(paste("Representative value (kg ", CH[4], " ", ha^{-1}, " ", day^{-1}, ")"))
)

p7b <- make_panel_plot(
  fig7_data %>% filter(substr(panel, 1, 2) == "b)"),
  "b)",
  expression(paste("Methane emission factor (kg ", CH[4], " ", kg^{-1}, " BOD)"))
) +
  scale_x_continuous(
    limits = c(0, 0.28),
    breaks = seq(0, 0.25, 0.05),
    expand = expansion(mult = c(0, 0))
  )

save_plot_set(
  p7a,
  "figure_7a_afolu_emission_factors",
  190, 135,
  output_dir = manuscript_figure_dir
)
save_plot_set(
  p7b,
  "figure_7b_waste_emission_factors",
  190, 100,
  output_dir = manuscript_figure_dir
)

message("Prepared manuscript Figures 6 and 7 in ",
        normalizePath(manuscript_figure_dir))
