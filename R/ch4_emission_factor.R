#### Malaysian Emission Factor Analysis ####
## Author: Yusri Yusup, Ph.D.##

##### Install and load packages #####

#install.packages(c("httr","jsonlite"))
#install.packages("ggh4x")
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggh4x)


##### DBRepo Data Info #####

id_db <- "8cb5b734-7d57-4ca5-81a6-4d1de7acee88"
id_table <- "ff844ee0-03d8-4531-ae43-14f4bbdf4cd4"
url <- "https://tidbrepo.usm.my/"

base_url <- paste0("https://tidbrepo.usm.my/api/v1/database/",id_db,"/table/",id_table,"/data")


#### Download Data ####
fetch_view_all <- function(base_url, size = 500, max_pages = 20, start_page = 0) {
  pages <- list()
  
  for (p in start_page:(start_page + max_pages - 1)) {
    
    resp <- GET(
      base_url,
      add_headers(Accept = "application/json"),
      query = list(page = p, size = size)
    )
    stop_for_status(resp)
    
    txt <- content(resp, as = "text", encoding = "UTF-8")
    obj <- fromJSON(txt, flatten = TRUE)
    
    # The response schema says: { "type": "string" } (not very informative),
    # so we handle common shapes:
    df <- NULL
    if (is.data.frame(obj)) {
      df <- obj
    } else if (!is.null(obj$rows)) {
      df <- obj$rows
    } else if (!is.null(obj$data)) {
      df <- obj$data
    } else if (is.list(obj) && length(obj) > 0 && is.data.frame(obj[[1]])) {
      df <- do.call(rbind, obj)
    }
    
    if (is.null(df) || nrow(df) == 0) break
    
    pages[[length(pages) + 1]] <- df
    
    # early stop if last page (common behavior)
    if (nrow(df) < size) break
  }
  
  if (length(pages) == 0) return(data.frame())
  do.call(rbind, pages)
}


#### Import to Workspace ####

df <- fetch_view_all(base_url, size = 500, max_pages = 20, start_page = 0)


#### Manage Data ####

df_clean <- df %>%
  mutate(
    tier = factor(tier),
    countries = factor(countries),
    sectors = factor(sectors),
    category = factor(category),
    unit = factor(unit)
  )


# ==========================================
# FILTER BY SECTOR -> UNIT, KEEP UNIT GROUPS
# THAT CONTAIN BOTH TIER 1 AND TIER 2
# ==========================================

df_sector_unit <- df_clean %>%
  filter(
    sectors %in% c("Energy", "AFOLU", "Waste"),
    tier %in% c(1, 2),
    !is.na(unit), unit != ""
  ) %>%
  group_by(sectors, unit) %>%
  mutate(
    has_tier1 = any(tier == 1),
    has_tier2 = any(tier == 2)
  ) %>%
  ungroup() %>%
  filter(has_tier1 & has_tier2) %>%
  select(-has_tier1, -has_tier2)

# Quick check: which sector-unit combos will be plotted?
sector_unit_kept <- df_sector_unit %>%
  distinct(sectors, unit) %>%
  arrange(sectors, unit)

print(sector_unit_kept)



# ==========================================
# FILTER BY SECTOR -> UNIT, KEEP UNIT GROUPS
# THAT CONTAIN BOTH TIER 1 AND TIER 2
# ==========================================

df_sector_unit <- df_clean %>%
  filter(
    sectors %in% c("Energy", "AFOLU", "Waste"),
    tier %in% c(1, 2),
    !is.na(unit), unit != ""
  ) %>%
  group_by(sectors, unit) %>%
  mutate(
    has_tier1 = any(tier == 1),
    has_tier2 = any(tier == 2)
  ) %>%
  ungroup() %>%
  filter(has_tier1 & has_tier2) %>%
  select(-has_tier1, -has_tier2)

# Quick check: which sector-unit combos will be plotted?
sector_unit_kept <- df_sector_unit %>%
  distinct(sectors, unit) %>%
  arrange(sectors, unit)

print(sector_unit_kept)

# Choose the sector with the most rows for plotting

main_unit_per_sector <- df_sector_unit %>%
  count(sectors, unit, name = "n") %>%
  group_by(sectors) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

df_plot <- df_sector_unit %>%
  inner_join(main_unit_per_sector %>% select(sectors, unit),
             by = c("sectors", "unit"))

# Check chosen unit per sector
print(main_unit_per_sector)



df_plot <- df_plot %>%
  mutate(
    diff_from_baseline = min,
    cond_label = ifelse(is.na(condition) | condition == "", "Unspecified condition", condition),
    tier_label = paste0("Tier ", tier)
  )

# ---- 2. Quantile-based filtering (10th–90th percentile, per sector) ----
df_filtered <- df_plot %>%
  group_by(sectors) %>%
  mutate(
    q_low  = quantile(diff_from_baseline, 0.05, na.rm = TRUE),
    q_high = quantile(diff_from_baseline, 0.95, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(
    diff_from_baseline >= q_low,
    diff_from_baseline <= q_high
  ) %>%
  select(-q_low, -q_high)

unit_to_expression <- function(unit_str) {
  
  if (unit_str == "kg CH4 ha-1 yr-1") {
    return(
      bquote(
        CH[4] * " emission factor (kg " * CH[4] *
          " ha"^{-1} * " yr"^{-1} * ")"
      )
    )
  }
  
  if (unit_str == "kg TJ-1") {
    return(
      bquote(
        CH[4] * " emission factor (kg " * CH[4] * TJ^{-1} * ")"
      )
    )
  }
  
  if (unit_str == "kg CH4 kg-1 BOD") {
    return(
      bquote(
        CH[4] * " emission factor (kg " * CH[4] *
          " kg"^{-1} * " BOD)"
      )
    )
  }
  
  # fallback (safe and correct)
  bquote(CH[4] * " emission factor (" * .(unit_str) * ")")
}



# ---- 4. Plotting function (linear scale, no negative axis) ----


plot_sector_linear <- function(df, sector_name) {
  
  library(stringr)
  
  df_s <- df %>% 
    filter(sectors == sector_name) %>%
    mutate(cond_label = str_wrap(cond_label, width = 30))
  
  unit_used <- unique(df_s$unit)
  
  if (length(unit_used) != 1) {
    stop(
      "Multiple units found for sector ", sector_name,
      ": ", paste(unit_used, collapse = ", ")
    )
  }
  
  x_label_expr <- unit_to_expression(unit_used)
  
  text_size <- if (sector_name == "AFOLU") 9 else 10
  
  ggplot(
    df_s,
    aes(
      x = diff_from_baseline,
      y = cond_label,
      fill = factor(tier)
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.75),
      width = 0.45                     # thinner bars
    ) +
    scale_y_discrete(
      expand = expansion(add = 0.8)    # more vertical spacing
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(
      values = c("grey70", "steelblue"),
      labels = c("Tier 1", "Tier 2"),
      name = "Tier"
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = text_size),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 20, unit = "pt")
    ) +
    labs(
      title = paste0(sector_name, " sector"),
      x = x_label_expr,
      y = "Emission condition"
    )
}

# ---- 5. Generate plots ----
p_energy <- plot_sector_linear(df_filtered, "Energy")
p_afolu  <- plot_sector_linear(df_filtered, "AFOLU")
p_waste  <- plot_sector_linear(df_filtered, "Waste")

# ---- 6. Print plots ----
print(p_energy)
print(p_afolu)
print(p_waste)



#### Export the figures ####

save_sector_plot <- function(plot_obj, filename, width_cm = 15, row_height_cm = 0.6) {
  
  n_rows <- length(unique(plot_obj$data$cond_label))
  height_cm <- n_rows * row_height_cm
  
  ggsave(
    filename = filename,
    plot     = plot_obj,
    width    = width_cm,
    height   = height_cm,
    units    = "cm",
    dpi      = 400
  )
}

save_sector_plot(p_afolu, "figs/afolu_emission_factors.png",
                 row_height_cm = 1.2)
save_sector_plot(p_waste,  "figs/waste_emission_factors.png")
save_sector_plot(p_energy, "figs/energy_emission_factors.png")
