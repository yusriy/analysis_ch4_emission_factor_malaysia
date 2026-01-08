#### Malaysian Emission Factor Analysis ####
## Author: Yusri Yusup, Ph.D.##

##### Install and load packages #####

#install.packages(c("httr","jsonlite"))
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)



##### DBRepo Data Info #####

id_db <- "8cb5b734-7d57-4ca5-81a6-4d1de7acee88"
id_table <- "ff844ee0-03d8-4531-ae43-14f4bbdf4cd4"
url <- "https://tidbrepo.usm.my/"

base_url <- paste0("https://tidbrepo.usm.my/api/v1/database/",id_db,"/table/",id_table,"/data")
#base_url <- "https://tidbrepo.usm.my/api/v1/database/8cb5b734-7d57-4ca5-81a6-4d1de7acee88/table/ff844ee0-03d8-4531-ae43-14f4bbdf4cd4/data"



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
    category = factor(category)
  )

#### Energy Sector Analysis ####

df_energy <- df_clean %>%
  filter(sectors == "Energy")

df_t1 <- df_energy %>%
  filter(tier == 1) %>%
  select(countries, category, condition, min) %>%
  rename(min_t1 = min)

df_th <- df_energy %>%
  filter(tier != 1) %>%
  select(countries, category, condition, tier, min) %>%
  rename(min_th = min)

df_compare <- df_th %>%
  inner_join(
    df_t1,
    by = c("countries", "category", "condition")
  ) %>%
  mutate(
    abs_diff = min_th - min_t1,
    rel_diff = (min_th - min_t1) / min_t1
  )


nrow(df_compare)

# Check how many matches per tier
table(df_compare$tier)

# Identify categories with no Tier-1 counterpart
anti_join(
  df_energy %>% filter(tier != 1),
  df_t1,
  by = c("countries", "category", "condition")
) %>% distinct(category, condition)




# df_plot <- df_compare %>%
#   mutate(
#     cat_cond = ifelse(
#       is.na(condition) | condition == "",
#       as.character(category),
#       paste(category, condition, sep = " – ")
#     )
#   )

# df_plot <- df_compare %>%
#   mutate(
#     cond_label = ifelse(
#       is.na(condition) | condition == "",
#       "Unspecified condition",
#       condition
#     )
#   )

df_plot <- df_compare %>%
  mutate(
    rel_diff_pct = 100 * (min_th - min_t1) / min_t1,
    cond_label = ifelse(
      is.na(condition) | condition == "",
      "Unspecified condition",
      condition
    )
  )



# df_plot <- df_plot %>%
#   arrange(abs(abs_diff)) %>%
#   mutate(cat_cond = factor(cat_cond, levels = unique(cat_cond)))

df_plot <- df_plot %>%
  arrange(abs(rel_diff_pct)) %>%
  mutate(cond_label = factor(cond_label, levels = unique(cond_label)))


x_lim <- max(abs(df_plot$rel_diff_pct), na.rm = TRUE)



ggplot(df_plot, aes(x = rel_diff_pct, y = cond_label, fill = rel_diff_pct > 0)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(
    limits = c(-x_lim, x_lim),
    breaks = pretty(c(-x_lim, x_lim), n = 5),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_manual(values = c("grey60", "steelblue")) +
  theme_minimal() +
  labs(
    x = "Percentage difference in minimum emission factor (Higher Tier − Tier 1)",
    y = "Emission condition"
  )
