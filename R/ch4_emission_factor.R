#### Malaysian Emission Factor Analysis ####
## Author: Yusri Yusup, Ph.D.##

##### Install and load packages #####

#install.packages(c("httr","jsonlite"))
library(httr)
library(jsonlite)



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







