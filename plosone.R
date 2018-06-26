# load libraries
library(tidyverse)
library(xml2)
library(lubridate)

# setup feed source
feed <- "http://journals.plos.org/plosone/feed/atom"

# import feed
feed <- read_xml(feed)
feed_list <- feed %>%
  xml_children() %>%
  as_list()

# transform feed
plosone_prep <- map(feed_list, "content") %>%
  bind_cols() %>%
  gather(key, value) %>%
  separate(value, c("Author", "Abstract"), sep = "</p>\n") %>%
  filter(row_number() > 2)

grep_plosone <- function(feed_list) {
  plosone <- matrix(ncol = 5, nrow = nrow(plosone_prep))
  for (i in 1:nrow(plosone_prep)) {
    plosone[i, 1] <- unlist(feed_list[[i+9]]$title) #title
    plosone[i, 2] <- attr(feed_list[[i+9]]$link, "href") # link
    plosone[i, 3] <- unlist(feed_list[[i+9]]$author[[1]]) # first author
    plosone[i, 4] <- unlist(feed_list[[i+9]]$id) # doi
    plosone[i, 5] <- unlist(feed_list[[i+9]]$published) # published
  }
  plosone <- as_tibble(plosone)
  names(plosone) <- c("Titel", "URL", "First_Author", "doi", "published")

  foo <- plosone_prep %>%
    mutate(Author   = gsub("<p>by ", "", Author),
           Author   = gsub("</p>", "", Author),
           Abstract = gsub("\n", "", Abstract),
           Abstract = gsub("<p>", "", Abstract),
           Abstract = gsub("</p>", "", Abstract),
           Abstract = gsub("<i>", "", Abstract),
           Abstract = gsub("</i>", "", Abstract)) %>%
    select(-key)
  #df <- list(foo, plosone)
  df <- bind_cols(plosone, foo)
  df$published <- as_date(df$published)
  return(df)
}
feed_plosone <- grep_plosone(feed_list)

# save feed daily
time <- gsub("-", "", as_date(Sys.time()))
save(feed_plosone, file = paste0("data/", time, "_feed_export.RData"))

