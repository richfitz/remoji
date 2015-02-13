#!/usr/bin/env Rscript
## This requires downloader and jsonlite

database_fetch <- function(overwrite=FALSE) {
  url <- "https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json"
  if (overwrite || !file.exists("emoji.json")) {
    downloader::download(url, "emoji.json")
  }
}

database_build <- function() {
  dat <- jsonlite::fromJSON(readLines("emoji.json"))
  dat <- dat[!is.na(dat$emoji),]
  n_alias <- sapply(dat$aliases, length)
  n_tag   <- sapply(dat$tags, length)

  dat_core <- dat[c("emoji", "description")]
  dat_alias <- data.frame(alias=unlist(dat$aliases),
                          dat_core[rep(seq_along(n_alias), n_alias),],
                          stringsAsFactors=FALSE)
  dat_tag <- data.frame(tag=unlist(dat$tags),
                        dat_core[rep(seq_along(n_tag), n_tag),],
                        stringsAsFactors=FALSE)

  save(dat_core, dat_alias, dat_tag, file="R/sysdata.rda")
}

if (!interactive()) {
  database_fetch()
  database_build()
}
