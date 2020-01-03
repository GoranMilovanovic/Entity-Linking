### --- WSD against Wikidata entities
### --- Experiment w. simple structural representations only
### --- Goran S. Milovanović, DataKolektiv, WMDE
### --- 24. December, 2019.

library(httr)
library(jsonlite)
library(tidyverse)

### --- functions

# - fetch_clean_wiki_text()
fetch_clean_wiki_text <- function(target_page) {
  
  # - API prefix
  API_prefix <- 
    paste0('https://en.wikipedia.org/w/api.php?', 
           'format=json', 
           '&action=query',
           '&prop=extracts', 
           '&exlimit=max', 
           '&explaintext', 
           '&redirects=', 
           '&exsectionformat=plain', 
           '&utf8=&titles=')
  
  # - fetch full page text from English Wikipedia
  API_call <- URLencode(paste0(API_prefix, target_page))
  # - make API call
  repeat {
    r <- tryCatch({httr::GET(API_call)
    }, 
    error = function(condition) {
      message("Curl error; wait 5 secs, try again.")
      Sys.sleep(5)
      httr::GET(API_call)
    })
    if (class(r) == "response") {
      if (r$status == 200) {
        break
      }
    }
  }
  # - raw to character representation:
  r <- rawToChar(r$content)
  # - parse JSON
  r <- fromJSON(r)
  r <- r$query$pages[[1]]$extract
  # - clean-up the API response a bit
  r <- gsub("\\\\n", " ", r)
  r <- gsub("\\s+", " ", r)
  # - return
  return(r)
}

# - unnest_udpipe_ngrams()
unnest_udpipe_ngrams <- function(tokens_dataframe) {
  d <- rbindlist(
    lapply(unique(tokens_dataframe$end),
           function(x) {
             d <- tokens_dataframe[which(tokens_dataframe$end == x), ]
             w <- which.max(nchar(d$keyword))
             d[w, ]
             })
    )
  d <- rbindlist(
    lapply(unique(d$start),
           function(x) {
             d <- d[which(d$start == x), ]
             w <- which.max(nchar(d$keyword))
             d[w, ]
           })
  )
  return(d)
}

# - fetch_sitelinks()
fetch_sitelinks <- function(target_page) {
  # - English Wikipedia MediaWiki API prefix
  APIprefix <- 'https://en.wikipedia.org/w/api.php?'
  # - store results
  links <- list()
  # - compose initial query
  query <- paste0(APIprefix,
                  paste0('action=query&prop=links&format=json&titles=', target_page)
  )
  # - count
  counter = 1
  repeat {
    # - contact the API
    result <- GET(url = URLencode(query))
    # - parse result
    # - raw to char
    result <- rawToChar(result$content)
    # - to JSON:    
    result <- fromJSON(result, simplifyDataFrame = T)
    # - content:
    links[[counter]] <- result$query$pages[[1]]$links
    # - check if there are more results
    if (!is.null(result$continue$plcontinue)) {
      # - pick up continuation parameters
      plcontinue <- result$continue$plcontinue
      # - increase counter
      counter <- counter + 1
      # - Compose continuation query
      query <- paste0(APIprefix,
                      'action=query&prop=links&format=json&titles=',
                      target_page, '&plcontinue=', plcontinue)
    } else {
      break
    }
  }
  # - compose links
  links <- rbindlist(links,
                     use.names = T,
                     fill = T)
  # - filter links for NS = 0
  links <- filter(links, ns == 0)
  links <- links[!duplicated(links), ]
  # - fetch sitelinks
  # - English Wikipedia MediaWiki API prefix
  APIprefix <- 'https://en.wikipedia.org/w/api.php?'
  # - store results
  sitelinks <- character(length(links$title))
  # - compose query prefix
  query <- paste0(APIprefix,
                  'action=query&prop=pageprops&format=json&titles='
                  )
  # - iterate over links$title
  for (i in 1:length(links$title)) {
    # - contact the API
    tpagetitle <- gsub(" ", "_", links$title[i])
    tquery <- paste0(query, tpagetitle)
    result <- GET(url = URLencode(tquery))
    # - parse result
    # - raw to char
    result <- rawToChar(result$content)
    # - to JSON:
    result <- fromJSON(result, simplifyDataFrame = T)
    if (!is.null(result$query$pages[[1]]$pageprops$wikibase_item)) {
      sitelinks[i] <- result$query$pages[[1]]$pageprops$wikibase_item 
    } else {
      sitelinks[i] <- NA
    }
  }
  # - add sitelinks to links
  links$sitelink <- sitelinks
  # - return
  links$ns <- NULL
  return(links)
}

# - search_wikidata()
search_wikidata <- function(phraseVector) {
  
  # - parse parameter vector
  originalPhrase <- unname(phraseVector[1])
  phrase <- unname(phraseVector[2])
  
  # - WikidataR::find_item
  repeat {
    r <- tryCatch({WikidataR::find_item(phrase)
    }, 
    error = function(condition) {
      print("Curl error; wait 5 secs, try again.")
      Sys.sleep(5)
      WikidataR::find_item(phrase)
    })
    if (class(r) == "find_item") {
      break
    }
  }
  
  # - resulting labels
  l <- sapply(r, function(y) {y$label})
  l <- unname(sapply(l, function(x) {ifelse(is.null(x), NA, x)}))
  # - resulting aliases
  a <- sapply(r, function(y) {y$aliases})
  a <- unlist(sapply(a, function(x) {ifelse(is.null(x), NA, x)}))
  # - resulting concepts
  c <- sapply(r, function(y) {y$concepturi})
  # - resulting descriptions
  d <- sapply(r, function(y) {y$description})
  d <- unname(unlist(sapply(d, function(x) {ifelse(is.null(x), NA, x)})))
  
  # - search through labels and aliases
  # - for an exact match w/o. capitalization
  # - including "^" and "$" in regex **for labels and aliases**:
  lSearch <- grepl(tolower(phrase),  tolower(l), fixed = F)
  lSearch[is.na(lSearch)] <- F
  aSearch <- grepl(tolower(phrase), tolower(a), fixed = T)
  aSearch[is.na(aSearch)] <- F
  search <- which(lSearch + aSearch >= 1)
  
  # - if x in labels and/or aliases
  if (length(search) > 0) {
    return(
      data.frame(
        searchText = originalPhrase,
        immediateSearchText = phrase,
        label = l[search],
        alias = a[search],
        description = d[search],
        uri = c[search],
        stringsAsFactors = F))
    # - else: it is missing...
  } else {
    return(data.frame(
      searchText = originalPhrase,
      immediateSearchText = phrase,
      label = NA,
      alias = NA,
      description = NA,
      uri = NA,
      stringsAsFactors = F))
  }
}



