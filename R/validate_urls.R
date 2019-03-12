#' Validate that the url is a valid url
#' 
#' @export
#' @param urls a vector of urls
#' @param logical should the function return a vector of logical values?
#' @return a vector containing only the valid urls, or a vector of logical values

validate_url <- function(urls, logical = FALSE) {
  # re_url <- rex(start,
  #                    group("http", maybe("s"), "://"),
  #                    group(
  #                      maybe(zero_or_more(number),
  #                            "-")
  #                    ),
  #                      group(zero_or_more(alnum),
  #                            group(maybe(dot),
  #                                  "revues.org")
  #                      ) %or% 
  #                        group("books.openedition.org") %or%
  #                        group("calenda.org") %or%
  #                        group(
  #                          group(zero_or_more(alnum),
  #                                maybe(dot)),
  #                          "hypotheses.org"),
  #                    maybe("/", non_space %>% zero_or_more()),
  #                    end)
  re_url <- "^(?:http(?:s)?://)(?:(?:(?:[[:digit:]])*-)?)(?:(?:(?:(?:(?:[[:alnum:]])*(?:(?:\\.)?revues\\.org))|(?:books\\.openedition\\.org))|(?:calenda\\.org))|(?:(?:(?:[[:alnum:]])*(?:\\.)?)hypotheses\\.org))(?:/(?:[^[:space:]])*)?$"
  if (!logical) {
    grep(re_url, urls, value = TRUE)
  } else {
    grepl(re_url, urls)
  }
}


#' Replaces revues.org by journals.opendition.org
#' @export
#' @param urls a vector of urls
#' @return a vector of urls
fix_revues_org <- function(urls) {
    urls %>% 
    stringr::str_match("^([^.]+).revues.org/([0-9]+)$") %>% 
    dplyr::as_data_frame() %>% 
    dplyr::mutate(url = dplyr::if_else(is.na(V3), urls, paste0("journals.openedition.org/", V2, "/", V3))) %>% 
    dplyr::pull(url)
}

#' Replaces journals.opendition.org by revues.org
#' @export
#' @param urls a vector of urls
#' @return a vector of urls
unfix_revues_org <- function(urls) {
  urls %>% 
    stringr::str_match("^http://journals.openedition.org/([a-z0-9]+)/([0-9]+)$") %>% 
    dplyr::as_data_frame() %>% 
    dplyr::mutate(url = paste0("http://", V2, ".revues.org/", V3)) %>% 
    dplyr::pull(url)
}

#' Extract the canonical URLs
#' 
#' @export
#' @param urls a vector of urls
#' @return a vector of canonical urls
#' @import rex
 
correct_urls <- function(urls) {
  # rex_mode()
  re_url <- rex::rex(start,
                     anything,
                     #                group("http", maybe("s"), "://"),
                     "/",
                     group(
                       maybe(zero_or_more(number),
                             "-")
                     ),
                     capture(name = "url",
                             group(one_or_more(alnum),
                                   maybe("-", one_or_more(alnum)),
                                   dot,
                                   "revues.org") %or%
                               group("journals" %or% "books", ".openedition.org/", group(one_or_more(alnum),
                                                                                         maybe("-", one_or_more(alnum)))) %or%
                               group("calenda.org") %or%
                               group(one_or_more(alnum),
                                     maybe("-", one_or_more(alnum)),
                                     dot,
                                     "hypotheses.org"),
                             group(maybe("/", number %>% one_or_more()))
                     ),
                     anything,
                     end)
  
  
  tmp <- rex::re_matches(urls, re_url) 
  tmp$url <- fix_revues_org(tmp$url) 
  tmp$url <- dplyr::if_else(is.na(tmp$url), NA_character_, paste0("http://", tmp$url))
  tmp %>% pull(url)
}
