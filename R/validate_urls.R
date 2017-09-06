#' Validate that the url is a valid url
#' 
#' @export
#' @param urls a vector of urls
#' @return a vector containing only the valid urls

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

