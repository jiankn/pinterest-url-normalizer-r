.pinterest_country_hosts <- c(
  "pinterest.at", "pinterest.be", "pinterest.ca", "pinterest.ch",
  "pinterest.cl", "pinterest.co", "pinterest.co.kr", "pinterest.co.nz",
  "pinterest.co.uk", "pinterest.com.au", "pinterest.com.br",
  "pinterest.com.mx", "pinterest.com.pe", "pinterest.com.tr",
  "pinterest.cz", "pinterest.de", "pinterest.dk", "pinterest.es",
  "pinterest.fi", "pinterest.fr", "pinterest.gr", "pinterest.hu",
  "pinterest.id", "pinterest.ie", "pinterest.it", "pinterest.jp",
  "pinterest.nl", "pinterest.no", "pinterest.ph", "pinterest.pl",
  "pinterest.pt", "pinterest.ro", "pinterest.se", "pinterest.sk"
)

.pinterest_regional_subdomains <- c(
  "at", "au", "be", "br", "ca", "ch", "cl", "co", "cz", "de", "dk",
  "es", "fi", "fr", "gr", "hu", "id", "ie", "it", "jp", "kr", "mx",
  "nl", "no", "nz", "pe", "ph", "pl", "pt", "ro", "se", "sk", "tr",
  "uk"
)

.pinterest_reserved_paths <- c(
  "business", "categories", "explore", "help", "ideas", "login",
  "logout", "oauth", "pin", "pin-builder", "resource", "search", "settings",
  "signup", "today", "topics"
)

.pinterest_error <- function(code, message) {
  condition <- structure(
    list(message = message, call = NULL, code = code),
    class = c("pinterest_url_error", "error", "condition")
  )
  stop(condition)
}

.is_pinterest_host <- function(host) {
  host <- tolower(host)
  host %in% c("pinterest.com", "www.pinterest.com", "m.pinterest.com") ||
    host %in% .pinterest_country_hosts ||
    (startsWith(host, "www.") && substring(host, 5L) %in% .pinterest_country_hosts) ||
    (
      endsWith(host, ".pinterest.com") &&
        sub("\\.pinterest\\.com$", "", host) %in% .pinterest_regional_subdomains
    )
}

.split_path <- function(path) {
  trimmed <- sub("^/", "", sub("/$", "", path))
  if (!nzchar(trimmed)) character() else strsplit(trimmed, "/", fixed = TRUE)[[1L]]
}

#' Parse a Pinterest URL
#'
#' Parses one URL without following redirects or making any network request.
#'
#' @param url A single character string.
#' @return A list with `kind`, `normalized_url`, and `identifier`.
#' @export
parse_pinterest_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url)) {
    .pinterest_error("INVALID_URL", "url must be one non-missing character string")
  }

  value <- trimws(url)
  if (!nzchar(value) || nchar(value, type = "bytes") > 2048L || grepl("\\\\", value)) {
    .pinterest_error("INVALID_URL", "URL is empty, too long, or contains a backslash")
  }

  match <- regexec("^https://([^/?#]+)(/[^?#]*)?(?:[?#].*)?$", value, perl = TRUE)
  parts <- regmatches(value, match)[[1L]]
  if (!length(parts)) {
    .pinterest_error("INVALID_URL", "an absolute HTTPS URL is required")
  }

  host <- tolower(parts[[2L]])
  path <- if (length(parts) >= 3L && nzchar(parts[[3L]])) parts[[3L]] else "/"
  if (grepl("[@:]", host) || grepl("%", path, fixed = TRUE)) {
    .pinterest_error("INVALID_URL", "credentials, ports, and encoded paths are not supported")
  }

  segments <- .split_path(path)

  if (identical(host, "pin.it")) {
    if (length(segments) != 1L || !grepl("^[A-Za-z0-9]{2,}$", segments[[1L]])) {
      .pinterest_error("UNSUPPORTED_URL", "unsupported pin.it path")
    }
    return(list(
      kind = "short",
      normalized_url = paste0("https://pin.it/", segments[[1L]], "/"),
      identifier = segments[[1L]]
    ))
  }

  if (!.is_pinterest_host(host)) {
    .pinterest_error("INVALID_URL", "host is not an allowed Pinterest domain")
  }

  if (length(segments) %in% c(2L, 3L) && identical(segments[[1L]], "pin")) {
    pin <- segments[[2L]]
    identifier <- if (grepl("^[0-9]{1,20}$", pin)) {
      pin
    } else {
      sub("^.*--([0-9]{1,20})$", "\\1", pin, perl = TRUE)
    }
    trailing_ok <- length(segments) == 2L || grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", segments[[3L]])
    if (grepl("^[0-9]{1,20}$", identifier) && trailing_ok) {
      return(list(
        kind = "pin",
        normalized_url = paste0("https://www.pinterest.com/pin/", identifier, "/"),
        identifier = identifier
      ))
    }
  }

  if (
    length(segments) == 3L && identical(segments[[1L]], "ideas") &&
      grepl("^[A-Za-z0-9][A-Za-z0-9_-]*$", segments[[2L]]) &&
      grepl("^[0-9]{1,20}$", segments[[3L]])
  ) {
    return(list(
      kind = "ideas",
      normalized_url = paste0(
        "https://www.pinterest.com/ideas/", segments[[2L]], "/", segments[[3L]], "/"
      ),
      identifier = segments[[3L]]
    ))
  }

  username_pattern <- "^[A-Za-z0-9_][A-Za-z0-9_.-]*$"
  slug_pattern <- "^[A-Za-z0-9][A-Za-z0-9_-]*$"
  if (
    length(segments) == 1L && grepl(username_pattern, segments[[1L]]) &&
      !(tolower(segments[[1L]]) %in% .pinterest_reserved_paths)
  ) {
    return(list(
      kind = "profile",
      normalized_url = paste0("https://www.pinterest.com/", segments[[1L]], "/"),
      identifier = segments[[1L]]
    ))
  }

  if (
    length(segments) == 2L && grepl(username_pattern, segments[[1L]]) &&
      grepl(slug_pattern, segments[[2L]]) &&
      !(tolower(segments[[1L]]) %in% .pinterest_reserved_paths)
  ) {
    return(list(
      kind = "board",
      normalized_url = paste0(
        "https://www.pinterest.com/", segments[[1L]], "/", segments[[2L]], "/"
      ),
      identifier = segments[[1L]]
    ))
  }

  .pinterest_error("UNSUPPORTED_URL", "unsupported Pinterest path")
}

#' Normalize a Pinterest URL
#'
#' @inheritParams parse_pinterest_url
#' @return A canonical URL string.
#' @export
normalize_pinterest_url <- function(url) {
  parse_pinterest_url(url)$normalized_url
}

#' Test whether a URL is a supported Pinterest URL
#'
#' @inheritParams parse_pinterest_url
#' @return `TRUE` for a supported URL, otherwise `FALSE`.
#' @export
is_pinterest_url <- function(url) {
  tryCatch(
    {
      parse_pinterest_url(url)
      TRUE
    },
    pinterest_url_error = function(error) FALSE
  )
}
