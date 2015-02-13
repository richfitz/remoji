##' Returns the unicode character for an emoji with given alias.
##' These cheatsheet at http://emoji-cheat-sheet.com will probably
##' help, or use \code{\link{find_emoji}} to find an emoji.  The
##' database comes from https://github.com/github/gemoji
##' @title Return unicode
##' @param alias Character vector of emoji aliases.
##' @param pad Logical indicating if the returned string should be
##' padded with a single space?  This seems to help printing.
##' @param must_work Logical indicating that a look failure should
##' cause an error.
##' @return A character vector the same length as \code{alias}
##' @export
##' @examples
##' emoji("+1")
##' message(emoji("+1"))
##' message(emoji(c("fire", "computer"), TRUE)[c(1, 1, 1, 2, 1, 1, 1)])
emoji <- function(alias, pad=FALSE, must_work=FALSE) {
  ret <- dat_alias$emoji[match(alias, dat_alias$alias)]
  msg <- is.na(ret)

  if (must_work && any(msg)) {
    err <- unique(alias[msg])
    pos <- lapply(err, find_emoji, approximate=TRUE)
    n <- sapply(pos, length)
    pos[n == 0] <- "<no suggestion>"
    pos <- sapply(pos, function(x) paste(x, collapse=", "))
    stop(emoji("crying_cat_face", TRUE),
         emoji("computer", TRUE),
         " Invalid emoji!  Suggestions: ",
         emoji("smiley_cat", TRUE), emoji("+1", TRUE), "\n",
         paste(sprintf("\t * %s: %s", err, pos), collapse="\n"))
  }

  if (pad) {
    ret[!msg] <- paste0(ret[!msg], " ")
  }
  ret
}

##' Find emoji by alias, tag or full name.  \code{grep} is used for
##' the match, allowing some flexibility.
##' @title Find emoji
##' @param str String to search for
##' @param type Field to look in - tag, alias or description
##' @param fixed Use fixed string search?  The default is a regular
##' expression.
##' @param approximate Use approximate string matches?
##' @return Vector of possible aliases.  These can be fed into
##' \code{\link{emoji}} or used in string substitution/interpolation
##' for \code{sub_emoji}.
##' @export
##' @examples
##' find_emoji("person")
##' find_emoji("person", "description")
##' emoji(find_emoji("person", "description"))
##' message(emoji(find_emoji("person", "description"), pad=TRUE))
find_emoji <- function(str, type=c("alias", "tag", "description"),
                       fixed=FALSE, approximate=FALSE) {
  type <- match.arg(type)
  dat <- switch(type, tag=dat_tag, alias=dat_alias, description=dat_core)
  if (approximate) {
    i <- agrep(str, dat[[type]])
  } else {
    i <- grep(str, dat[[type]], fixed=fixed)
  }
  unique(dat_alias$alias[match(dat$description[i], dat_alias$description)])
}
##' @rdname find_emoji
##' @export
list_emoji <- function(type=c("alias", "tag", "description")) {
  type <- match.arg(type)
  switch(type, tag=dat_tag, alias=dat_alias, description=dat_core)[[type]]
}

##' Emoji string interpolation in the style of github.  Intersperse
##' emoji aliases surrounded by colons with text and the returned
##' string will replace the aliases with unicode emoji, optionally
##' padded so that they print nicely.
##' @title Emoji string interpolation
##' @param str A string containing emoji aliases surrounded by
##' colons.
##' @param pad Should each emoji be padded with a single trailing
##' space?  The default is \code{TRUE} as this looks heaps better here.
##' @export
sub_emoji <- function(str, pad=TRUE) {
  if (length(str) > 1) {
    str <- sapply(str, sub_emoji, pad, USE.NAMES=FALSE)
  } else {
    re <- emoji_re()
    match <- regexpr(re, str)
    if (match > 0L) {
      end <- match + attr(match, "match.length", exact=TRUE) - 1L
      alias <- gsub(":", "", substr(str, match, end), fixed=TRUE)
      e <- emoji(alias, pad, must_work=TRUE)
      str <- sub_emoji(sub(re, e, str), pad)
    }
  }
  str
}
##' @export
##' @rdname sub_emoji
unsub_emoji <- function(str, pad=TRUE) {
  if (length(str) > 1) {
    sapply(str, unsub_emoji, pad, USE.NAMES=FALSE)
  } else {
    ## First, we have to split apart the entire string:
    str <- strsplit(str, NULL)[[1]]
    i <- match(str, dat_alias$emoji)
    ## Swap out matches:
    j <- !is.na(i)
    str[j] <- paste0(":", dat_alias$alias[i[j]], ":")

    if (pad) {
      k <- which(j) + 1L
      k <- k[k <= length(str) & str[k] == " "]
      if (length(k) > 0) {
        str <- str[-k]
      }
    }
    paste(str, collapse="")
  }
}

##' Print a table of emoji.  May be useful for filtering through to
##' find the right emoji, especially with \code{\link{find_emoji}}.
##' @title Table of emoji
##' @param alias Vector of aliases
##' @export
##' @examples
##' emoji_table(find_emoji("frown", approximate=TRUE))
emoji_table <- function(alias) {
  if (length(alias) == 0) {
    stop("No aliases given ", rep(emoji("fire", TRUE), 25))
  }
  fmt <- paste0("%", max(nchar(alias)), "s : %s")
  str <- paste(sprintf(fmt, alias, emoji(alias)), collapse="\n")
  message(str)
}

emoji_re <- function() {
  ":[-A-Za-z_0-9+]+:"
}

contains_emoji <- function(str) {
  grepl(emoji_re(), str)
}
