# Function that capitalizes the first letter of each word
# Options:
# Strict: TRUE:remove capital letters mid word
# Space: TRUE:keep space between words, FALSE: remove space between words
capwords <- function(s, strict = FALSE, space = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),{
    s <- substring(s, 2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  nameCap = sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  if (space == FALSE){
    nameCap = gsub(" ", "",nameCap) 
  }
  return(nameCap)
}
