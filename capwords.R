## Function that creates camelCase text from strings
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
