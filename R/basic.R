#' @title replace # with %23
#' @param x character
#' @examples
#' repash('#1234')
#' @importFrom stringr str_replace
#' @export
repash <- function(x) {
  str_replace(x, '#', '%23')
}
################################################

#' @title load a token file
#' @examples
#' token('./token1.txt')
#' @export
token <- function(file){
  scan(file, what = 'c')
}
################################################

#' @title load clan data 
#' @param tag clan tag
#' @param token a token 
#' @examples
#' load_clan( tag = "#282UJRU28", token('~/dev/rcoc/tokens/token001'))
#' @importFrom httr GET stop_for_status content add_headers
#' @export
load_clan  <- function(tag, auth_key ){
  url <- paste( "https://api.clashofclans.com/v1/clans",  repash(tag), sep= '/')
  r <- GET(url, add_headers(Authorization = paste("Bearer", auth_key, sep = " ")))
  r <- content(r, as = "parsed") 
  return(r)
}
  

  




