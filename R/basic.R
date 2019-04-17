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
#' @importFrom httr GET add_headers content
#' @examples
#' load_clan( tag = "#282UJRU28", token('~/dev/rcoc/tokens/token001'))
#' @importFrom httr GET stop_for_status content add_headers
#' @export
load_clan  <- function(tag, auth_key ){
  url <- paste( "https://api.clashofclans.com/v1/clans",  repash(tag), sep= '/')
  clan <- GET(url, add_headers(Authorization = paste("Bearer", auth_key, sep = " ")))
  clan <- content(clan, as = "parsed")
  class(clan) <-  'clan'
  return(r)
}
################################################
#' @title info S3 generic method
#' @param An R object. Currently there are methods for clan objects
#' @export
info <- function(x, ...) {
  UseMethod('info', x)
}
###############################################
#' @title info S3 generic method
#' @param An R object. Currently there are methods for clan objects
#' @export
member <- function(x, ...) {
  UseMethod('member', x)
}
###############################################
#' @title Returns info about the ith member of the clan
#' @param i integer: ith member of the clan
#' @param x: an object of class clan
#' @examples
#' .member ( 4, x = my_clan)
#' @importFrom tibble tibble
#' @export
.member <- function(i,  x, d = lubridate::now()){
  member <- x$memberList[[i]]
  tibble(
    date = d,
    tag = member$tag,
    name = member$name,
    role = member$role,
    exp_level = member$expLevel,
    league =member$league$name,
    trophies = member$trophies,
    versus_trophies = member$versusTrophies,
    clan_rank = member$clanRank,
    previous_clan_rank =  member$previousClanRank,
    donations = member$donations,
    donations_received = member$donationsReceived
  )
}
###############################################
#' @title Member method for objects of class clan.
#' @description Returns a tibble of info about all members of the clan
#' @param x: an object of class clan
#' @examples
#' member (my_clan)
#' @importFrom dplyr bind_rows
#' @export
member.clan <- function(x){
  n <- x$members
  member_list <-  lapply(seq_len(n) , .member,  x )
  bind_rows(member_list)

}
###############################################
#' @title Info method for objects of class clan.
#' @description Returns a tibble of info about the clan
#' @param x: an object of class clan
#' @examples
#' info (my_clan)
#' @importFrom tibble tibble
#' @importFrom lubridate now
#' @export

# f <- function(x, date =  lubridate::now()){
#   dplyr::tibble(date = date, tt = 0)
# }
#
# f(1)

info.clan <- function(x, d =  lubridate::now()){

  # tibble(
  #   info = c( "tag", "name","members", "type","location","clan level",
  #             "clan points","clan versus points","required trophies","war frequency",
  #             "war win streak","war wins","war ties","war losses","is war log public"),
     tibble (
      date = d,
      tag = x$tag,
      name = x$name,
      members = x$members,
      type = x$type,
      location_name = x$location$name,
      clan_level = x$clanLevel,
      clan_points = x$clanPoints,
      clan_versus_point = x$clanVersusPoints,
      required_trophies = x$requiredTrophies,
      war_frequency = x$warFrequency,
      war_win_streak =  x$warWinStreak,
      war_wins = x$warWins,
      war_ties = x$warTies,
      war_losses = x$warLosses,
      is_war_log_public = x$isWarLogPublic
    )
}






