#' replace # with %23
#' @param x character
#' @examples
#' repash('#1234')
#' @importFrom stringr str_replace
#' @export
repash <- function(x) {
  str_replace(x, '#', '%23')
}
################################################
#' load a token file
#' @examples
#' token('./token1.txt')
#' @export
token <- function(file){
  scan(file, what = 'c')
}
################################################
#' load clan data
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
  return(clan)
}

################################################
#' load player data
#' @param tag member tag
#' @param token a token
#' @importFrom httr GET add_headers content
#' @examples
#' load_player( tag = "#PQYGGRVUR", token('~/dev/rcoc/tokens/token-34'))
#' @importFrom httr GET stop_for_status content add_headers
#' @export
load_player  <- function(tag, auth_key ){
  url <- paste( "https://api.clashofclans.com/v1/players",  repash(tag), sep= '/')
  player <- GET(url, add_headers(Authorization = paste("Bearer", auth_key, sep = " ")))
  player <- content(player, as = "parsed")
  class(player) <-  'player'
  return(player)
}


################################################
#' load current war data
#' @param tag clan tag
#' @param token a token
#' @importFrom httr GET add_headers content
#' @examples
#' load_current_war( tag = "#282UJRU28", token('~/dev/rcoc/tokens/token001'))
#' @importFrom httr GET stop_for_status content add_headers
#' @export
load_war  <- function(tag, auth_key ){
  url <- paste( "https://api.clashofclans.com/v1/clans",  repash(tag), 'currentwar', sep= '/')
  current_war <- GET(url, add_headers(Authorization = paste("Bearer", auth_key, sep = " ")))
  current_war <- content(current_war, as = "parsed")
  class(current_war) <-  'war'
  return(current_war)
}
################################################
#' info S3 generic method
#' @param An R object. Currently there are methods for clan objects
#' @export
info <- function(x, ...) {
  UseMethod('info', x)
}
###############################################
#' info S3 generic method
#' @param An R object. Currently there are methods for clan objects
#' @export
member <- function(x, ...) {
  UseMethod('member', x)
}
###############################################
#' tag S3 generic method
#' @param An R object. Currently there are methods for clan objects
#' @export
tag <- function(x, ...) {
  UseMethod('tag', x)
}
###############################################
#' export S3 generic method
#' @param An R object. Currently there are methods for clan objects
#' @export
export <- function(x, ...) {
  UseMethod('export', x)
}

###############################################
#' Returns info about the ith member of the clan
#' @param i integer: ith member of the clan
#' @param x: an object of class clan
#' @examples
#' member ( 4, x = my_clan)
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
#' Member method for objects of class clan.
#' Returns a tibble of info about all members of the clan
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
#' Info method for objects of class clan.
#' Returns a tibble of info about the clan
#' @param x: an object of class clan
#' @examples
#' info (my_clan)
#' @importFrom tibble tibble
#' @importFrom lubridate now
#' @export
info.clan <- function(x, d =  lubridate::now()){

  is_war_log_public <-  x$isWarLogPublic

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
      war_ties = ifelse(is.null(x$warTies), NA, x$warTies),
      war_losses = ifelse(is.null(x$warLosses), NA, x$warLosses) ,
      is_war_log_public = x$isWarLogPublic
    )
}
################################
#' tag method for objects of class clan.
#' Returns the tag a  clan
#' @param x: an object of class clan
#' @examples
#' tag (my_clan)
#' @export
tag.clan <- function(x){
  x$tag
}
################################
#' tag method for objects of class  war
#' Returns the tag of the opponent
#' @param x: an object of class current war
#' @examples
#' tag (current war)
#' @export
tag.current_war <- function(x){
  x$opponent$tag
}

##################################
#' export method for objects of class clan.
#' save a clan object in path. File name si ex-tag-today.rds
#' @param x: an object of class clan
#' @importFrom readr write_rds
#' @examples
#' export (my_clan, '~/')
#' @export
export.clan <- function(x, path) {
  file <- paste(paste('ex', tag(clan), format(now(), '%Y-%m-%d'), sep = '-'), 'rds', sep = '.')
  write_rds(x , file.path(path, file))
  invisible(paste(file, 'exported'))
}
#######################################################
#' War attack from one of clan or opponent
#' Inner Function it should  be used only within `get_attack()``
#' @param x: an object of class war
#' @param who character(1) either 'clan' or 'opponent'
#' @importFrom dplyr  select mutate arrange everything
#' @examples
#' current_war <- load_current_war(tag = "#282UJRU28", auth_key = token('~/dev/rcoc/tokens/token-34'))
#' .get_attack(this_war, who = 'clan')
#' .get_attack(this_war, who = 'opponent')
#' @export
.get_attack <- function(war, who ){

  attack <- lapply(war[[who]]$members, "[[", 'attacks')
  attack <- lapply(attack,  bind_rows)
  attack <- bind_rows(attack)
  attack$attacker_clan_tag <- war[[who]]$tag
  attack$attacker_clan_name <- war[[who]]$name

  attack <- attack %>%
    select(attacker_clan_tag, attacker_clan_name, everything()) %>%
    mutate(win = ifelse(stars > 0 , 1, 0),
           attack = 1)  %>%
    arrange(order)

  attack <- attack %>%
    mutate(cum_win = cumsum(win),
           cum_star =  cumsum(stars),
           cum_attack = cumsum(attack))

  # names(attack) <-  c("attacker_clan_tag",
  #                     "attacker_clan_name",
  #                     "attacker_tag",
  #                     "defender_tag" ,
  #                     "stars" ,
  #                     "destruction_percentage",
  #                     "order",
  #                     "win",
  #                     "cum_win")
  attack

}
#############################################
#' War attacks from both clan and opponent
#' Return a tibble of attacks
#' @param x: an object of class war
#' @importFrom dplyr  arrange bind_rows
#' @examples
#' current_war <- load_current_war(tag = "#282UJRU28", auth_key = token('~/dev/rcoc/tokens/token-34'))
#' .get_attack(current_war, who = 'clan')
#' .get_attack(current_war, who = 'opponent')
#' @export
get_attack <-  function(war) {
  attack <- try(lapply(c('clan', 'opponent'), .get_attack, war =  war))
  if ( class(attack) != 'try-error'){
    attack <-  bind_rows(attack)
    attack <- arrange(attack, order)
  } else {
    cat('no war yet!', '\n')
    attack <- NULL
  }
  attack
}
######################################################
#' Experience points  from both clan and opponent
#' Return plot
#' @param x: an object of class war
#' @param max_lag: Max number of lags when attacking
#' @importFrom dplyr left_join select
#' @importFrom ggplot2 ggplot geom_bar scale_fill_manual coord_flip facet_wrap theme_minimal ylab xlab
#' @examples
#' compare (war = this_war, max_lag = 3)
#' @export
compare <- function(war, max_lag = 3, auth_key) {


  this_war <-  war

  opponent_member <- this_war$opponent$tag %>%
    load_clan(auth_key = auth_key) %>%
    member()

  clan_member <- this_war$clan$tag %>%
    load_clan(auth_key = auth_key) %>%
    member()

  clan_warrior <- tibble(
    tag = vapply(this_war$clan$members, '[[', 'tag', FUN.VALUE = character(1)),
    map_position =   vapply(this_war$clan$members, '[[', 'mapPosition', FUN.VALUE = integer(1))) %>%
    arrange(map_position)

  opponent_warrior <- tibble(
    tag = vapply(this_war$opponent$members, '[[', 'tag', FUN.VALUE = character(1)),
    map_position =   vapply(this_war$opponent$members, '[[', 'mapPosition', FUN.VALUE = integer(1))) %>%
    arrange(map_position)

  clan_warrior <- clan_warrior %>%
    left_join(clan_member, by = 'tag') %>%
    select(tag,map_position,name,exp_level,league,trophies)

  names(clan_warrior) <- paste('clan', names(clan_warrior), sep = '_')

  opponent_warrior <- opponent_warrior %>%
    left_join(opponent_member, by = 'tag') %>%
    select(tag,map_position,name,exp_level,league,trophies)

  names(opponent_warrior) <- paste('opponent', names(opponent_warrior), sep = '_')

  clan_opponent_warrior <- clan_warrior %>%
    left_join( opponent_warrior,
               by = c('clan_map_position' = 'opponent_map_position'))

  tmp <- list()
  for ( i in seq_len(max_lag+1)-1){

    tmp[[i+1]] <-  clan_opponent_warrior %>%
      select(clan_name, clan_exp_level,opponent_exp_level, clan_map_position) %>%
      mutate(clan_name = lag(clan_name, i),
             clan_exp_level = lag(clan_exp_level,  i),
             delta_exp_level = clan_exp_level - opponent_exp_level,
             sign =  factor(ifelse(delta_exp_level > 0 , 1, -1)),
             lag = paste('Lag:',i)) %>%
      filter ( !is.na(clan_name))

  }
  tmp <- bind_rows(tmp)

  pl <- tmp %>%
    ggplot(aes(reorder(clan_name ,-clan_map_position), delta_exp_level, fill = sign)) +
    geom_bar( stat = 'identity') +
    scale_fill_manual(values=c("1" = "darkgreen","-1" = "darkred")) +
    coord_flip() +
    facet_wrap(~lag) +
    theme_minimal() +
    theme(legend.position = "none")  +
    ylab('Delta Experience Level') +
    xlab('Clan Member')


  pl

}







