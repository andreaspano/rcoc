rm(list =  ls())
require(rcoc)
# require(httr)
# require(dplyr)
# require(ggplot2)
# require(RColorBrewer)

#' load_clan( tag = "#282UJRU28", token('~/dev/rcoc/tokens/token001'))

#ii <- load_clan(tag = "#282UJRU28", auth_key = token('~/dev/rcoc/tokens/token001'))

clan <- readRDS('./clan.Rds')
class(clan) <-  'clan'
this_clan <-  info(clan)
this_member <-  member(clan)





#####################################

x <- this_member$trophies
n <- length(x)
score <-  seq_len(n)/n
plot( x, rev(score), type = 'l')
abline(v = median(x))
abline(h = .5)

#####################################

names(clan)

#"badgeUrls"
map_clan_info <-  list (

  elem = c(
  "tag","name","type","location","clanLevel",
  "clanPoints","clanVersusPoints","requiredTrophies","warFrequency",
  "warWinStreak","warWins","warTies","warLosses","isWarLogPublic"
  ),

  lang = list(
    eng = c(
      "tag","name","type","description","location","clan level",
      "clan points","clan versus points","required trophies","war frequency",
      "war win streak","war wins","war ties","war losses","is war log public"
    )))

lang <- function(map, .lang) {
  map$lang[[.lang]]
}

elem <- function(map) map$elem

map <- map_clan_info

fun_clan_info <- function(map) {

  tibble(lang(clan_info, 'eng'),
         unlist(clan[elem(clan_info)])
  )

}

fun_clan_info()




clan[clan_info]

d <- tibble(name = vapply(clan$memberList, '[[', 'name', FUN.VALUE = character(1)),
            trophies = vapply(clan$memberList, '[[', 'trophies', FUN.VALUE = numeric(1)))


png('~/tmp/smile.png')
ggplot(d) + geom_bar (aes(reorder(name, trophies) , trophies), fill = 'lightblue', stat = 'identity') +
    coord_flip() +
    theme(legend.position = "none") +
    xlab ('Number of trophies') +
    ylab('Name') +
    scale_fill_manual(values = heat.colors(nrow(d), alpha=1))
dev.off()


# https://www.whatismyip.com/ to know your IP address
#home_ip_address <- "93.71.8.96"


#############################################################################################

url <- paste( "https://api.clashofclans.com/v1/clans",  clantag, sep= '/')
url <- paste(url, 'currentwar', sep = '/')
r <- GET(url, add_headers(Authorization = paste("Bearer", token, sep = " ")))
stop_for_status(r)
r<-content(r,as="parsed")

team_size <- r$teamSize


.number_of_attack <- function(i, r) {
    r$clan$members[[i]]$attacks
}


number_of_attack <- function(r) {


    team_size <- r$teamSize
    lapply(seq_len(team_size) , .number_of_attack, r = r)

}


number_of_attack(r)

d <- tibble(name = vapply(r$memberList, '[[', 'name', FUN.VALUE = character(1)),
            trophies = vapply(r$memberList, '[[', 'trophies', FUN.VALUE = numeric(1)))



