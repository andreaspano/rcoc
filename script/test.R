require(httr)
require(dplyr)
require(ggplot2)
require(RColorBrewer)


ii <- load_clan(tag = "#282UJRU28", auth_key = token('~/dev/rcoc/tokens/token001'))


f <- function(tag, auth_key ){

  url <- paste( "https://api.clashofclans.com/v1/clans",  repash(tag), sep= '/')

  r <- GET(url, add_headers(Authorization = paste("Bearer", auth_key, sep = " ")))

  r <- content(r, as = "parsed") 

  return(r)
}

i <- load_clan( tag = "#282UJRU28", token('~/dev/rcoc/tokens/token001'))



#clantag <- "#282UJRU28"
clantag <- "%23282UJRU28"

token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6IjNlZWM5NTBhLTUzZTEtNGYxOS05NmU5LTc0NTRhMGJjNmZkNCIsImlhdCI6MTU1NTI1OTQ3MSwic3ViIjoiZGV2ZWxvcGVyLzkwOWIwNGFhLTAzZDItMTljMS1kNDA0LTgzNjBlNzg4MTQ0MSIsInNjb3BlcyI6WyJjbGFzaCJdLCJsaW1pdHMiOlt7InRpZXIiOiJkZXZlbG9wZXIvc2lsdmVyIiwidHlwZSI6InRocm90dGxpbmcifSx7ImNpZHJzIjpbIjkzLjcxLjguOTYiXSwidHlwZSI6ImNsaWVudCJ9XX0.pqLcpAfhLhzVH-gajztu7Q8RPXQqKZiguRyyLuLcEmXzT1C7kUW5Zfpzbc7U2uXLDSVEKcCjXQCuCS3hS8TuAg"


url <- paste( "https://api.clashofclans.com/v1/clans",  clantag, sep= '/')

#url <- paste(url, 'currentwar', sep = '/')
r <- GET(url, add_headers(Authorization = paste("Bearer", token, sep = " ")))
stop_for_status(r)
#content(r)
r<-content(r,as="parsed") 


d <- tibble(name = vapply(r$memberList, '[[', 'name', FUN.VALUE = character(1)), 
            trophies = vapply(r$memberList, '[[', 'trophies', FUN.VALUE = numeric(1)))


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



