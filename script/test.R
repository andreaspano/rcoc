rm(list =  ls())
require(rcoc)
require(httr)
require(dplyr)
require(lubridate)
require(readr)
require(ggplot2)


# Load  my clan
my_clan <- load_clan(tag = "#282UJRU28", auth_key = token('~/dev/rcoc/tokens/token-34'))
#  info about my clan
my_clan_info <-  info(clan)
# table of my clan members
my_clan_member <-  member(my_clan)
my_clan_member %>% select(name,  tag)


# Load my friend clan
friend_clan <- load_clan(tag = "#QYCUJPVL", auth_key = token('~/dev/rcoc/tokens/token-34'))
# info about my fiend clan
friend_clan_info <-  info(friend_clan)
# table of my clan members
friend_clan_member <-  member(friend_clan)



# Load current war
this_war <- load_war(tag = "#282UJRU28", auth_key = token('~/dev/rcoc/tokens/token-34'))
#write_rds(this_war, './export/war_20190420.rds')



# see attacks during war
war_attack <- get_attack(war = this_war)

ggplot(war_attack) +
  geom_line(aes(order, cum_win, group =  attacker_clan_name, color = attacker_clan_name)) +
  geom_line(aes(order, cum_star, group =  attacker_clan_name, color = attacker_clan_name)) +
  geom_line(aes(order, cum_attack, group =  attacker_clan_name,color = attacker_clan_name)) +
  geom_ribbon(aes(x =  order, ymin = cum_win, ymax = cum_star, group =  attacker_clan_name, fill = attacker_clan_name), alpha =  0.5) +
  xlab ('Attack Order') +ylab('Results') +
  theme_minimal() +
  theme(legend.title=element_blank())




# gather info  about opponent clan
opponent <- this_war$opponent$tag %>% load_clan(auth_key = token('~/dev/rcoc/tokens/token-34'))
opponent_clan <-  info(opponent)
opponent_member <-  member(opponent)

# compare exp points between clan and opponent
compare(war = this_war , max_lag = 3)


# checjk single player
#my_clan_member %>% select (name, tag) #PQYGGRVUR

this_player <- load_player( tag = "#PQYGGRVUR", token('~/dev/rcoc/tokens/token-34'))
names(this_player)
bind_rows(this_player$troops)
bind_rows(this_player$spells)
bind_rows(this_player$heroes)

alex <- load_player( tag = "#QPPVPU8Q", token('~/dev/rcoc/tokens/token-34'))

#QPPVPU8Q
bind_rows(alex$troops)

str(this_player)



