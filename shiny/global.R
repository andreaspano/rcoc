require(rcoc)


require(shiny)
require(ggplot2)
require(dplyr)

options(shiny.sanitize.errors = FALSE)
auth_key <- 'eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzUxMiIsImtpZCI6IjI4YTMxOGY3LTAwMDAtYTFlYi03ZmExLTJjNzQzM2M2Y2NhNSJ9.eyJpc3MiOiJzdXBlcmNlbGwiLCJhdWQiOiJzdXBlcmNlbGw6Z2FtZWFwaSIsImp0aSI6IjBjMDk1YTVlLTZmYTctNGQ5Ny1hMDQwLWFmNzYyYjc4ZDc5ZCIsImlhdCI6MTU1NzE3NjMyNiwic3ViIjoiZGV2ZWxvcGVyLzkwOWIwNGFhLTAzZDItMTljMS1kNDA0LTgzNjBlNzg4MTQ0MSIsInNjb3BlcyI6WyJjbGFzaCJdLCJsaW1pdHMiOlt7InRpZXIiOiJkZXZlbG9wZXIvc2lsdmVyIiwidHlwZSI6InRocm90dGxpbmcifSx7ImNpZHJzIjpbIjM0LjI0NC4yMTQuNjMiXSwidHlwZSI6ImNsaWVudCJ9XX0.cY15VfMWDr6XbXM6Cvo9CZOZx2iA2nyjtrP5qckcf_GBYAJiKHwDYPQPM9BRXcdJegxOVcrmDb08sXTsZyiX5g'
tag <- '#282UJRU28'

# League rank and colors 
league_rank <- c('Unranked','Bronze League III','Bronze League II','Bronze League I','Silver League III','Silver League II',
                 'Silver League I','Gold League III','Gold League II','Gold League I','Crystal League I',
                 'Crystal League II','Crystal League III','Master League I','Master League II','Master League III',
                 'Champion League I','Champion League II','Champion League III','Titan League I','Titan League II','Titan League III','Legend')
league_color <- c('#e4e3da', rep('#ad6416', 3) , rep('#c0c0c0', 3) , rep('#b58500', 3) , rep('#5a005a', 3) , rep('#5c5755', 3) , rep('#6c3b2a', 3), rep('#ffce00', 3), '#5a005a') 
names(league_color) <- league_rank


my_clan <- load_clan(tag = tag, auth_key = auth_key)

my_clan_info <-  info(my_clan)
my_clan_info <- tibble(names(my_clan_info), t(my_clan_info)[,1])
names(my_clan_info) <- c('tag', 'value')
tbl_clan_info <- my_clan_info %>% 
  mutate(tag = gsub('_', ' ', tag)) 




my_clan_member <-  member(my_clan) %>% 
  select(name, role, league, trophies, exp_level) %>% 
  arrange(rev(trophies))


# logo
clan_logo_url <- my_clan$badgeUrls$medium


pl_member <- ggplot (my_clan_member) + aes(reorder(name,trophies), trophies, group = league, fill = league) +
  geom_bar(stat = 'identity') +
  xlab('Member') + ylab ('Trophies') +
  coord_flip() +
  scale_fill_manual("League", values = league_color) +
  theme_minimal()


tbl_donation <-  member(my_clan) %>% 
  select(name, donations, donations_received) %>% 
  arrange(rev(donations))

pl_donation <- ggplot(tbl_donation) + 
  geom_bar(aes(reorder(name, donations) ,donations), fill = 'darkgreen', stat = 'identity') + 
  geom_bar(aes(name, -donations_received), fill = 'darkred', stat = 'identity') + 
  ylim(c(-1, 1) *max(c(tbl_donation$donations , tbl_donation$donations_received))) +
  ylab('Made and Received Donations')+xlab('Members') +
  coord_flip() +
  theme_minimal()



# Load current war
#this_war <- load_war(tag = tag, auth_key = auth_key)
# 
# # gather info  about opponent clan
#opponent <- this_war$opponent$tag %>% load_clan(auth_key = auth_key)
#opponent_clan <-  info(opponent)
#opponent_member <-  member(opponent)



#pl_opponent <- ggplot (opponent_member) + aes(reorder(name,trophies), trophies, group = league, fill = league) +
#  geom_bar(stat = 'identity') +
#  xlab('Opponents') + ylab ('Trophies') +
#  coord_flip() +
#  scale_fill_manual("League", values = league_color) +
#  theme_minimal()



# 
# # compare exp points between clan and opponent
#pl_compare <-  compare(war = this_war , max_lag = 3, auth_key = auth_key)


# see attacks during war
#war_attack <- get_attack(war = this_war)

# ggplot(war_attack) +
#   geom_line(aes(order, cum_win, group =  attacker_clan_name, color = attacker_clan_name)) +
#   geom_line(aes(order, cum_star, group =  attacker_clan_name, color = attacker_clan_name)) +
#   geom_line(aes(order, cum_attack, group =  attacker_clan_name,color = attacker_clan_name)) +
#   geom_ribbon(aes(x =  order, ymin = cum_win, ymax = cum_star, group =  attacker_clan_name, fill = attacker_clan_name), alpha =  0.5) +
#   xlab ('Attack Order') +ylab('Results') +
#   theme_minimal() +
#   theme(legend.title=element_blank())




# Run the application 
#shinyApp(ui = ui, server = server)
