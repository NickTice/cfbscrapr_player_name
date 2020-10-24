library(stringi)

# test pbp
pbp_2019 <- data.frame()
for(i in 1:12){
  pbp <- cfb_pbp_data(year=2019, season_type = 'regular', week = i, epa_wpa = TRUE)
  pbp_2019 <- rbind(pbp_2019, pbp)
}

# yours. takes about 2.5 minutes
start.time <- Sys.time()
test=pbp_2019
test=add_player_cols(test)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# mine. takes about 6 mins to run
start.time <- Sys.time()
test2=pbp_2019
test2=cbind(test2, play_text%>%
                   map_df(player_name))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




# each of these indexes where ours disagree and return what each of ours say with the play text.

# a few plays here that are a little weird. A few of mine kept the comma. Two of yours were weird but i think it's the play text
idx=which(test$passer_player_name!=test2$passer)
yours=test[idx,]$passer_player_name
mine=test2[idx,]$passer
text=test[idx,]$play_text

diff=cbind(yours,mine, text)
diff

# Just a few off here too. 
idx=which(test$rusher_player_name!=test2$rusher)
yours=test[idx,]$rusher_player_name
mine=test2[idx,]$rusher
text=test[idx,]$play_text

diff=cbind(yours, mine, text)
diff

# some weird stuff going on here
idx=which(test$receiver_player_name!=test2$receiver)
yours=test[idx,]$receiver_player_name
mine=test2[idx,]$receiver
text=test[idx,]$play_text

diff=cbind(yours, mine, text)
diff

# mine is very wrong for every pick six but yours looks good for this
idx=which(test$interception_player_name!=test2$intercept_player)
yours=test[idx,]$interception_player_name
mine=test2[idx,]$intercept_player
text=test[idx,]$play_text

diff=cbind(yours, mine, text)
diff

# no issues
idx=which(test$punter_player_name!=test2$punter)
yours=test[idx,]$punter_player_name
mine=test2[idx,]$punter
text=test[idx,]$play_text

diff=cbind(yours, mine, text)
diff

# yours has "at" at the end of a few
idx=which(test$punt_returner_player_name!=test2$punt_returner)
yours=test[idx,]$punt_returner_player_name
mine=test2[idx,]$punt_returner
text=test[idx,]$play_text

diff=cbind(yours, mine, text)
diff

# no issues
idx=which(test$kickoff_player_name!=test2$kickoff_player)
yours=test[idx,]$kickoff_player_name
mine=test2[idx,]$kickoff_player
text=test[idx,]$play_text

diff=cbind(yours, mine, text)
diff

# no issues
idx=which(test$kickoff_returner_player_name!=test2$kickoff_returner)
yours=test[idx,]$kickoff_returner_player_name
mine=test2[idx,]$kickoff_returner
text=test[idx,]$play_text

diff=cbind(yours, mine, text)
diff

# yours only seems to do the last name
idx=which(test$fg_kicker_player_name!=test2$fg_kicker)
yours=test[idx,]$fg_kicker_player_name
mine=test2[idx,]$fg_kicker
text=test[idx,]$play_text

diff=cbind(yours, mine, text)
diff

