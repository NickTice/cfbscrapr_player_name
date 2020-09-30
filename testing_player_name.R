library(stringi)



pbp_cfb <- data.frame()
for(i in 1:4){
  pbp <- cfb_pbp_data(year=2020, season_type = 'regular', week = i, epa_wpa = TRUE)
  pbp_2020 <- rbind(pbp_2020, pbp)
}


compareNA <- function(v1,v2) {
  # This function returns TRUE wherever elements are the same, including NA's,
  # and false everywhere else.
  same <- (v1 == v2)  |  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}


pbp_S=pbp_cfb
pbp_N=pbp_cfb


start.time <- Sys.time()
pbp_S=add_player_cols(pbp_S)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


start.time <- Sys.time()
pbp_N=player_name(pbp_N)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# Testing different matches. Returns index that can be checked by setting equal to x

# your code sometimes doesn't record a passer on sacks
matches=which(!(compareNA(trimws(pbp_S$passer_player_name),pbp_N$passer)))
matches

x= 
pbp_S$play_text[x]
pbp_S$passer_player_name[x]
pbp_N$passer[x]

#############################################################################

# mine gets two wrong
matches=which(!(compareNA(trimws(pbp_S$rusher_player_name),pbp_N$rusher)))
matches

x= 
pbp_S$play_text[x]
pbp_S$rusher_player_name[x]
pbp_N$rusher[x]

#############################################################################


# Yours has some weird strings for interceptions 
pbp_S$receiver_player_name[which(pbp_S$receiver_player_name=="")]=NA
matches=which(!(compareNA(trimws(pbp_S$receiver_player_name),pbp_N$receiver)))
matches

x= 
pbp_S$play_text[x]
pbp_S$receiver_player_name[x]
pbp_N$receiver[x]

#############################################################################


# your interception player seems off
matches=which(!(compareNA(trimws(pbp_S$interception_player_name), pbp_N$intercept_player)))
matches

x= 7326
pbp_S$play_text[x]
pbp_S$interception_player_name[x]
pbp_N$intercept_player[x]

#############################################################################

# both of ours miss a few
matches=which(!(compareNA(trimws(pbp_S$punter_player_name), pbp_N$punter)))
matches

x= 
pbp_S$play_text[x]
pbp_S$punter_player_name[x]
pbp_N$punter[x]

############################################################################

# your punt returner seems really off
matches=which(!(compareNA(trimws(pbp_S$punt_returner_player_name), pbp_N$punt_returner)))
matches

x= 
pbp_S$play_text[x]
pbp_S$punt_returner_player_name[x]
pbp_N$punt_returner[x]

############################################################################

# mine misses one
matches=which(!(compareNA(trimws(pbp_S$kickoff_player_name), pbp_N$kickoff_player)))
matches

x= 
pbp_S$play_text[x]
pbp_S$kickoff_player_name[x]
pbp_N$kickoff_player[x]

############################################################################

# mine misses some onside kicks
matches=which(!(compareNA(trimws(pbp_S$kickoff_returner_player_name), pbp_N$kickoff_returner)))
matches

x= 
pbp_S$play_text[x]
pbp_S$kickoff_returner_player_name[x]
pbp_N$kickoff_returner[x]

############################################################################

# another one that seems off
matches=which(!(compareNA(trimws(pbp_S$fg_kicker_player_name), pbp_N$fg_kicker)))
matches

x= 
pbp_S$play_text[x]
pbp_S$fg_kicker_player_name[x]
pbp_N$fg_kicker[x]

