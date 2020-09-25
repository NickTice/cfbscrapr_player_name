library(stringi)


player_name=function(pbp){
  result_df=data.frame(passer=character(),
                              receiver=character(),
                              rusher=character(),
                              intercept_player=character(),
                              sack_player=character(),
                              sack_player1=character(),
                              sack_player2=character(),
                              punter=character(),
                              punt_returner=character(),
                              kickoff_player=character(),
                              kickoff_returner=character(),
                              fg_kicker=character(),
                              completion=numeric(),
                              interception_vec=numeric(),
                              punt_yds=numeric(),
                              return_yds=numeric(),
                              touchback=numeric(),
                              fg_distance=numeric(),
                              fg_made=numeric(),
                              fg_block=numeric())
                              
  for (i in 1:length(pbp$play_text)) {
  
  string = pbp$play_text[i]  
  
  if (grepl("NO PLAY", string, fixed = TRUE)){
    passer=NA
    receiver= NA
    rusher=NA
    intercept_player=NA
    sack_player=NA
    sack_player1=NA
    sack_player2=NA
    punter=NA
    punt_returner=NA
    kickoff_player=NA
    kickoff_returner=NA
    fg_kicker=NA
    completion=NA
    interception_vec=NA
    punt_yds=NA
    return_yds=NA
    touchback=NA
    fg_distance=NA
    fg_made=NA
    fg_block=NA
  }
  
  else{
    if (grepl(" pass complete to ", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " pass complete to ", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " pass complete to ", string, fixed = TRUE)[[1]][2]
      
      start.2=stri_locate_all(pattern = " for ", string, fixed = TRUE)[[1]][1]
      
      passer=substr(string, 1, start.1-1)
      receiver= substr(string, end.1+1, start.2-1)
      rusher=NA
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=NA
      completion=1
      interception_vec=0
      punt_yds=NA
      return_yds=NA
      touchback=NA
      fg_distance=NA
      fg_made=NA
      fg_block=NA
    }
    
    else if (grepl(" Yd pass from ", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " Yd pass from ", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " Yd pass from ", string, fixed = TRUE)[[1]][2]
      
      start.2=stri_locate_all(pattern = " (", string, fixed = TRUE)[[1]][1]
      
      passer=substr(string, end.1+1, start.2-1)
      receiver= trimws(substr(string, 1, start.1-3))
      rusher=NA
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=NA
      completion=1
      interception_vec=0
      punt_yds=NA
      return_yds=NA
      touchback=NA
      fg_distance=NA
      fg_made=NA
      fg_block=NA
    }
    else if (grepl("pass incomplete", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " pass incomplete", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " pass incomplete", string, fixed = TRUE)[[1]][2]
      
      if (grepl(", broken up by", string, fixed = TRUE)) {
        start.2=stri_locate_all(pattern = ", broken up by", string, fixed = TRUE)[[1]][1]-1
      } else if ( grepl("Penalty, ", string, fixed = TRUE)) {
        start.2=stri_locate_all(pattern = "for a 1st down", string, fixed = TRUE)[[1]][1]-1
      } else {
        start.2=nchar(string)
      }

      passer=substr(string, 1, start.1-1)
      receiver= ifelse(substr(string, end.1+5, start.2)=="", NA, substr(string, end.1+5, start.2))
      rusher=NA
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=NA
      completion=0
      interception_vec=0
      punt_yds=NA
      return_yds=NA
      touchback=NA
      fg_distance=NA
      fg_made=NA
      fg_block=NA
    }
    
    else if (grepl("run for", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " run for ", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " run for ", string, fixed = TRUE)[[1]][2]
      
      start.2=stri_locate_all(pattern = " yds to the ", string, fixed = TRUE)[[1]][1]
      
      passer=NA
      receiver=NA
      rusher=substr(string, 1, start.1-1)
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=NA
      completion=NA
      interception_vec=NA
      punt_yds=NA
      return_yds=NA
      touchback=NA
      fg_distance=NA
      fg_made=NA
      fg_block=NA
    }
    
    else if (grepl(" Yd Run ", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " Yd Run ", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " Yd Run ", string, fixed = TRUE)[[1]][2]
      
      start.2=stri_locate_all(pattern = " Yd Run ", string, fixed = TRUE)[[1]][1]
      
      passer=NA
      receiver=NA
      rusher=trimws(substr(string, 1, start.1-3))
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=NA
      completion=NA
      interception_vec=NA
      punt_yds=NA
      return_yds=NA
      touchback=NA
      fg_distance=NA
      fg_made=NA
      fg_block=NA
    }
    
    else if (grepl("pass intercepted", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " pass intercepted", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " pass intercepted ", string, fixed = TRUE)[[1]][2]
      
      start.2=stri_locate_all(pattern = " return for ", string, fixed = TRUE)[[1]][1]
      
      
      passer=substr(string, 1, start.1-1)
      receiver=NA
      rusher=NA
      intercept_player= substr(string, end.1+1, start.2-1)
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=NA
      completion=0
      interception_vec=1
      punt_yds=NA
      return_yds=NA
      touchback=NA
      fg_distance=NA
      fg_made=NA
      fg_block=NA
    }
    
    else if (grepl("sacked by", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " sacked by ", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " sacked by ", string, fixed = TRUE)[[1]][2]
      
      start.2=stri_locate_all(pattern = " for ", string, fixed = TRUE)[[1]][1]
      
      passer=substr(string, 1, start.1-1)
      receiver=NA
      rusher=NA
      intercept_player=NA
      sack_player=substr(string, end.1+1, start.2-1)
      if(grepl(" and ", sack_player, fixed = TRUE)){
        sack_player1=strsplit(sack_player, " and ")[[1]][1]
        sack_player2=strsplit(sack_player, " and ")[[1]][2]
        sack_player=NA
      } else{
      }
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=NA
      completion=NA
      interception_vec=NA
      punt_yds=NA
      return_yds=NA
      touchback=NA
      fg_distance=NA
      fg_made=NA
      fg_block=NA
      
    }
    
    else if (grepl("punt", string, fixed = TRUE)){
    
      if (grepl("touchback", string, fixed = TRUE)){

        start.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][1]
        end.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][2]

        start.2=stri_locate_all(pattern = " yds for a touchback", string, fixed = TRUE)[[1]][1]

        passer=NA
        receiver=NA
        rusher=NA
        intercept_player=NA
        sack_player=NA
        sack_player1=NA
        sack_player2=NA
        punter=substr(string, 1, start.1-1)
        punt_returner=NA
        kickoff_player=NA
        kickoff_returner=NA
        fg_kicker=NA
        completion=NA
        interception_vec=NA
        punt_yds=as.numeric(substr(string, end.1+1, start.2-1))
        return_yds=NA
        touchback=1
        fg_distance=NA
        fg_made=NA
        fg_block=NA
      }
      else if (grepl("downed", string, fixed = TRUE)){
          
          start.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][1]
          end.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][2]

          start.2=stri_locate_all(pattern = " yds, downed at the", string, fixed = TRUE)[[1]][1]

          passer=NA
          receiver=NA
          rusher=NA
          intercept_player=NA
          sack_player=NA
          sack_player1=NA
          sack_player2=NA
          punter=substr(string, 1, start.1-1)
          punt_returner=NA
          kickoff_player=NA
          kickoff_returner=NA
          fg_kicker=NA
          completion=NA
          interception_vec=NA
          punt_yds=as.numeric(substr(string, end.1+1, start.2-1))
          return_yds=NA
          touchback=1
          fg_distance=NA
          fg_made=NA
          fg_block=NA}

      else if (grepl("fair catch", string, fixed = TRUE)){
        
        start.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][1]
        end.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][2]

        start.2=stri_locate_all(pattern = " yds, fair catch by ", string, fixed = TRUE)[[1]][1]
        end.2=stri_locate_all(pattern = " yds, fair catch by ", string, fixed = TRUE)[[1]][2]

        start.3=stri_locate_all(pattern = " at the ", string, fixed = TRUE)[[1]][1]


        passer=NA
        receiver=NA
        rusher=NA
        intercept_player=NA
        sack_player=NA
        sack_player1=NA
        sack_player2=NA
        punter=substr(string, 1, start.1-1)
        punt_returner=substr(string, end.2+1, start.3-1)
        kickoff_player=NA
        kickoff_returner=NA
        fg_kicker=NA
        completion=NA
        interception_vec=NA
        punt_yds=as.numeric(substr(string, end.1+1, start.2-1))
        return_yds=NA 
        touchback=0
        fg_distance=NA
        fg_made=NA
        fg_block=NA
      }

      else if (grepl(" returns ", string, fixed = TRUE)){

        start.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][1]
        end.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][2]

        start.2=stri_locate_all(pattern = " yds , ", string, fixed = TRUE)[[1]][1]
        end.2=stri_locate_all(pattern = " yds , ", string, fixed = TRUE)[[1]][2]

        start.3=stri_locate_all(pattern = " returns for ", string, fixed = TRUE)[[1]][1]
        end.3=stri_locate_all(pattern = " returns for ", string, fixed = TRUE)[[1]][2]

        start.4=stri_locate_all(pattern = " yds to the ", string, fixed = TRUE)[[1]][1]

        passer=NA
        receiver=NA
        rusher=NA
        intercept_player=NA
        sack_player=NA
        sack_player1=NA
        sack_player2=NA
        punter=substr(string, 1, start.1-1)
        punt_returner=substr(string, end.2+1, start.3-1)
        kickoff_player=NA
        kickoff_returner=NA
        fg_kicker=NA
        completion=NA
        interception_vec=NA
        punt_yds=as.numeric(substr(string, end.1+1, start.2-1))
        return_yds=as.numeric(trimws(substr(string, end.3, start.4-1)))
        touchback=0
        fg_distance=NA
        fg_made=NA
        fg_block=NA
      }  

      else if (grepl(" out-of-bounds ", string, fixed = TRUE)){
        
        start.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][1]
        end.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][2]

        start.2=stri_locate_all(pattern = " yds, punt out-of-bounds ", string, fixed = TRUE)[[1]][1]

        passer=NA
        receiver=NA
        rusher=NA
        intercept_player=NA
        sack_player=NA
        sack_player1=NA
        sack_player2=NA
        punter=substr(string, 1, start.1-1)
        punt_returner=NA
        kickoff_player=NA
        kickoff_returner=NA
        fg_kicker=NA
        completion=NA
        interception_vec=NA
        punt_yds=as.numeric(substr(string, end.1+1, start.2-1))
        return_yds=NA
        touchback=0
        fg_distance=NA
        fg_made=NA
        fg_block=NA
      }
        else {
          
          start.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][1]
          end.1=stri_locate_all(pattern = " punt for ", string, fixed = TRUE)[[1]][2]

          start.2=stri_locate_all(pattern = " yds", string, fixed = TRUE)[[1]][1]

          passer=NA
          receiver=NA
          rusher=NA
          intercept_player=NA
          sack_player=NA
          sack_player1=NA
          sack_player2=NA
          punter=substr(string, 1, start.1-1)
          punt_returner=NA
          kickoff_player=NA
          kickoff_returner=NA
          fg_kicker=NA
          completion=NA
          interception_vec=NA
          punt_yds=as.numeric(substr(string, end.1+1, start.2-1))
          return_yds=NA
          touchback=1
          fg_distance=NA
          fg_made=NA
          fg_block=NA
        }}
    
    else if (grepl("kickoff", string, fixed = TRUE)){
      
      if (grepl("return", string, fixed = TRUE)){
        
        start.1=stri_locate_all(pattern = " kickoff for ", string, fixed = TRUE)[[1]][1]
        end.1=stri_locate_all(pattern = " kickoff for ", string, fixed = TRUE)[[1]][2]
        
        start.2=stri_locate_all(pattern = " yds , ", string, fixed = TRUE)[[1]][1]
        end.2=stri_locate_all(pattern = " yds , ", string, fixed = TRUE)[[1]][2]
        
        start.3=stri_locate_all(pattern = " return for ", string, fixed = TRUE)[[1]][1]
        end.3=stri_locate_all(pattern = " return for ", string, fixed = TRUE)[[1]][2]
        
        start.4=stri_locate_all(pattern = " yds to the ", string, fixed = TRUE)[[1]][1]
        
        passer=NA
        receiver=NA
        rusher=NA
        intercept_player=NA
        sack_player=NA
        sack_player1=NA
        sack_player2=NA
        punter=NA
        punt_returner=NA
        kickoff_player=substr(string, 1, start.1-1)
        kickoff_returner=substr(string, end.2+1, start.3-1)
        fg_kicker=NA
        completion=NA
        interception_vec=NA
        punt_yds=NA
        return_yds=as.numeric(trimws(substr(string, end.3, start.4-1)))
        touchback=0
        fg_distance=NA
        fg_made=NA
        fg_block=NA
      }
      
      else if(grepl("touchback", string, fixed = TRUE)){
        
        start.1=stri_locate_all(pattern = " kickoff for ", string, fixed = TRUE)[[1]][1]
        
        passer=NA
        receiver=NA
        rusher=NA
        intercept_player=NA
        sack_player=NA
        sack_player1=NA
        sack_player2=NA
        punter=NA
        punt_returner=NA
        kickoff_player=substr(string, 1, start.1-1)
        kickoff_returner=NA
        fg_kicker=NA
        completion=NA
        interception_vec=NA
        punt_yds=NA
        return_yds=NA
        touchback=1
        fg_distance=NA
        fg_made=NA
        fg_block=NA
      }
      else {
        
        start.1=stri_locate_all(pattern = " kickoff for ", string, fixed = TRUE)[[1]][1]
        
        passer=NA
        receiver=NA
        rusher=NA
        intercept_player=NA
        sack_player=NA
        sack_player1=NA
        sack_player2=NA
        punter=NA
        punt_returner=NA
        kickoff_player=substr(string, 1, start.1-1)
        kickoff_returner=NA
        fg_kicker=NA
        completion=NA
        interception_vec=NA
        punt_yds=NA
        return_yds=NA
        touchback=1
        fg_distance=NA
        fg_made=NA
        fg_block=NA
      }}
    
    
  else if (grepl("FG", string, fixed = TRUE)){
    
    if (grepl("GOOD", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " yd FG ", string, fixed = TRUE)[[1]][1]
      
      passer=NA
      receiver=NA
      rusher=NA
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_player
      kickoff_returner=NA
      kickoff_returner
      fg_kicker=substr(string, 1, start.1-4)
      fg_kicker
      completion=NA
      interception_vec=NA
      punt_yds=NA
      return_yds=NA
      return_yds
      touchback=1
      fg_distance=as.numeric(substr(string, start.1-2, start.1-1))
      fg_made=1
      fg_block=0
    }
    
    if (grepl("MISSED", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " yd FG ", string, fixed = TRUE)[[1]][1]
      
      passer=NA
      receiver=NA
      rusher=NA
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_player
      kickoff_returner=NA
      kickoff_returner
      fg_kicker=substr(string, 1, start.1-4)
      fg_kicker
      completion=NA
      interception_vec=NA
      punt_yds=NA
      return_yds=NA
      return_yds
      touchback=1
      fg_distance=as.numeric(substr(string, start.1-2, start.1-1))
      fg_made=0
      fg_block=0
    }
    
    if (grepl("BLOCKED", string, fixed = TRUE)){
      
      start.1=stri_locate_all(pattern = " yd FG ", string, fixed = TRUE)[[1]][1]
      
      passer=NA
      receiver=NA
      rusher=NA
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=substr(string, 1, start.1-4)
      completion=NA
      interception_vec=NA
      punt_yds=NA
      punt_blocked=NA
      return_yds=NA
      touchback=1
      fg_distance=as.numeric(substr(string, start.1-2, start.1-1))
      fg_made=0
      fg_block=1
    }
    
    else {
      
      start.1=stri_locate_all(pattern = " yd FG ", string, fixed = TRUE)[[1]][1]
      
      passer=NA
      receiver=NA
      rusher=NA
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_player
      kickoff_returner=NA
      kickoff_returner
      fg_kicker=substr(string, 1, start.1-4)
      fg_kicker
      completion=NA
      interception_vec=NA
      punt_yds=NA
      return_yds=NA
      return_yds
      touchback=1
      fg_distance=as.numeric(substr(string, start.1-2, start.1-1))
      fg_made=NA
      fg_block=0
    }}
    
    else {
      passer=NA
      receiver=NA
      rusher=NA
      intercept_player=NA
      sack_player=NA
      sack_player1=NA
      sack_player2=NA
      punter=NA
      punt_returner=NA
      kickoff_player=NA
      kickoff_returner=NA
      fg_kicker=NA
      completion=NA
      interception_vec=NA
      punt_yds=NA
      punt_blocked=NA
      return_yds=NA
      touchback=NA
      fg_distance=NA
      fg_made=NA
      fg_block=NA
    }}
    
  result=data.frame(passer,
                    receiver,
                    rusher,
                    intercept_player,
                    sack_player,
                    sack_player1,
                    sack_player2,
                    punter,
                    punt_returner,
                    kickoff_player,
                    kickoff_returner,
                    fg_kicker,
                    completion,
                    interception_vec,
                    punt_yds,
                    return_yds,
                    touchback,
                    fg_distance,
                    fg_made,
                    fg_block)
  
  names(result)=c("passer",
                  "receiver",
                  "rusher",
                  "intercept_player",
                  "sack_player",
                  "sack_player1",
                  "sack_player2",
                  "punter",
                  "punt_returner",
                  "kickoff_player",
                  "kickoff_returner",
                  "fg_kicker",
                  "completion",
                  "interception_vec",
                  "punt_yds",
                  "return_yds",
                  "touchback",
                  "fg_distance",
                  "fg_made",
                  "fg_block")
  result_df=rbind(result_df, result)
  }
  
  pbp=cbind(pbp, result_df)
  return(pbp)
}




