library(stringi)

result_df<-data.frame(passer=character(0), receiver=character(0), rusher=character(0), completion=numeric(0), interception_vec=integer(0))

for (i in 1:length(pbp_cfb$play_text)) {
  string = pbp_cfb$play_text[i]  
  if (grepl("NO PLAY", string, fixed = TRUE)){
    passer=NA
    receiver= NA
    rusher=NA
    completion=NA
    interception_vec=NA
  }
  else{
    if (grepl(" pass complete to ", string, fixed = TRUE)){
      start.1=stri_locate_all(pattern = " pass complete to ", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " pass complete to ", string, fixed = TRUE)[[1]][2]
      
      start.2=stri_locate_all(pattern = " for ", string, fixed = TRUE)[[1]][1]
      
      passer=substr(string, 1, start.1-1)
      receiver= substr(string, end.1+1, start.2-1)
      rusher=NA
      completion=1
      interception_vec=0
    }
    
    else if (grepl("pass incomplete", string, fixed = TRUE)){
      start.1=stri_locate_all(pattern = " pass incomplete", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " pass incomplete", string, fixed = TRUE)[[1]][2]
      
      passer=substr(string, 1, start.1-1)
      receiver= substr(string, end.1+5, nchar(string))
      rusher=NA
      completion=0
      interception_vec=0
    }
    
    else if (grepl("run for", string, fixed = TRUE)){
      start.1=stri_locate_all(pattern = " run for ", string, fixed = TRUE)[[1]][1]
      end.1=stri_locate_all(pattern = " run for ", string, fixed = TRUE)[[1]][2]
      
      start.2=stri_locate_all(pattern = " yds to the ", string, fixed = TRUE)[[1]][1]
      
      passer=NA
      receiver=NA
      rusher=substr(string, 1, start.1-1)
      completion=NA
      interception_vec=NA
    }
    
    else if (grepl("pass intercepted", string, fixed = TRUE)){
      start.1=stri_locate_all(pattern = " pass intercepted", string, fixed = TRUE)[[1]][1]
      
      passer=substr(string, 1, start.1-1)
      receiver=NA
      rusher=NA
      completion=0
      interception_vec=1
    }
    
    else if (grepl("sacked by", string, fixed = TRUE)){
      start.1=stri_locate_all(pattern = " sacked by ", string, fixed = TRUE)[[1]][1]
      
      passer=substr(string, 1, start.1-1)
      receiver=NA
      rusher=NA
      completion=0
      interception_vec=0
    }
    else {
      passer=NA
      receiver=NA
      rusher=NA
      completion=NA
      interception_vec=NA
    }
  }
  result=data.frame(passer, receiver, rusher, completion, interception_vec)
  names(result)=c("passer", "receiver", "rusher", "completion", "interception_vec")
  result_df=rbind(result_df, result)
}

pbp_cfb2=cbind(pbp_cfb, result_df)