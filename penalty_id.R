library(stringi)

# testing with some pbp
test=pbp_2019
start.time <- Sys.time()
test=penalty_id(test)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken




# play text of plays with NA for penalty yardage
test$play_text[test$penalty==1][is.na(test$penalty_yds[test$penalty==1])]

# same thing returning yards gained for NA penalty_yds
# if yards_gained was equal to 0, 5, 10, or 15, I automatically set the penalty yardage to that number
test$yards_gained[test$penalty==1][is.na(test$penalty_yds[test$penalty==1])]


# play text of "Missing" plays
test$play_text[which(test$penalty_name=="Missing", TRUE)]


penalty_id=function(pbp){
  
  result_df=data.frame(penalty=numeric(),
                       penalty_name=character(),
                       penalty_yds=numeric()
  )
  
  for (i in 1:length(pbp$play_text)) {
    
    string = pbp$play_text[i]  
    
    if (grepl("PENALTY", string, fixed = TRUE) | grepl("Penalty", string, fixed = TRUE)){
      
      if (grepl(" off-setting ", string, fixed = TRUE)){
        penalty=1
        penalty_name="Off-Setting"
        penalty_yds=0
        }
      
      else if (grepl("declined", string, fixed = TRUE)){
        penalty=1
        penalty_name="Declined Penalty"
        penalty_yds=0
      }
        
      else {
        if (grepl(" roughing passer ", string, fixed = TRUE) | grepl(" Roughing Passer ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Roughing the Passer"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" offensive holding ", string, fixed = TRUE) | grepl(" Offensive Holding ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Offensive Holding"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        } 
        
        else if (grepl(" pass interference", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Pass Interference"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" encroachment", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Encroachment"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        
        
        else if (grepl(" Defensive Pass Interference ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Defensive Pass Interference"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Offensive Pass Interference ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Offensive Pass Interference"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        
        else if (grepl(" illegal procedure ", string, fixed = TRUE) | grepl(" Illegal Procedure ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Procedure"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" holding ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Holding"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" offside ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Offside"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Offensive Offside ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Offensive Offside"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" illegal fair catch signal ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Fair Catch Signal"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Illegal Batting ", string, fixed = TRUE) | grepl(" illegal batting ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Batting"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Neutral Zone Infraction ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Neutral Zone Infraction"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" ineligible downfield ", string, fixed = TRUE) | grepl(" Ineligible Downfield ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Ineligible Downfield"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" illegal use of hands ", string, fixed = TRUE) | grepl(" Illegal Use of Hands ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Use of Hands"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Kickoff Out of Bounds ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Kickoff Out of Bounds"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" 12 Men on the Field ", string, fixed = TRUE) | grepl(" 12 men on the field ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="12 Men on the Field"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" illegal block ", string, fixed = TRUE) | grepl(" Illegal Block ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Block"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" personal foul ", string, fixed = TRUE) | grepl(" Personal Foul ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Personal Foul"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" false start ", string, fixed = TRUE)|grepl(" False Start ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="False Start"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" substitution infraction ", string, fixed = TRUE)| grepl(" Substitution Infraction ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Substitution Infraction"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" illegal formation ", string, fixed = TRUE) | grepl(" Illegal Formation ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Formation"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" illegal touching ", string, fixed = TRUE) | grepl(" Illegal Touching ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Touching"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
      
        else if (grepl(" clipping ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Clipping"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
      
        else if (grepl(" Sideline Infraction ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Sideline Infraction"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Sideline Interference ", string, fixed = TRUE) | grepl(" sideline interference ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Sideline Interference"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        
        else if (grepl(" crackback ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Crackback"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Illegal Snap ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Snap"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" illegal helmet contact ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Helmet Contact"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        
        else if (grepl(" roughing holder ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Roughing Holder"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        

        
        else if (grepl(" Horse Collar Tackle ", string, fixed = TRUE) | grepl(" horse collar tackle ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Horse Collar Tackle"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
          
        else if (grepl(" illegal participation ", string, fixed = TRUE) | grepl(" Illegal Participation ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Participation"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        
        
        else if (grepl(" tripping ", string, fixed = TRUE) | grepl(" Tripping ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Tripping"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Illegal Shift ", string, fixed = TRUE) | grepl(" illegal shift ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Shift"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" illegal motion ", string, fixed = TRUE)|grepl(" Illegal Motion ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Motion"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        
        else if (grepl(" roughing the kicker ", string, fixed = TRUE) | grepl(" Roughing the Kicker ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Roughing the Kicker"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Defensive Offside ", string, fixed = TRUE) | grepl(" Offside Defense ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Defensive Offside"
          penalty_yds=penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Defensive Holding ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Defensive Holding"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Delay of Game ", string, fixed = TRUE) | grepl(" delay of game ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Delay of Game"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
    
        else if (grepl(" Targeting ", string, fixed = TRUE) | grepl(" targeting ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Targeting"
          penalty_yds=penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Face Mask ", string, fixed = TRUE) | grepl(" face mask ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Face Mask"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Illegal Forward Pass ", string, fixed = TRUE) | grepl(" illegal forward pass ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Forward Pass"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Intentional Grounding ", string, fixed = TRUE) | grepl(" intentional grounding ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Intentional Grounding"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" illegal kicking ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Kicking"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Illegal Conduct ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Illegal Conduct"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        
        else if (grepl(" kick catching interference ", string, fixed = TRUE) | grepl(" Kick Catching Interference ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Kick Catching Interference"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Unnecessary Roughness ", string, fixed = TRUE) | grepl(" unnecessary roughness ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Unecessary Roughness"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Unsportsmanlike Conduct ", string, fixed = TRUE) | grepl(" unsportsmanlike conduct ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Unsportsmanlike Conduct"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" Running Into Kicker ", string, fixed = TRUE) | grepl(" running into kicker ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Running Into Kicker"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" failure to wear required equipment ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Failure to Wear Required Equipment"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else if (grepl(" player disqualification ", string, fixed = TRUE) | grepl(" Player Disqualification ", string, fixed = TRUE)){
          
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Player Disqualification"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
        
        else{
          if (nrow(stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]])==2){
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][2,1]
          }
          
          else if (grepl("ards)", string, fixed = TRUE)) {
            start.1=stri_locate_all(pattern = "ards)", string, fixed = TRUE)[[1]][1]-2
          }
          
          else {
            start.1=stri_locate_all(pattern = " yards to the ", string, fixed = TRUE)[[1]][1]
          }
          
          
          penalty=1
          penalty_name="Missing"
          penalty_yds=as.numeric(trimws(chartr(old = "(", new = " ", substr(string, start.1-2, start.1))))
        }
      }
      }
    
    
    else{
      penalty=0
      penalty_name=NA
      penalty_yds=NA
    }
  
    if (is.na(penalty_yds)){
      if (any(abs(pbp$yards_gained[i])==c(0,5,10,15))){
        penalty_yds=abs(pbp$yards_gained[i])}
    }
    
    result=data.frame(penalty,
                      penalty_name,
                      abs(penalty_yds)
                      )
    
    names(result)=c("penalty",
                    "penalty_name",
                    "penalty_yds"
                    )
  
    
    result_df=rbind(result_df, result)
  }
  

  pbp=cbind(pbp, result_df)
  return(pbp)
}

