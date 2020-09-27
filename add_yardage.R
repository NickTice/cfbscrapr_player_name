#' Clean yardage
#' Cleans CFB (D-I) Yardage Data to create yardage columns
#'
#' @param play_df (\emph{data.frame} required) Play dataframe pulled from API via the `cfb_pbp_data()` function
#' @details Cleans CFB (D-I) Yardage Data to create yardage columns.
#' @return The original `play_df` with the following columns appended to it:
#' \describe{
#' \item{yds_rushed}{Rushing Yards}
#' \item{yds_receiving}{Receiving Yards}
#' \item{yds_kickoff}{Kickoff Yards}
#' \item{yds_punted}{Punt Yards}
#' \item{punt_yd_line}{Punt Yard Line}
#' \item{punt_yds_gained}{Punt Yards Gained}
#' \item{yds_punt_return}{Punt Return yardage}
#' \item{yds_fumble_return}{Fumble Return yardage}
#' \item{yds_sacked}{Sack yardage}
#' }
#' @keywords internal
#' @import stringr
#' @import dplyr
#' @import tidyr
#' @export
#'

add_yardage <- function(play_df){
  
  play_df <- play_df %>% 
    dplyr::mutate(
      yds_rushed = case_when(
        .data$rush_vec == 1 & str_detect(.data$play_text, regex("run for no gain", ignore_case = TRUE)) ~ 0,
        .data$rush_vec == 1 & 
          str_detect(.data$play_text, regex("run for a loss of", ignore_case = TRUE)) ~ 
          -1*as.numeric(stringr::str_extract(
            stringi::stri_extract_first_regex(.data$play_text, '(?<= run for a loss of)[^,]+'), "\\d+")),
        .data$rush_vec == 1 & 
          str_detect(.data$play_text, regex("run for", ignore_case = TRUE)) ~ 
          as.numeric(stringr::str_extract(
            stringi::stri_extract_first_regex(.data$play_text, '(?<= run for)[^,]+'), "\\d+")),
        .data$rush_vec == 1 & 
          str_detect(.data$play_text, regex("yd run", ignore_case = TRUE)) ~ 
          as.numeric(
            stringr::str_remove(stringr::str_extract(.data$play_text, regex('\\d{0,2} Yd Run', ignore_case = TRUE)), 
                                regex("yd run", ignore_case = TRUE))),
        TRUE ~ 0
      ),
      yds_receiving = case_when(
        .data$pass_vec == 1 & str_detect(.data$play_text, regex("pass complete to", ignore_case = TRUE)) & 
          str_detect(.data$play_text, regex("for no gain", ignore_case = TRUE)) ~ 0,
        .data$pass_vec == 1 & 
          str_detect(.data$play_text, regex("pass complete to", ignore_case = TRUE)) & 
          str_detect(.data$play_text, regex("for a loss of", ignore_case = TRUE)) ~ 
          -1*as.numeric(stringr::str_extract(
            stringi::stri_extract_first_regex(.data$play_text, '(?<= for a loss of)[^,]+'), "\\d+")),
        .data$pass_vec == 1 & 
          str_detect(.data$play_text, regex("pass complete to", ignore_case = TRUE)) ~ 
          as.numeric(stringr::str_extract(
            stringi::stri_extract_first_regex(.data$play_text, '(?<= for)[^,]+'), "\\d+")),
        TRUE ~ 0
      ),
      yds_kickoff = 0,
      yds_kickoff = ifelse(.data$kickoff_play == 1, as.numeric(stringr::str_extract(
        stringi::stri_extract_first_regex(.data$play_text, '(?<= kickoff for)[^,]+'),"\\d+")), .data$yds_kickoff),
      yds_punted = 0,
      yds_punted = ifelse(.data$punt == 1, as.numeric(stringr::str_extract(
        stringi::stri_extract_first_regex(.data$play_text, '(?<= punt for)[^,]+'),"\\d+")), .data$yds_punted),
      
      yds_punt_return = case_when(
        .data$punt == 1 & .data$touchback_punt ~ 20,
        .data$punt == 1 & !.data$touchback_punt & 
          str_detect(.data$play_text, regex("fair catch|fair caught|out-of-bounds", ignore_case = TRUE)) ~ 0,
        .data$punt == 1 & !.data$touchback_punt ~ as.numeric(stringr::str_extract(
          stringi::stri_extract_first_regex(.data$play_text, '(?<= returns for)[^,]+'), "\\d+")),
        TRUE ~ 0
      ),
      yds_fumble_return = case_when(
        .data$fumble_vec == 1 & .data$kickoff_play != 1 ~ as.numeric(stringr::str_extract(
          stringi::stri_extract_first_regex(.data$play_text, '(?<= return for)[^,]+'), "\\d+")),
        TRUE ~ 0),
      yds_sacked = case_when(
        .data$sack == 1 ~ -1*as.numeric(stringr::str_extract(
          stringi::stri_extract_first_regex(.data$play_text, '(?<= sacked)[^,]+'), "\\d+")),
        TRUE ~ 0)) 
  return(play_df)
}
