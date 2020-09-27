#' Add Player Columns 
#' Extracts player names from play text
#'
#' @param pbp (\emph{String} required): Team, select a valid team in D-I football
#' 
#' @return A data frame with 11 variables:
#' \describe{
#'   \item{\code{rusher_player_name}}{character.}
#'   \item{\code{receiver_player_name}}{character.}
#'   \item{\code{passer_player_name}}{character.}
#'   \item{\code{sack_player_name}}{integer.}
#'   \item{\code{sack_player_name2}}{integer.}
#'   \item{\code{interception_player_name}}{integer.}
#'   \item{\code{punter_player_name}}{integer.}
#'   \item{\code{fg_kicker_player_name}}{character.}
#'   \item{\code{kickoff_player_name}}{character.}
#'   \item{\code{kickoff_returner_player_name}}{character.}
#'   \item{\code{punt_returner_player_name}}{character.}
#' }
#' 
#' @keywords Team Roster
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "GET"
#' @importFrom utils "URLencode"
#' @importFrom assertthat "assert_that"
#' @importFrom glue "glue"
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @export
#' @examples
#' pbp <- cfb_pbp_data(year = 2020, week = 3, team = 'Miami', epa_wpa = TRUE)
#' add_player_cols(pbp)
#'

add_player_cols <- function(pbp) {
  ## Extract player names
  # RB names 
  pbp <- pbp %>%
    mutate(
      rush_player = ifelse(.data$rush == 1, 
                           str_extract(.data$play_text, "(.{0,25} )run |(.{0,25} )\\d{0,2} Yd Run"), NA),
      rush_player = str_remove(.data$rush_player, " run | \\d+ Yd Run"))
  # QB names 
  pbp <- pbp %>%
    mutate(
      pass_player = 
        ifelse(.data$pass == 1 & .data$play_type != "Passing Touchdown", 
               str_extract(.data$play_text, 
                           "pass from (.*?) \\(|(.{0,30} )pass |(.+) sacked by|(.+) sacked for|(.{0,30} )incomplete "), NA),
      pass_player = str_remove(.data$pass_player, "pass | sacked by| sacked for| incomplete"),
      pass_player = if_else(.data$play_type == "Passing Touchdown", 
                            str_extract(.data$play_text, "pass from(.+)"), .data$pass_player),
      pass_player = str_remove(.data$pass_player, "pass from "), 
      pass_player = str_remove(.data$pass_player, "\\(.+\\)"),
      pass_player = str_remove(.data$pass_player, " \\,"),
      pass_player = ifelse(.data$play_type == "Passing Touchdown" & is.na(.data$pass_player),
                           str_extract(.data$play_text, "(.+)pass complete to"), .data$pass_player),
      pass_player = str_remove(.data$pass_player, " pass complete to(.+)"),
      pass_player = str_remove(.data$pass_player, " pass complete to"),
      pass_player = ifelse(.data$play_type == "Passing Touchdown" & is.na(.data$pass_player),
                           str_extract(.data$play_text, "(.+)pass,to"), .data$pass_player),
      pass_player = str_remove(.data$pass_player, " pass,to(.+)"),
      pass_player = str_remove(.data$pass_player, " pass,to"))
  ## Receiver names
  pbp <- pbp %>%
    mutate(
      receiver_player = ifelse(.data$pass == 1, str_extract(.data$play_text, "to (.+)"), NA),
      receiver_player = if_else(str_detect(.data$play_text, regex("Yd pass", ignore_case = TRUE)),
                                str_extract(.data$play_text, "(.{0,25} )\\d{0,2} Yd pass"),
                                .data$receiver_player),
      receiver_player = if_else(str_detect(.data$play_text, regex("Yd TD pass", ignore_case = TRUE)),
                                str_extract(.data$play_text, "(.{0,25} )\\d{0,2} Yd TD pass"),
                                .data$receiver_player),
      receiver_player = ifelse(.data$play_type == "Sack" |
                                 .data$play_type == "Interception Return" |
                                 .data$play_type == "Interception Return Touchdown" |
                                 (.data$play_type %in% c("Fumble Recovery (Opponent)",
                                                   "Fumble Recovery (Opponent) Touchdown") &
                                    str_detect(.data$play_text, "sacked")), NA, .data$receiver_player),
      receiver_player = str_remove(.data$receiver_player, "to "),
      receiver_player = str_remove(.data$receiver_player, "\\,.+"),
      receiver_player = str_remove(.data$receiver_player, "for (.+)"),
      receiver_player = str_remove(.data$receiver_player, " (\\d{1,2})"),
      receiver_player = str_remove(.data$receiver_player, " Yd pass"),
      receiver_player = str_remove(.data$receiver_player, " Yd TD pass"),
      receiver_player = str_remove(.data$receiver_player, "pass complete to"),
      receiver_player = str_remove(.data$receiver_player, regex("penalty", ignore_case = TRUE)),
      receiver_player = ifelse(!str_detect(.data$receiver_player, "III"),
                               str_remove(.data$receiver_player, "[A-Z]{3,}+"), .data$receiver_player),
      receiver_player = ifelse(!str_detect(.data$receiver_player, "III"),
                               str_remove(.data$receiver_player, "[A-Z]{3,}+"), .data$receiver_player),
      receiver_player = ifelse(!str_detect(.data$receiver_player, "III"),
                               str_remove(.data$receiver_player, "[A-Z]{3,}+"), .data$receiver_player),
      receiver_player = str_remove(.data$receiver_player, " &"),
      receiver_player = str_remove(.data$receiver_player, "A&M"),
      receiver_player = str_remove(.data$receiver_player, " ST"),
      receiver_player = str_remove(.data$receiver_player, " GA"),
      receiver_player = str_remove(.data$receiver_player, " UL"),
      receiver_player = str_remove(.data$receiver_player, " FL"),
      receiver_player = str_remove(.data$receiver_player, " OH"),
      receiver_player = str_remove(.data$receiver_player, " NC"),
      receiver_player = str_remove(.data$receiver_player, " É"),
      receiver_player = str_remove(.data$receiver_player, " fumbled,"),
      receiver_player = str_remove(.data$receiver_player, "the (.+)"),
      receiver_player = str_remove(.data$receiver_player, "pass incomplete to"),
      receiver_player = str_remove(.data$receiver_player, "(.+)pass incomplete to"),
      receiver_player = str_remove(.data$receiver_player, "(.+)pass incomplete"),
      receiver_player = str_remove(.data$receiver_player, "pass incomplete"))
  
  ## Extract player names
  ## Sack player names
  pbp <- pbp %>%
    mutate(
      sack_players = ifelse(.data$pass == 1 & .data$play_type == "Sack", 
                            str_extract(.data$play_text, "sacked by(.+)"), NA),
      sack_players = str_remove(.data$sack_players, "for (.+)"),
      sack_players = str_remove(.data$sack_players, "(.+)by"),
      sack_player1 = str_remove(.data$sack_players, "and (.+)"),
      sack_player2 = if_else(str_detect(.data$sack_players, "and (.+)"), 
                             str_remove(.data$sack_players, " (.+) and"), NULL)) 
  ## Interception player names
  pbp <- pbp %>%
    mutate(
      interception_player = ifelse(.data$pass == 1 & (.data$play_type == "Interception Return"| 
                                                        .data$play_type == "Interception Return Touchdown"),
                                   str_extract(.data$play_text, "intercepted (.+)"), NA),
      interception_player = if_else(str_detect(.data$play_text, regex("Yd pass", ignore_case = TRUE)),
                                    str_extract(.data$play_text, "(.{0,25} )\\d{0,2} Yd pass"),
                                    .data$interception_player),
      interception_player = if_else(str_detect(.data$play_text, regex("Yd Interception Return", ignore_case = TRUE)),
                                    str_extract(.data$play_text, "(.{0,25} )\\d{0,2} Yd Interception Return"),
                                    .data$interception_player),
      interception_player = str_remove(.data$interception_player, "return (.+)"),
      interception_player = str_remove(.data$interception_player, "(.+) intercepted "),
      interception_player = str_remove(.data$interception_player, "intercepted"),
      interception_player = str_remove(.data$interception_player, " Yd Interception Return"),
      interception_player = str_remove(.data$interception_player, " (\\d{1,2})"))
  
  ## Punter Name
  pbp <- pbp %>%
    mutate(
      punter_player = ifelse(str_detect(.data$play_type, "Punt"),
                             str_extract(.data$play_text, ".{0,25} punt"), NA),
      punter_player = str_remove(.data$punter_player," punt"))
  
  ## Punt Returner
  pbp <- pbp %>%
    mutate(
      punt_returner_player = ifelse(str_detect(.data$play_type, "Punt"), 
                                    str_extract(.data$play_text, ", .{0,25} returns"), NA),
      punt_returner_player = str_remove(.data$punt_returner_player, ", "),
      punt_returner_player = str_remove(.data$punt_returner_player, " returns")) 
  
  ## Kickoff Specialist Name
  pbp <- pbp %>%
    mutate(
      kickoff_player = ifelse(str_detect(.data$play_type, "Kickoff"),
                              str_extract(.data$play_text, ".{0,25} kickoff"), NA),
      kickoff_player = str_remove(.data$kickoff_player," kickoff"))
  
  ## Kickoff Returner
  pbp <- pbp %>% 
    mutate(
      kickoff_returner_player = ifelse(str_detect(.data$play_type,"ickoff"),
                                       str_extract(.data$play_text,", .{0,25} return"), NA),
      kickoff_returner_player = str_remove(.data$kickoff_returner_player,", "),
      kickoff_returner_player = str_remove(.data$kickoff_returner_player," return")) 
  ## Field Goal Kicker
  pbp <- pbp %>% 
    mutate(
      fg_kicker_player = ifelse(str_detect(.data$play_type, "Field Goal"),
                                str_extract(.data$play_text, regex("(.{0,25} )\\d{0,2} yd field goal| | (.{0,25} )\\d{0,2} yd fg", ignore_case = TRUE)), NA),
      fg_kicker_player = str_remove(.data$fg_kicker_player, regex(" Yd Field Goal|Yd FG", ignore_case = TRUE)),
      fg_kicker_player = str_remove(.data$fg_kicker_player," (\\d{1,2})")
    )
  
  
  pbp <- pbp %>%
    rename(
      rusher_player_name = .data$rush_player,
      receiver_player_name = .data$receiver_player,
      passer_player_name = .data$pass_player,
      sack_player_name = .data$sack_player1,
      sack_player_name2 = .data$sack_player2,
      interception_player_name = .data$interception_player,
      punter_player_name = .data$punter_player,
      fg_kicker_player_name = .data$fg_kicker_player,
      kickoff_player_name = .data$kickoff_player,
      kickoff_returner_player_name = .data$kickoff_returner_player,
      punt_returner_player_name = .data$punt_returner_player)
  return(pbp)
  
}

