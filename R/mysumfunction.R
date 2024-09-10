

#' Title
#'
#' @param param + A numeric value
#'
#' @return Output the input number plus by 1
#' @export
#'
#' @examples mysumfunction(1)
#'



mysumfunction <- function (s){
  s <- as.numeric(stringr::str_extract_all(s,"\\d+"))
  s+1
}
