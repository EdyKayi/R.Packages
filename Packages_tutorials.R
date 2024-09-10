#Loading libraries
#Instal usethis packages
library(usethis)
library(devtools)
library(roxygen2)

#usethis::create_package(path = "C:/Users/edemt/OneDrive - Smart Workplace/R.Packages")
use_git_config(user.name="EdyKayi", user.email="edemtitriku1@gmail")

usethis::create_github_token()

credentials::set_github_pat()

#usethis::use_github()

use_git()

usethis::use_r("mysumfunction")

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
  s+1
}

devtools::load_all()

