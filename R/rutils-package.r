#' rutils
#'
#' @name rutils
#' @docType package

#' Set up twitter
#' @description Initiate OAuth authentication
setup_twitter <- function() {
  library(twitteR)
  library(ROAuth)
  library(RCurl)
  requestURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  consumerKey <- "31IR5vXasfNNMO4vZpBkw"
  consumerSecret <- "Nps3lhbITl4ssjbpVMD9ni1tjnzDly58UBNSvFcmzs"
  twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=requestURL,accessURL=accessURL,authURL=authURL)
  twitCred$handshake()
}
