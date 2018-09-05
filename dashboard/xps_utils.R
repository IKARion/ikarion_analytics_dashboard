require(httr)
xpsEndpoint <- "http://cosa-app.fh-luebeck.de:50101/"
sendModelToXPS <- function(model) {
  
  POST(xpsEndpoint, add_headers(Authorization = "Token 285e19f20beded7d215102b49d5c09a0"), body = toJSON(model))
}