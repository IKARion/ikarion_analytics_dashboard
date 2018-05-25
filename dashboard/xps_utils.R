require(httr)
xpsEndpoint <- "http://cosa-app.fh-luebeck.de:50101/"
sendModelToXPS <- function(model) {
  
  POST(xpsEndpoint, add_headers(Authorization = "Token b5b41fac0361d157d9673ecb926af5ae"), body = toJSON(model))
}