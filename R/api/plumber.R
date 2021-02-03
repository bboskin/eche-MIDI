#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(jsonlite)

#* @apiTitle Plumber Example API

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

#* Get next train
#* @get /bart
function() {
  plaza <- fromJSON("http://api.bart.gov/api/etd.aspx?cmd=etd&orig=plza&key=MW9S-E7SL-26DU-VV8V&json=y")
  as.numeric(plaza$root$station$etd[[1]]$estimate[[1]]$minutes)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @get /sum
function(a, b) {
    as.numeric(a) - as.numeric(b)
}
