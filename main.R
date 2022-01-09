source("msgE5.R")
source("msgE8.R")
source("msgDF.R")

separateMessage <- function(msg){

  #payload
  payload = msg[c(-1:-4,(-length(msg)+2):(-length(msg)))]
  
  data <- switch(
    payload[1],
    "E5" = messageE5(payload),
    "E8" = messageE8(payload),
    "DF" = messageDF(payload)
  )
  
  return(data)
}
msgE5 <- scan("msgE5.txt", character())
dataE5 <- separateMessage(msgE5)
msgE8 <- scan("msgE8.txt", character())
dataE8 <- separateMessage(msgE8)
msgDF <- scan("msgDF.txt", character())
dataDF <- separateMessage(msgDF)
# bitwShiftL
# readBin(x, numeric(), size = 8, endian = "big")