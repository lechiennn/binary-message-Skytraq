
separateMessage <- function(msg){

  #payload
  payload = msg[c(-1:-4,(-length(msg)+2):(-length(msg)))]
  
  fieldName = c("Message ID", "Version", "IOD", "Receiver WN", "Receiver TOW",
                "Measurement period","Measurement indicator", "Reserved 1", "NMEAS")
  hexString = c()
  
  indexPayload = 1
  
  field = c(1,1,1,2,4,2,1,1,1)
  
  for(i in field){
    hex = ""
    for(k in 1:i){
      hex = paste(hex, payload[indexPayload], sep = "")
      indexPayload = indexPayload + 1
    }
    hexString <- append(hexString, hex)
  }
  
  numberOfMeasurement <- strtoi(hexString[9], 16L)
  #test
  channel = data.frame(
    id = c(1:12)
  )
  
  ##################
  for(index in 1:numberOfMeasurement){
    #test
    new = c()
    channelField = c(1,1,1,1,8,8,4,1,1,1,2,2)
    for(i in channelField){
      hex = ""
      for(k in 1:i){
        hex = paste(hex, payload[indexPayload], sep = "")
        indexPayload = indexPayload + 1
      }
      #hexString <- append(hexString, hex)
      new <- append(new, hex)
    }
    #test
    channel[,ncol(channel)+1] <- new
    colnames(channel)[ncol(channel)] <- paste("channel",index, sep ="_")
  }
  
  #################
  
  data = list(hexString, channel)
  
  return(data)
}
msg <- scan("msg.txt", character())
data <- separateMessage(msg)