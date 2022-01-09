source("convertHex.R")

messageE5 <- function(payload){
  fieldName = c("Message ID", "Version", "IOD", "Receiver WN", "Receiver TOW",
                "Measurement period","Measurement indicator", "Reserved 1", "NMEAS")
  first14bytes = c()
  
  
  indexPayload = 1
  
  field = c(1,1,1,2,4,2,1,1,1)
  #####################
  
  for(i in field){
    hex = ""
    for(k in 1:i){
      hex = paste(hex, payload[indexPayload], sep = "")
      indexPayload = indexPayload + 1
    }
    first14bytes <- append(first14bytes, strtoi(hex,16L))
  }
  names(first14bytes) <- fieldName
  
  
  #################
  numberOfMeasurement <- first14bytes[9]
  
  channel = data.frame(
    Status = c("GNSS type", "Signal type","SVID","Lock time indicator","Frequency ID",
               "CN0", "Pseudorange", "Accumulated carrier cycle", "Doppler frequency",
               "Pseudorange standard deviation","Accumulated carrier cycle standard deviation",
               "Doppler frequency standard deviation", "Channel indicator", "Reserved 2",
               "Timestamp")
  )
  type = c(1,1,1,1,1,1,2,2,2,1,1,1,1,1)
  
  ##################
  for(index in 1:numberOfMeasurement){
    
    new = c()
    channelField = c(1,1,1,1,8,8,4,1,1,1,2,2)
    for(i in channelField){
      hex = ""
      for(k in 1:i){
        hex = paste(hex, payload[indexPayload], sep = "")
        indexPayload = indexPayload + 1
      }
      
      new <- append(new, hex)
    }
    
    for(i in c(1,4)){
      new <- append(new, substring(new[i],2,2),i)
      new[i] <- substring(new[i], 1,1)
    }
    
    numeral = c()
    for (i in 1:14){
      if (type[i]==1) numeral <- append(numeral, strtoi(new[i],16L))
      else numeral <- append(numeral, convertHexToFloat(new[i]))
    }
    
    ts = first14bytes[4] * 7 * 24 * 3600 + first14bytes[5]/1000
    numeral <- append(numeral, ts)
    
    channel[,ncol(channel)+1] <- numeral
    colnames(channel)[ncol(channel)] <- paste("channel",index, sep ="_")
  }
  
  timestamp <- c()
  #################
  
  
  data = list(first14bytes, channel)
  return (data)
}
