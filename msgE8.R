source("convertHex.R")

messageE8 <- function(payload){
  fieldName = c("Message ID", "Version", "IOD", "NSVS")
  first4bytes = c()
  indexPayload = 1
  field = c(1,1,1,1)
  
  ######################
  for(i in field){
    hex = ""
    for(k in 1:i){
      hex = paste(hex, payload[indexPayload], sep = "")
      indexPayload = indexPayload + 1
    }
    first4bytes <- append(first4bytes, hex)
  }
  names(first4bytes) <- fieldName
  
  
  ###########################
  numberOfSatellite <- strtoi(first4bytes[4], 16L)
  SVstatus = data.frame(
    Status = c("GNSS Type", "SVID", "Elevation", "Azimuth")
  )
  ###########################
  for(index in 1:numberOfSatellite){
    
    new = c()
    channelField = c(1,1,2,2)
    for(i in channelField){
      hex = ""
      for(k in 1:i){
        hex = paste(hex, payload[indexPayload], sep = "")
        indexPayload = indexPayload + 1
      }
      if (i==1) new <- append(new, strtoi(hex,16L))
      else new <- append(new, convertHexToFloat(hex))
    }
    
    
    SVstatus[,ncol(SVstatus)+1] <- new
    colnames(SVstatus)[ncol(SVstatus)] <- paste("SV",index, "status", sep =" ")
  }
  data = list(first4bytes, SVstatus)
  return(data)
}