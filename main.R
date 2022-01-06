convertHexToFloat <- function(string){
  exponentLength = 11
  fractionLength = 52
  actualZero = 1023
  if (nchar(string) == 8){
    exponentLength = 8
    fractionLength = 23
    actualZero = 127
  }
  
  
  bits = c()
  for (i in 1:nchar(string)){
    int = strtoi(substring(string,i,i),16)
    bit = c()
    for (j in 1:4){
      x = int %% 2
      bit = append(bit, x)
      int = int %/% 2
    }
    bit = rev(bit)
    bits = append(bits, bit)
  }
  signBit = bits[1]
  exponent = bits[2:(exponentLength+1)]
  fraction = bits[(exponentLength+2):(nchar(string)*4)]
  #####
  sign = 1
  if (signBit == 1) sign = -1 
  #####
  e = 0
  for (i in 0:(exponentLength-1)){
    e = e + exponent[exponentLength-i]*2^i
  }
  
  #####
  f = 1
  for (i in 1:fractionLength){
    f = f + fraction[i]/(2^i)
  }
  
  
  result = sign*f*2^(e-actualZero)
  return (result)
}

messageDF <- function(payload){
  fieldName = c("Message ID", "IOD", "Navigation State", "WN","TOW","ECEF POS_X",
                "ECEF POS_Y","ECEF POS_Z","ECEF VEL_X","ECEF VEL_Y","ECEF VEL_Z",
                "Clock Bias","Clock Drift","GDOP","PDOP","HDOP","VDOP","TDOP")
  field = c(1,1,1,2,8,8,8,8,4,4,4,8,4,4,4,4,4,4)
  indexPayload = 1
  
  messageBody = c()
  
  for(i in field){
    hex = ""
    for(k in 1:i){
      hex = paste(hex, payload[indexPayload], sep = "")
      indexPayload = indexPayload + 1
    }
    messageBody <- append(messageBody, hex)
  }
  names(messageBody) <- fieldName
  return (messageBody)
}

 
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
      
      new <- append(new, hex)
    }
    
    
    SVstatus[,ncol(SVstatus)+1] <- new
    colnames(SVstatus)[ncol(SVstatus)] <- paste("SV",index, "status", sep =" ")
  }
  data = list(first4bytes, SVstatus)
  return(data)
}

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
    first14bytes <- append(first14bytes, hex)
  }
  names(first14bytes) <- fieldName
  
  
  #################
  numberOfMeasurement <- strtoi(first14bytes[9], 16L)
  
  channel = data.frame(
    Status = c("GNSS type", "Signal type","SVID","Lock time indicator","Frequency ID",
             "CN0", "Pseudorange", "Accumulated carrier cycle", "Doppler frequency",
             "Pseudorange standard deviation","Accumulated carrier cycle standard deviation",
             "Doppler frequency standard deviation", "Channel indicator", "Reserved 2")
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
      if (type[i]==1) numeral <- append(numeral, as.numeric(strtoi(new[i],16L)))
      else numeral <- append(numeral, convertHexToFloat(new[i]))
    }
    
    channel[,ncol(channel)+1] <- new
    colnames(channel)[ncol(channel)] <- paste("channel",index, sep ="_")
  }
  
  #################
  
  
  data = list(first14bytes, channel)
  return (data)
}


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
msg <- scan("msgE5.txt", character())
data <- separateMessage(msg)

# bitwShiftL
# readBin(x, numeric(), size = 8, endian = "big")