source("convertHex.R")

messageDF <- function(payload){
  fieldName = c("Message ID", "IOD", "Navigation State", "WN","TOW","ECEF POS_X",
                "ECEF POS_Y","ECEF POS_Z","ECEF VEL_X","ECEF VEL_Y","ECEF VEL_Z",
                "Clock Bias","Clock Drift","GDOP","PDOP","HDOP","VDOP","TDOP","Timestamp")
  field = c(1,1,1,2,8,8,8,8,4,4,4,8,4,4,4,4,4,4)
  indexPayload = 1
  
  messageBody = c()
  
  for(i in field){
    hex = ""
    for(k in 1:i){
      hex = paste(hex, payload[indexPayload], sep = "")
      indexPayload = indexPayload + 1
    }
    if (i == 1)  messageBody <- append(messageBody, strtoi(hex,16L))
    else if (i == 2)  messageBody <- append(messageBody, strtoi(hex,16L))
    else messageBody <- append(messageBody,convertHexToFloat(hex))
  }
  ts = messageBody[4] * 7 * 24 * 3600 + messageBody[5]
  messageBody <- append(messageBody, ts)
  data <- data.frame(
    fieldName,
    messageBody
  )
  
  return (data)
}