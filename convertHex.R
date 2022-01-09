convertHexToFloat <- function(string){
  exponentLength = 11
  fractionLength = 52
  actualZero = 1023
  if (nchar(string) == 8){
    exponentLength = 8
    fractionLength = 23
    actualZero = 127
  }
  if(nchar(string)==4){
    exponentLength = 15
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
  if(nchar(string)==4) return (sign*e)
  
  #####
  f = 1
  for (i in 1:fractionLength){
    f = f + fraction[i]/(2^i)
  }
  
  
  result = sign*f*2^(e-actualZero)
  return (result)
}