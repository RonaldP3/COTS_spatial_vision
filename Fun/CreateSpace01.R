# Function for ommitting superflues text entries in tables
createSpace <- function(data) {
  # Create output
  output <- rep("", times=length(data))
  output[1] <- toString(data[1])
  
  # Iterate
  for (i in 1: (length(data)-1)) {
    if (data[i] != data[i+1]) {
      output[i+1] <- toString(data[i+1])
    }
  }
  return(output)
}