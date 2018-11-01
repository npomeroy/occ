trimDS = function(speed,
                  direction,
                  pressure,
                  ctd,
                  seafet,
                  read.csv = TRUE,
                  write.csv = TRUE){
  
  # Read csv files
  if (read.csv == TRUE) {
    speed = read.csv(speed, check.names = FALSE)
    direction = read.csv(direction, check.names = FALSE)
    pressure = read.csv(pressure)
    ctd = read.csv(ctd)
    seafet = read.csv(seafet)
  } else {
    speed = speed
    direction = direction
    pressure = pressure
    ctd = ctd
    seafet = seafet
  }

  speed$DateTimeUTC = ymd_hms(speed$DateTimeUTC)
  direction$DateTimeUTC = ymd_hms(direction$DateTimeUTC)
  pressure$DateTimeUTC = ymd_hms(pressure$DateTimeUTC)

  start.time = speed$DateTimeUTC[1]
  end.time = tail(speed$DateTimeUTC, n = 1)
  
  seafet.trim = subset(seafet, DateTime >= start.time & DateTime <= end.time)
  ctd.trim = subset(ctd, DateTime >= start.time & DateTime <= end.time)
  
  if (write.csv == TRUE) {
    write.csv(ctd.trim,
              paste0(ctd.trim$ShallowCTDID[1], '_trimmed.csv'),
              row.names = FALSE)
    
    write.csv(seafet.trim,
              paste0("seafet", '_trimmed.csv'),
              row.names = FALSE)
  }
  
  return(list(
    speed = speed,
    direction = direction,
    pressure = pressure,
    ctd = ctd.trim,
    seafet = seafet.trim
  ))
  
  
}
