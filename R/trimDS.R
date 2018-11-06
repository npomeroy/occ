trimDS = function(speed,
                  direction,
                  pressure,
                  ctd,
                  seafet,
                  filename,
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
  DateTimeUTC = speed$DateTimeUTC

  start.time = speed$DateTimeUTC[1]
  end.time = tail(speed$DateTimeUTC, n = 1)
  
  speed.nodate = speed[-1]
  direction.nodate = direction[-1]
  depths = as.numeric(colnames(speed.nodate))
  
  for (i in 1:nrow(speed.nodate)){
    pres.i = pressure$Pressure[i]
    depths.i = depths[depths < pres.i]
    speed.nodate[i, !depths %in% depths.i] = speed.nodate[i, !depths %in% depths.i] == NA
    direction.nodate [i, !depths %in% depths.i] =  direction.nodate [i, !depths %in% depths.i] == NA
  }
  
  nona = sapply(speed.nodate, function(x) all(is.na(x)))
  speed.nona = speed.nodate[nona == FALSE]
  direction.nona = direction.nodate[nona == FALSE]
  
  
  speed.trim = cbind(DateTimeUTC,speed.nona)
  direction.trim = cbind(DateTimeUTC,direction.nona)
  
  adcp.long.speed = melt(speed.trim, id.vars = 'DateTimeUTC')
  adcp.long.direction = melt(direction.trim, id.vars = 'DateTimeUTC')

  colnames(adcp.long.speed) = c("DateTimeUTC", "Height", "Speed")
  colnames(adcp.long.direction) = c("DateTimeUTC", "Height", "Direction")
  
  adcp.long = left_join(adcp.long.speed,adcp.long.direction, by = c("DateTimeUTC","Height"))
  colnames(adcp.long) = c("DateTimeUTC", "HeightBin_m", "Speed_ms","Direction_deg")
  adcp.long = data.frame(adcp.long[order(adcp.long$DateTimeUTC),])
  colnames(pressure) = c("DateTimeUTC","Pressure_db")
    
  seafet.trim = subset(seafet, DateTime >= start.time & DateTime <= end.time)
  colnames(seafet.trim) = c("DateTimeUTC","pH","Temp_degC")
  
  ctd.trim = subset(ctd, DateTime >= start.time & DateTime <= end.time)
  ctd.id = ctd.trim$ShallowCTDID[1]
  
  if (write.csv == TRUE) {
    write.csv(speed.trim,
              paste0(filename, '_speed.csv'),
              row.names = FALSE)
    
    write.csv(direction.trim,
              paste0(filename, '_direction.csv'),
              row.names = FALSE)
    
    write.csv(pressure,
              paste0(filename, '_pressure.csv'),
              row.names = FALSE)
    
    write.csv(adcp.long,
              paste0(filename, '_speeddirection.csv'),
              row.names = FALSE)
    
    write.csv(ctd.trim,
              paste0(filename, '_CTD.csv'),
              row.names = FALSE)
    
    write.csv(seafet.trim,
              paste0(filename, '_SeaFET.csv'),
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
