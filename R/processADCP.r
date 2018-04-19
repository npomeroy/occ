processADCP = function(file, write.csv = TRUE) {

  data.all = read.aquadoppProfiler(file,
                                   tz = 'UTC',
                                   despike = TRUE)

  first = min(
    which(
      data.all[['heading']] <= median(data.all[['heading']]) + 0.1 &
        data.all[['heading']] >= median(data.all[['heading']]) - 0.1 &
        data.all[['pressure']] <= median(data.all[['pressure']]) + 2 &
        data.all[['pressure']] >= median(data.all[['pressure']]) - 2
    )
  )
  last = max(
    which(
      data.all[['heading']] <= median(data.all[['heading']]) + 0.1 &
        data.all[['heading']] >= median(data.all[['heading']]) - 0.1 &
        data.all[['pressure']] <= median(data.all[['pressure']]) + 2 &
        data.all[['pressure']] >= median(data.all[['pressure']]) - 2
    )
  )

  data.xyz = read.aquadoppProfiler(
    file,
    from = first,
    to = last,
    tz = 'UTC',
    despike = TRUE
  )

  if (data.xyz[["oceCoordinate"]] == "enu") {
    data.enu = data.xyz
  } else {
    data.enu = xyzToEnuAdp(data.xyz, declination = 0)
  }

  DateTimeUTC = data.enu[['time']]
  Distance = data.enu[['distance']]
  Pressure = data.enu[['pressure']]
  Temperature = data.enu[['temperature']]
  Heading = data.enu[['heading']]
  Pitch = data.enu[['pitch']]
  Roll = data.enu[['roll']]
  Error = data.enu[['error']]
  name = file_path_sans_ext(basename(file))


  speed = sqrt((data.enu[["v"]][, , 1]) ^ 2 + (data.enu[["v"]][, , 2]) ^ 2 + (data.enu[["v"]][, , 3]) ^
                 2)
  speed.df = as.data.frame(speed)

  direction = (rad2deg(atan2(data.enu[["v"]][, , 2], data.enu[["v"]][, , 1])))
  direction = ifelse(direction > 0, direction, direction + 360)
  direction.df = as.data.frame(direction)

  colnames(speed) = paste0('Speed', Distance)
  colnames(direction) = paste0('Dir', Distance)

  adcp.metadata = data.frame(DateTimeUTC,
                             Heading,
                             Pitch,
                             Roll,
                             Pressure,
                             Temperature)

  speed.df = data.frame(DateTimeUTC, speed)
  direction.df = data.frame(DateTimeUTC, direction)
  pressure.df = data.frame(DateTimeUTC, Pressure)

  colnames(speed.df) = c("DateTimeUTC", paste0(round(Distance, digits = 2)))
  colnames(direction.df) = c("DateTimeUTC", paste0(round(Distance, digits = 2)))

  if (write.csv == TRUE) {
    write.csv(speed.df,
              paste0(dirname(file), '/', name, '_speed.csv'),
              row.names = FALSE)

    write.csv(direction.df,
              paste0(dirname(file), '/', name, '_direction.csv'),
              row.names = FALSE)

    write.csv(pressure.df,
              paste0(dirname(file), '/', name, '_pressure.csv'),
              row.names = FALSE)
  }


  return(list(
    speed = speed.df,
    direction = direction.df,
    pressure = pressure.df
  ))

}
