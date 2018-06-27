processCTD_moored = function(file,
                             tz.in = "UTC",
                             tz.out = "UTC",
                             write.csv = TRUE,
                             SN3029 = FALSE,
                             oxygen = TRUE) {
  
  data = suppressWarnings(read.ctd.sbe(file))
  name = file_path_sans_ext(basename(file))
  pressure = data[['pressure']]
  depth = data[['depth']]
  temperature =  data[['temperature']]
  conductivity = data[['conductivity']]
  salinity =  data[['salinity']]
  sigmaT =  data[['sigmaT']]
  start.time = ymd_hms(data[['startTime']])
  year = year(start.time)
  
  if (SN3029 == FALSE) {
    if (is.null(data[['timeSCP']]) == TRUE) {
      time.raw = data[['timeJV2']]
    } else {
      time.raw = data[['timeSCP']]
    }
    
    origin = ymd_hms(paste0(year, "-01-01 00:00:00"), tz = tz.in)
    
    date = origin + time.raw * 3600 * 24 - days(1)
    date = with_tz(date, tz = tz.out)
  }
  
  
  if (SN3029 == TRUE) {
    if (is.null(data[['timeS']]) == FALSE) {
      time.elapsed = data[['timeS']]
      date = start.time + lubridate::seconds(time.elapsed)
    } else {
      time.elapsed = data[['timeH']]
      date = start.time + lubridate::seconds(time.elapsed * 3600)
    }
    
    date = force_tz(date, tz = tz.in)
    date = with_tz(date, tz = tz.out)
    
  }
  
  
  df.raw = data.frame(name,
                      date,
                      pressure,
                      depth,
                      temperature,
                      salinity,
                      sigmaT)
  colnames(df.raw) = c(
    'ShallowCTDID',
    'DateTime',
    'Pressure_db',
    'Depth_m',
    'Temp_DegC',
    'Saln_PSU',
    'Density_Sigmat'
  )
  
  if (is.null(conductivity) == FALSE) {
    df.raw$Cond_S_per_m = conductivity
  }
  
  if (oxygen == TRUE) {
    df.raw$Oxygen_mgL = data[['oxygen']]
  }
  
  df = subset(df.raw, Saln_PSU > 20)
  
  seafet.time = format(date, format = "%Y-%m-%d %H:%M:%S")
  df.seafet.raw = data.frame(seafet.time,
                             temperature,
                             salinity)
  
  df.seafet = subset(df.seafet.raw, salinity > 20)
  colnames(df.seafet) = c("date/time", "temperature", "salinity")
  
  if (write.csv == TRUE) {
    write.csv(df,
              paste0(dirname(file), '/', name, '_processed.csv'),
              row.names = FALSE)
    
    write.table(
      df.seafet,
      paste0(dirname(file), '/', name,  '_SEAFET', '.csv'),
      row.names = FALSE,
      col.names = FALSE,
      sep = ','
    )
  }
  
  
  
  return(df)
  
  print("Moored CTD data processed")
  
}
