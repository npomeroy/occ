processCTD_moored = function(file,
                             tz.in = "UTC",
                             tz.out = "UTC",
                             csv = TRUE,
                             SN3029 = FALSE) {
  if (SN3029 == FALSE) {
    data = read.ctd.sbe(file)
    name = file_path_sans_ext(basename(file))
    pressure = data[['pressure']]
    depth = data[['depth']]
    temperature =  data[['temperature']]
    conductivity = data[['conductivity']]
    salinity =  data[['salinity']]
    sigmaT =  data[['sigmaT']]
    start.time = ymd_hms(data[['startTime']])
    year = year(start.time)

    if (is.null(data[['timeSCP']]) == TRUE) {
      time.raw = data[['timeJV2']]
    } else {
      time.raw = data[['timeSCP']]
    }

    origin = ymd_hms(paste0(year, "-01-01 00:00:00"), tz = tz.in)

    date = origin + time.raw * 3600 * 24 - days(1)

    date = with_tz(date, tz = tz.out)

    df.raw = data.frame(name,
                    date,
                    pressure,
                    depth,
                    temperature,
                    conductivity,
                    salinity,
                    sigmaT)
    colnames(df.raw) = c(
      'ShallowCTDID',
      'DateTime',
      'Pressure_db',
      'Depth_m',
      'Temp_DegC',
      'Cond_S_per_m',
      'Saln_PSU',
      'Density_Sigmat'
    )

    df = subset(df.raw, Saln_PSU > 20)

    seafet.time = format(date, format = "%Y-%m-%d %H:%M:%S")
    df.seafet.raw = data.frame(seafet.time,
                           temperature,
                           salinity)

    df.seafet = subset(df.seafet.raw, salinity > 20)

  }
  if (SN3029 == TRUE) {
    data = read.ctd.sbe(file)
    name = file_path_sans_ext(basename(file))
    pressure = data[['pressure']]
    depth = data[['depth']]
    temperature =  data[['temperature']]
    salinity =  data[['salinity']]
    start.time = ymd_hms(data[['startTime']])
    year = year(start.time)

    if (is.null(data[['timeS']]) == FALSE) {
      time.elapsed = data[['timeS']]
      date = start.time + lubridate::seconds(time.elapsed)
    } else {
      time.elapsed = data[['timeH']]
      date = start.time + lubridate::seconds(time.elapsed*3600)
    }


    date = force_tz(date, tz = tz.in)

    date = with_tz(date, tz = tz.out)

    df = data.frame(name,
                    date,
                    pressure,
                    depth,
                    temperature,
                    salinity)
    colnames(df) = c(
      'ShallowCTDID',
      'DateTime',
      'Pressure_db',
      'Depth_m',
      'Temp_DegC',
      'Saln_PSU'
    )

    seafet.time = format(date, format = "%Y-%m-%d %H:%M:%S")
    df.seafet = data.frame(seafet.time,
                           temperature,
                           salinity)
    colnames(df.seafet) = c("date/time", "temperature", "salinity")

  }

  if (csv == TRUE) {
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



  return(list(ctd.ts = df, seafet.ts = df.seafet))

}
