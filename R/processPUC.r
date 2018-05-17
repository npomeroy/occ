processPUC = function(puc.file,
                      ctd.file,
                      tz.in = "UTC",
                      tz.out = "UTC",
                      write.csv = TRUE) {

  puc.samples = read.csv(puc.file)
  ctd.data = read.csv(ctd.file)

    # Reformat PUC sample start time
  puc.samples$DateTime_LocalStart = mdy_hm(puc.samples$DateTimeLocal, tz = tz.in)

  # Convert PUC local time to UTC
  puc.samples$DateTime_UTCStart = with_tz(puc.samples$DateTime_LocalStart, tz = tz.out)

  # Find end time of PUC (45 min interval)
  puc.samples$DateTime_UTCEnd = puc.samples$DateTime_UTCStart + lubridate::minutes(45)

  # Format CTD date and time
  ctd.data$DateTime = ymd_hms(ctd.data$DateTime, tz = "UTC")

  # Loop through each PUC sample and find the average T,S,P from the correct CTD time series
  for (i in 1:nrow(puc.samples)) {
    SN.cast.subset.i = subset(
      ctd.data,
      ShallowCTDID == puc.samples$ShallowCTDID[i] &
        DateTime >= puc.samples$DateTime_UTCStart[i] &
        DateTime <= puc.samples$DateTime_UTCEnd[i]
    )
    puc.samples$Pressure_db_4DICCalc[i] = mean(SN.cast.subset.i$Pressure_db, na.rm =
                                                 TRUE)
    puc.samples$Salinity_4DICCalc[i] = mean(SN.cast.subset.i$Saln_PSU, na.rm =
                                              TRUE)
    puc.samples$Temperature_DegC_4DICCalc[i] = mean(SN.cast.subset.i$Temp_DegC, na.rm =
                                                      TRUE)
  }

  if (write.csv == TRUE) {
    write.csv(
      puc.samples,
      paste0(
        dirname(puc.file),
        '/',
        file_path_sans_ext(basename(puc.file)),
        '_processed.csv'
      ),
      row.names = FALSE
    )
  }
}
