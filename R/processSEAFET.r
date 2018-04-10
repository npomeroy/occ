processSEAFET = function(file,
                         tz.in = "UTC",
                         tz.out = "UTC",
                         csv = TRUE) {

  # Read seaFET CSV data

  test = read.csv(file, header = FALSE)

  skip = min(which(test[, 1] != "SATFHR")) - 1

  data = read.csv(file,
                  header = FALSE,
                  skip = skip)

  data$DateTime = ymd(strptime(data$V2, format = "%Y%j"), tz = tz.in) + seconds(data$V3 *
                                                                                  3600)

  data$DateTime_tz.out = with_tz(data$DateTime, tz = tz.out)

  # Rename variables and add to data frame
  date = data$DateTime_tz.out
  pH_ext = data$V5
  temp = data$V6

  processed = data.frame(date, pH_ext, temp)
  colnames(processed) = c('DateTime', 'pH', 'Temp')


  if (csv == TRUE) {
    write.csv(processed,
              paste0(
                dirname(file),
                '/',
                file_path_sans_ext(basename(file)),
                '_processed.csv'
              ),
              row.names = FALSE)
  }

  return(processed)

}
