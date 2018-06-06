processCTD_profile = function(dir) {
  # List all of the .cnv files in folder
  temp = list.files(path = dir, pattern = "*.cnv")
  setwd(dir)
  
  # Loop through each .cnv cast and write data to .csv file
  for (i in 1:length(temp)) {
    ctd.raw = read.ctd.sbe(temp[i]) # Read data file
    name = file_path_sans_ext(basename(temp[i]))
    
    if (max(ctd.raw[['depth']]) < 3) {
      df = NULL
    } else {
      ctd.trim = ctdTrim(ctd.raw,
                         method = 'sbe',
                         parameters = list(minSoak = 0.5, maxSoak = 3))
      
      p.trim = ctd.trim[["pressure"]]
      d.trim = ctd.trim[["depth"]]
      
      p.seq = predict(lm(p.trim ~ d.trim), data.frame(d.trim = seq(1, floor(
        max(ctd.trim[['depth']])
      ), by = 1)))
      
      ctd.processed = ctdDecimate(ctd.trim, p = p.seq)
      
      pressure = ctd.processed[['pressure']] # Pull pressure data
      depth = round(ctd.processed[['depth']]) # Pull depth data
      temperature =  ctd.processed[['temperature']] # Pull temperature data
      conductivity = ctd.processed[['conductivity']] # Pull conductivity data
      salinity =  ctd.processed[['salinity']] # Pull salinity data
      beamtransmission = ctd.processed[['beamTransmission']] # Pull beam transmission
      density = ctd.processed[['density']] - 1000 # Pull density data
      
      df = data.frame(name,
                      pressure,
                      depth,
                      temperature,
                      conductivity,
                      salinity,
                      density)
      
      colnames(df) = c(
        'ShallowCTDID',
        'Pressure_db',
        'Depth_m',
        'Temp_DegC',
        'Cond_S_per_m',
        'Saln_PSU',
        'Density_Sigmat'
      )
      
      if (is.null(beamtransmission) == FALSE) {
        df$BeamTrans_Percent = beamtransmission
      }
    }
    write.csv(df, paste0(dir, name, '.csv'), row.names = FALSE)
    print(paste(i,"of", length(temp), "casts processed"))
  }
}
