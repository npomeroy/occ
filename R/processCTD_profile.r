processCTD_profile = function(dir) {
  # List all of the .cnv files in folder
  temp = list.files(path = dir, pattern = "*.cnv")
  setwd(dir)
  
  # Loop through each .cnv cast and write data to .csv file
  for (i in 1:length(temp)) {
    ctd.raw = read.ctd.sbe(temp[i]) # Read data file
    name = file_path_sans_ext(basename(temp[i]))
    
    if (max(ctd.raw[['depth']]) < 0) {
      df = NULL
    } else {
      ctd.trim = ctdTrim(ctd.raw,
                         method = 'downcast')
      
      p.trim = ctd.trim[["pressure"]]
      d.trim = ctd.trim[["depth"]]
      
      seq = seq(1, floor(max(ctd.trim[['depth']])))
      
      if (max(seq) == 1) {
        ctd.processed = ctd.trim
        pressure = ctd.processed[['pressure']] # Pull pressure data
        depth = round(ctd.processed[['depth']]) # Pull depth data
        temperature =  ctd.processed[['temperature']] # Pull temperature data
        conductivity = ctd.processed[['conductivity']] # Pull conductivity data
        salinity =  ctd.processed[['salinity']] # Pull salinity data
        beamtransmission = ctd.processed[['beamTransmission']] # Pull beam transmission
        density = ctd.processed[['density']] - 1000 # Pull density data
        
        df.raw = data.frame(name,
                            pressure,
                            depth,
                            temperature,
                            conductivity,
                            salinity,
                            density)
        
        colnames(df.raw) = c(
          'ShallowCTDID',
          'Pressure_db',
          'Depth_m',
          'Temp_DegC',
          'Cond_S_per_m',
          'Saln_PSU',
          'Density_Sigmat'
        )
        
        df.sub = subset(df.raw, Depth_m >= 1)
        
        df = df.sub %>%
          summarize(
            ShallowCTDID = first(ShallowCTDID),
            Pressure_db = mean(Pressure_db),
            Depth_m = mean(Depth_m),
            Temp_DegC = mean(Temp_DegC),
            Cond_S_per_m = mean(Cond_S_per_m),
            Saln_PSU = mean(Saln_PSU),
            Density_Sigmat = mean(Density_Sigmat)
          )
        if (is.null(beamtransmission) == FALSE) {
          df$BeamTrans_Percent = beamtransmission
        }
        
      } else{
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
      print(paste(i, "of", length(temp), "casts processed"))
    }
  }
}