mergedataSTR = function(str.file, lookup.file, output.dir){
  
  data = read.csv(str.file)
  lookup = read.csv(lookup.file)
  
  path = dirname(str.file)
  path.str = unlist(strsplit(path,"/"))[4]
  filename = paste0(file_path_sans_ext(basename(str.file)))
  
  SN = ifelse(is.na(unlist(strsplit(
    as.character(data.frame(
      strsplit(filename, "SBE0", 'CollapseDelimiters', true)
    )[2, ]), "_"
  ))[1]),
  unlist(strsplit(as.character(
    data.frame(strsplit(
      filename, "SBE", 'CollapseDelimiters', true
    ))[2, ]
  ), "_"))[1],
  unlist(strsplit(as.character(
    data.frame(strsplit(
      filename, "SBE0", 'CollapseDelimiters', true
    ))[2, ]
  ), "_"))[1])
  
  
  data$Serial.Number = SN    
  colnames(data) = c('DateTimeUTC','Temp_degC','Serial.Number')
  
  lookup.SN = subset(lookup, Serial.Number == paste(SN))
  data$Latitude = lookup.SN$Latitude
  data$Longitude = lookup.SN$Longitude
  data$Depth_ft = lookup.SN$Depth
  data$Depth_m = data$Depth_ft*0.3048
  data$RegionCode = lookup.SN$RegionCode
  data$Location = lookup.SN$Location
  data$LocationCode = lookup.SN$LocationCode
  data$REA_site = as.factor(ifelse(lookup.SN$REA_site == "",NA,as.character(lookup.SN$REA_site)))
  data$SiteTag = lookup.SN$SiteTag
  data$DeployCruise = lookup.SN$DeployCruise
  data$RetrieveCruise = paste(path.str)
  
  output.name = paste0(first(data$RetrieveCruise),"_", first(data$REA_site),"_SBE",SN,"_",round(first(data$Depth_m)),"m")
  
  write.csv(data, paste0(output.dir,output.name,".csv"), row.names = FALSE)
  
  print(paste0('STR time series for SN' , SN, ' merged with metadata and saved at ', output.dir))
  
}
