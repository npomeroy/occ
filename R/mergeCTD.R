mergeCTD = function(dir, cruise, group) {
  setwd(dir)
  
  filenames.csv = list.files(
    path = dir,
    recursive = TRUE,
    pattern =  paste0(
      ".*",
      as.character(cruise),
      ".*",
      as.character(group),
      ".*",
      ".*csv"
    )
  )
  
  filenames.cnv = list.files(
    path = dir,
    recursive = TRUE,
    pattern =  paste0(
      ".*",
      as.character(cruise),
      ".*",
      as.character(group),
      ".*",
      ".*cnv"
    )
  )
  
  if (length(filenames.csv) != length(filenames.cnv)) {
    warning (
      'Number of csv files does not equal number of cnv files. Check that all cnv files have been processed to csv.'
    )
  }
  
  paths = paste0(dir,filenames.csv)
  paths = paths[file.size(paths)>10]
  
  df = NULL
  
  for (i in 1:length(paths)){
    df.i = read.csv(paths[i])
    df = rbind.fill(df, df.i)
  }

  write_xlsx(df, path = paste0(cruise, "_", group, "_merged.xlsx"))
  print(paste("Casts from cruise", cruise, "and region", group, "merged in", dir))
}
