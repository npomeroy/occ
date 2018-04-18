mergeCTD = function(dir, cruise, group){

  setwd(dir)

  filenames.csv = list.files(path = dir, pattern =  paste0(".*",
                                                       as.character(cruise),
                                                       ".*",
                                                       as.character(group),
                                                       ".*",
                                                       ".*csv"))

  filenames.cnv = list.files(path = dir, pattern =  paste0(".*",
                                           as.character(cruise),
                                           ".*",
                                           as.character(group),
                                           ".*",
                                           ".*cnv"))

  if (length(filenames.csv) != length(filenames.cnv)) {
     warning ('Number of csv files does not equal number of cnv files. Check that all cnv files have been processed to csv.')
  }


  df = rbind.fill(lapply(filenames.csv, function(x) {
    read.csv(x)
  }))

  write_xlsx(df,path = paste0(cruise,"_",group,"_merged.xlsx"))
}
