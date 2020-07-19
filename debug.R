
load("debug-gdalbuildvrt.rda")

names(param)[1] = "files_in"
list2env(param, .GlobalEnv)
files_in = param[[1]] %>% gsub("~", "/home/kong", .)
band = 1
gdalUtilities::gdalbuildvrt(files_in,
                            sd = band,
                            output.vrt = "a.vrt",
                            srcnodata = 255,
                            vrtnodata = 255,
                            a_srs = a_srs)
