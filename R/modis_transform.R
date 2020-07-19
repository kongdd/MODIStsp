
# modis_transform(
#     outfile_vrt, outrep_file_0,
#     out_res_sel, mod_proj_str, outproj_str, full_ext,
#     out_format, datatype, nodata_out, compress, ncores, bbox, out_res)
modis_transform <- function(
    outfile_vrt, outrep_file_0,
    out_res_sel, mod_proj_str, outproj_str, full_ext, resampling,
    out_format, datatype, nodata_out, compress, ncores, bbox, out_res)
{
    # Launch the spatial processing -
    # operations to be done depend on whether resize and/or
    # reprojection and/or resampling are necessary. Operations
    # are done on the temporary vrt file built above!

    # Identify which processing is needed
    reproj_type <- if (out_res_sel == "Native" &
                       outproj_str$wkt  == mod_proj_str$wkt) {
      "GdalTranslate" #only save to new format and mosaic

    } else if (out_res_sel == "Resampled" &
               outproj_str$wkt == mod_proj_str$wkt) {
      "Resample1_Resize0" #Change of resolution

    } else if (out_res_sel == "Native"     &
               outproj_str$wkt != mod_proj_str$wkt &
               full_ext    == TRUE) {
      "Resample0_Resize0" #Change of projection

    } else if (out_res_sel == "Native"     &
               outproj_str$wkt != mod_proj_str$wkt &
               full_ext    == FALSE) {
      "Resample0_Resize1" #Change of projection and extent

    } else if (out_res_sel == "Resampled"  &
               outproj_str$wkt != mod_proj_str$wkt &
               full_ext    == TRUE) {
      "Resample1_Resize0" #Change of resolution and
      #projection

    } else if (out_res_sel == "Resampled"  &
               outproj_str$wkt != mod_proj_str$wkt &
               full_ext    == FALSE) {
      "Resample1_Resize1"  #Change of resolution,
      #projection and extent
    } else {
      "Error"
    }

    if (out_format == "GTiff") {
      switch(reproj_type,
             GdalTranslate = gdalUtilities::gdal_translate(
               outfile_vrt,
               outrep_file_0,
               a_srs    = mod_proj_str,
               of       = out_format,
               ot       = as.character(datatype),
               a_nodata = nodata_out,
               co = paste("COMPRESS", compress, sep = "=")
               # ,
               #   verbose = FALSE
             ),
             Resample0_Resize0 = gdalUtilities::gdalwarp(
               outfile_vrt, outrep_file_0,
               s_srs = mod_proj_str,
               t_srs = outproj_str,
               of    = out_format,
               r     = resampling,
               co    = paste("COMPRESS", compress, sep = "="),
               ot    = as.character(datatype),
               multi = TRUE,
               wo    = c("INIT_DEST = NO_DATA",
                         paste0("NUM_THREADS=", ncores)),
               nomd  = TRUE,
               overwrite = TRUE
             ),
             Resample0_Resize1 = gdalUtilities::gdalwarp(
               outfile_vrt, outrep_file_0,
               s_srs  = mod_proj_str,
               t_srs  = outproj_str,
               of     = out_format,
               r      = resampling,
               te     = bbox,
               co     = paste("COMPRESS", compress, sep = "="),
               ot     = as.character(datatype),
               multi  = TRUE,
               wo     = c("INIT_DEST = NO_DATA",
                          paste0("NUM_THREADS=", ncores)),
               nomd   = TRUE,
               overwrite  = TRUE
             ),
             Resample1_Resize0 = gdalUtilities::gdalwarp(
               outfile_vrt, outrep_file_0,
               s_srs  = mod_proj_str,
               t_srs  = outproj_str,
               of     = out_format,
               r      = resampling,
               tr     = rep(out_res, 2),
               co     = paste("COMPRESS", compress, sep = "="),
               ot     = as.character(datatype),
               multi  = TRUE,
               wo     = c("INIT_DEST = NO_DATA",
                          paste0("NUM_THREADS=", ncores)),
               nomd   = TRUE,
               overwrite = TRUE
             ),
             Resample1_Resize1 = gdalUtilities::gdalwarp(
               outfile_vrt, outrep_file_0,
               s_srs     = mod_proj_str, t_srs = outproj_str,
               of        = out_format,
               r         = resampling,
               te        = bbox,
               tr        = rep(out_res, 2),
               co        = paste("COMPRESS", compress, sep = "="),
               ot        = as.character(datatype),
               multi     = TRUE,
               wo        = c("INIT_DEST = NO_DATA",
                             paste0("NUM_THREADS=", ncores)),
               nomd      = TRUE,
               overwrite = TRUE
             ),
             stop(
               "Internal error in out_res_sel, outproj_str or ",
               "full_ext. Aborting!"))
    } else {
      # on ENVI format, processing is identical, save for
      # not providing the "COMPRESSION" option to avoid
      # warnings
      switch(reproj_type,
             GdalTranslate = gdalUtilities::gdal_translate(
               outfile_vrt,
               outrep_file_0,
               a_srs     = mod_proj_str,
               of        = out_format,
               ot        = as.character(datatype),
               a_nodata  = nodata_out,
               wo        = c("INIT_DEST = NO_DATA",
                             paste0("NUM_THREADS=", ncores)),
             ),
             Resample0_Resize0 = gdalUtilities::gdalwarp(
               outfile_vrt,
               outrep_file_0,
               s_srs     = mod_proj_str,
               t_srs     = outproj_str,
               of        = out_format,
               r         = resampling,
               ot        = as.character(datatype),
               multi     = TRUE,
               wo        = c("INIT_DEST = NO_DATA",
                             paste0("NUM_THREADS=", ncores)),
               nomd      = TRUE,
               overwrite = TRUE
             ),
             Resample0_Resize1  = gdalUtilities::gdalwarp(
               outfile_vrt,
               outrep_file_0,
               s_srs     = mod_proj_str,
               t_srs     = outproj_str,
               of        = out_format,
               r         = resampling,
               te        = bbox,
               ot        = as.character(datatype),
               multi     = TRUE,
               wo        = c("INIT_DEST = NO_DATA",
                             paste0("NUM_THREADS=", ncores)),
               nomd      = TRUE,
               overwrite = TRUE
             ),
             Resample1_Resize0  =  gdalUtilities::gdalwarp(
               outfile_vrt,
               outrep_file_0,
               s_srs     = mod_proj_str,
               t_srs     = outproj_str,
               of        = out_format,
               r         = resampling,
               tr        = rep(out_res, 2),
               ot        = as.character(datatype),
               multi     = TRUE,
               wo        = c("INIT_DEST = NO_DATA",
                             paste0("NUM_THREADS=", ncores)),
               nomd      = TRUE,
               overwrite = TRUE
             ),
             Resample1_Resize1  =  gdalUtilities::gdalwarp(
               outfile_vrt,
               outrep_file_0,
               s_srs     = mod_proj_str,
               t_srs     = outproj_str,
               of        = out_format,
               r         = resampling,
               te        = bbox,
               tr        = rep(out_res, 2),
               ot        = as.character(datatype),
               multi     = TRUE,
               wo        = c("INIT_DEST = NO_DATA",
                             paste0("NUM_THREADS=", ncores)),
               nomd      = TRUE,
               overwrite = TRUE
             ),
             stop("Internal error in out_res_sel, outproj_str ",
                  "or full_ext. Aborting!"))
    }
}
