#' @title gh_selprod
#' @description Handler for the actions to be taken when the product
#'  is changed
#' @noRd
#'
gh_selprod <- function(h, wids, prod_opt_list, general_opts) {
  #nocov start
  if (!all(requireNamespace(c("gWidgets", "gWidgetsRGtk2")))) {
    stop("You need to install package gWidgets to use MODIStsp GUI. Please install it with:
                install.packages(c('gWidgets', 'gWidgetsRGtk2')")
  } else {
    requireNamespace("gWidgets")
    requireNamespace("gWidgetsRGtk2")
  }
  sel_prod   <- ifelse(!is.null(gWidgets::svalue(wids$prod)),
                       gWidgets::svalue(wids$prod),
                       sel_prod)

  sel_prodopts <- prod_opt_list[[sel_prod]]
  # Select the last version (it assumes that versions in xml file are in
  # increasing order)
  wids$vers[] <- names(sel_prodopts)
  gWidgets::svalue(wids$vers) <- sel_prodopts[[length(sel_prodopts)]]$v_number #nolint
  # Disable sensor choice for combined datasets
  if (sel_prodopts[[gWidgets::svalue(wids$vers)]]$combined == 1) {
    gWidgets::enabled(wids$sens) <- FALSE
    wids$sens[] <- "Combined"
    gWidgets::svalue(wids$sens)  <- "Combined"
  } else {
    gWidgets::enabled(wids$sens) <- TRUE
    wids$sens[] <- c("Terra", "Aqua", "Both")
    gWidgets::svalue(wids$sens)  <- general_opts$sensor
  }
  # On product change, automatically modify the default projection - latlon
  # for non-tiled, Sinu tiled
# ()
  if (sel_prodopts[[gWidgets::svalue(wids$vers)]]$tiled == 0) {

    gWidgets::enabled(tiles_group) <- FALSE
    gWidgets::enabled(bbox_group)  <- TRUE
    gWidgets::svalue(wids$output_ext)   <- "Define Custom Area"
    gWidgets::svalue(wids$proj_choice)  <- "Native"
    gWidgets::svalue(wids$output_proj4) <- "4008"
    gWidgets::svalue(wids$output_xmin) <- -180
    gWidgets::svalue(wids$output_xmax) <-  180
    gWidgets::svalue(wids$output_ymin) <- -90
    gWidgets::svalue(wids$output_ymax) <-  90

  } else {
    gWidgets::svalue(wids$output_xmin) <- ""
    gWidgets::svalue(wids$output_xmax) <- ""
    gWidgets::svalue(wids$output_ymin) <- ""
    gWidgets::svalue(wids$output_ymax) <- ""
    if (gWidgets::svalue(wids$output_ext) == "Define Custom Area") {
      gWidgets::enabled(bbox_group)   <- TRUE
      gWidgets::enabled(tiles_group)  <- FALSE
    } else {
      gWidgets::enabled(bbox_group)   <- FALSE
      gWidgets::enabled(tiles_group)  <- TRUE
    }
    gWidgets::svalue(wids$proj_choice)    <- "Native"
    gWidgets::svalue(wids$output_proj4) <-
      "MODIS Sinusoidal" #nolint
  }
  # reset dummy variables for band selection to 0 on product change and
  # reset the labels corresponding to selected layers
  gui_env$temp_wid_bands         <- 0
  gui_env$temp_wid_bands_indexes <- 0
  gui_env$temp_wid_bands_quality <- 0

  cur_prodopts <- sel_prodopts[[gWidgets::svalue(wids$vers)]]

  gWidgets::svalue(wids$sel_layers) <- "- None selected -"
  gWidgets::svalue(wids$sel_qi)     <- ifelse(
    length(cur_prodopts[["quality_bandnames"]]) == 0,
    "- No Quality Indicators are available for this product - ",
    " - None selected - "
  )
  gWidgets::svalue(wids$sel_si) <- ifelse(
    length(cur_prodopts[["indexes_bandnames"]]) == 0,
    "- Spectral Indexes can not be computed on this product - ",
    " - None selected - "
  )

  #nocov end
}
