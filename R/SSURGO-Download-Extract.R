#' @author Zachary Kramer, \email{zbk3@nau.edu}


################
# Main function
################
#' @title Download and extract SSURGO data
#' @description Given sets of coordinates, download the respective SSURGO area for each set, extract
#' the data into both spatial and tabular files, select a set of keys, extract the respective
#' data within those keys, and populate soil_WISE.csv and soil_Layers.csv with that data.
#' @details \preformatted{1) Create and set a directory to store the SSURGO data
#' 2) Download each site
#' 3) Extract each site into spatial and tabular data
#' 4) Choose a cokey, mukey, and the respective chkeys
#' 5) Extract the necessary data fields from the tabular data
#' 6) Populate SWRuns_InputData_soils_WISE and SWRuns_InputData_SoilLayers
#'    a) If data is incomplete, fill it with STATSGO data}
#' @export
download_and_extract_ssurgo <- function() {
  
  # The standard error message for the steps in this function.
  # This function will also flag STATSGO and insert an NA row with the correct label.
  error_warning_msg <- function(label, lat, lon) {
    cat("\n        > Error")
    cat(paste("\n             > Coordinates (", lat, ", ", lon, ") failed.", sep=""))
    cat(paste("\n             > Please check whether or not SSURGO supports the coordinates."))
    cat(paste("\n             > STATSGO data will be extracted for this site once the other sites are finished.\n\n"))
    flag_statsgo(label)
    return(NULL)
  }
  
  ############
  # Pre-steps
  ############
  # Require the FedData library
  require(FedData)
  # Grab settings
  coordinates <- SWRunInformation[runIDs_sites, c("X_WGS84", "Y_WGS84")]
  # Set directory
  create_and_set_directory(SSURGO.Directory, SSURGO.Redo)
  
  ###########################################
  # Download, format, and populate each site
  ###########################################
  for (i in 1:nrow(coordinates)) {
    # Get coordinates
    lat   <- coordinates[i, 2]
    lon   <- coordinates[i, 1]
    # Get site name
    label <- SWRunInformation[i, "Label"]
    # Create a spatial polygon object to serve as an area to download
    site  <- convert_coords_to_bounding_box(lat, lon)
    
    ###########
    # Download
    ###########
    # Print out step information
    cat(paste("Site", i, "of", nrow(coordinates)))  # Example: Site 1 of 2
    cat("\n    > Downloading SSURGO data...")
    # Get the NRCS SSURGO data (USA ONLY)
    soil_data <- tryCatch({ get_ssurgo(template = site, label = paste("SOILDATA-", lat, "-", lon, sep=""))},  # Download to a folder: "SOILDATA-[LAT]-[LON]"
                          error   = function(e) { error_warning_msg(label, lat, lon) },
                          warning = function(w) { error_warning_msg(label, lat, lon) })
    # Check if FedData was able to download and extract the files
    if (is.null(soil_data)) next
    
    ##############
    # Choose keys
    ##############
    # Print out step information
    cat("\n    > Choosing keys...")
    # Call function
    keys <- tryCatch({ choose_keys(soil_data)},
                     error   = function(e) { error_warning_msg(label, lat, lon) },
                     warning = function(w) { error_warning_msg(label, lat, lon) })
    # Check if enough keys were chosen, if not skip site
    if (length(keys) <=1) next
    
    #################################
    # Extract only the needed fields
    #################################
    cat("\n    > Extracting needed data from CSV's...")
    extracted_soil_data <- extract_and_format_soil_data(soil_data, keys)
    
    #########################################
    # Check if the extraction was successful
    #########################################
    # Do any horizons exist?
    if (length(extracted_soil_data$hzdepb.r) == 0) {
      cat("\n        > No horizons exist for this site; will fill with STATSGO\n\n")
      flag_statsgo(label)
      next
    }
    # Are the horizons filled with NA?
    skip <- FALSE
    for (i in 1:length(extracted_soil_data$sandtotal.r)) {
      if (is.na(extracted_soil_data$sandtotal.r) && is.na(extracted_soil_data$claytotal.r) && is.na(extracted_soil_data$silttotal.r)) {
        cat("\n        > All of the horizons lacked data; will fill with STATSGO\n\n")
        flag_statsgo(label)
        skip <- TRUE
        break
      }
    }
    if (skip) next
    
    ################
    # Convert units
    ################
    extracted_soil_data <- convert_units(extracted_soil_data)
    
    #####################################
    # Populate CSVs and global variables
    #####################################
    cat("\n    > Writing to CSV's and globals...")
    populate_csv_files(extracted_soil_data, label)
    
    ##################
    # Finishing steps
    ##################
    # Update global variables (for Input Master)
    did_extract[[3]]              <<- TRUE
    sites_externalsoils_source[i] <<- "SSURGO_USA"
    cat("\n    > Done!\n\n")
  }
}


####################
# Helper functions
####################
is.not.null <- function(x) return(! is.null(x))
is.not.na   <- function(x) return(! is.na(x)) 

#' @title Fill the next row with NA
#' @description The next row in both the soil layers CSV and soil texture CSV will have a label inserted, and
#' the data filled with NA, so that other external extractions will be able to insert data there.
#' @param label The matching label for this site in Input Master
fill_row_with_NA <- function(label) {
  ############
  # Open CSVs
  ############
  soil         <- read.csv(file.path(dir.sw.dat, datafile.soils),  header = TRUE, stringsAsFactors = FALSE)
  soil_layers  <- read.csv(file.path(dir.in, datafile.soillayers), header = TRUE, stringsAsFactors = FALSE)
  
  #############
  # Add labels
  #############
  # [CSVs]
  soil[nrow(soil) + 1, "Label"]               <- label
  soil_layers[nrow(soil_layers) + 1, "Label"] <- label
  # [Globals]
  sw_input_soils[nrow(sw_input_soils) + 1, "Label"]           <<- label
  sw_input_soillayers[nrow(sw_input_soillayers) + 1, "Label"] <<- label
  
  ############
  # Save CSVs
  ############
  write.csv(file = file.path(dir.sw.dat, datafile.soils),  row.names = FALSE, soil)
  write.csv(file = file.path(dir.in, datafile.soillayers), row.names = FALSE, soil_layers)
}

#' @title Create and set directory
#' @description Attempt to create a new directory and then set the working directory to it.
#' @param directory The directory to create and set
#' @param force_redo Remove the directory, for use in re-downloading SSURGO data
#' @note Warnings are turned off because it is okay if the directory already exists.
create_and_set_directory <- function(directory, force_redo) {
  # Remove existing directory if user specified to do so
  if (force_redo && dir.exists(directory)) unlink(directory, recursive = TRUE)
  # Create new directory
  dir.create(directory, showWarnings = F, recursive = T)  
  # Set working directory
  setwd(directory)
}


##################
# Other functions
##################
#' @title Convert coordinates to a bounding box
#' @description Create a raster polygon to grab an area from SSURGO via coordinates
#' @note FedData cannot grab a single pair of coordinates
#' @param lat Lattitude (float)
#' @param lon Longitude (float)
#' @param s Size of the bounding box - default is .001
#' @return A raster object to serve as a bounding box
#' @export
convert_coords_to_bounding_box <- function(lat, lon, s = .001) {
  return(polygon_from_extent(raster::extent(lon, lon + s, lat, lat + s), proj4string = "+proj=longlat +datum=NAD83 +no_defs"))
}

#' @title Choose mukey, cokey(s), and chkey(s)
#' @description Choose the mukey with the largest summed component percent, the cokey with the largest individual component percent (within the mukey),
#' then grab all chkeys that match the cokey.
#' @param soil_data A named list of length 2:\preformatted{
#'                  (1) "spatial": A SpatialPolygonsDataFrame of soil mapunits in the template.
#'                  (2) "tabular": A named list of data.frame's with the SSURGO tabular data}
#' @return list consisting of the mukey, cokey, and chkey(s)
#' @export
choose_keys <- function(soil_data) {
  
  ############################################################
  # Return the mukey with the largest total component percent
  #     > Could be replaced with an apply statement
  ############################################################
  get_mukey <- function(master) {
    mukey       <- master$mukey[1]
    largest_sum <- 0
    s           <- 0
    for (i in 1:nrow(master)) {
      if (master$mukey[i] != mukey || i == nrow(master)) {  # Change of mukey or end of the last mukey
        if (s > largest_sum) {                              # This mukey has a larger total component percent
          largest_sum   <- s                                # Update the largest sum
          largest_mukey <- mukey                            # Update the largest mukey
        }
        mukey <- master$mukey[i]                            # Next mukey
        s     <- 0                                          # Reset sum
      }
      s <- s + master$comppct.r[i]  
    }
    return(largest_mukey)
  }
  
  #################################################################
  # Return the cokey with the largest individual component percent
  #     > Could be replaced with an apply statement
  #################################################################
  get_cokey <- function(master, mukey) {
    largest_pct <- 0
    cokey       <- FALSE
    for (i in 1:nrow(master)) {
      if (master$mukey[i] == mukey) {             # Only search within our chosen mukey
        if (master$comppct.r[i] > largest_pct) {  # New highest component percent 
          largest_pct <- master$comppct.r[i]
          cokey <- master$cokey[i]                # Save cokey
        }
      }
    }
    return(cokey)
  }
  
  
  # Create a data frame that contains component percent, mukey, cokey, and chkey.
  # 
  # Grabbing data:
  #    > Mukey, component percent, and cokey are grabbed from component
  #    > Cokey and chkey are grabbed from chorizon
  #    > Chkey is grabbed from chfrags. 
  #        (Fragvol is also grabbed, since at least two fields are needed to maintain a data frame)
  #
  # Joining data:
  #    > Component is joined with chorizon by cokey
  #    > The above data frame is joined with chfrags by chkey
  # 
  # Return a data frame with component percent, mukey, cokey, and chkey
  grab_data <- function(soil_data) {
    # COMPONENT
    fields         <- c('comppct.r', 'mukey', 'cokey')      # The fields to grab
    component      <- soil_data$tabular$component           # Grab the component table
    component_data <- component[, ][, fields]               # Grab all rows of the defined fields
    # CHORIZON
    fields         <- c('cokey', 'chkey')
    chorizon       <- soil_data$tabular$chorizon      
    chorizon_data  <- chorizon[, ][, fields]
    # CHFRAGS
    fields         <- c('fragvol.r', 'chkey')               # Two fields are necessary to maintain a data frame
    chfrags        <- soil_data$tabular$chfrags
    chfrags_data   <- chfrags[, ][, fields]
    # Create a master table containing all of the above fields, joined by cokey and chkey
    intermediate   <- merge(component_data, chorizon_data)  # Join by cokey
    master         <- merge(intermediate, chfrags_data)     # Join by chkey
  }
  
  ############################
  # Grab needed data and keys
  ############################
  # Create data frame with needed data
  master <- grab_data(soil_data)
  # Grab mukey and cokey
  chosen_mukey <- get_mukey(master)
  chosen_cokey <- get_cokey(master, chosen_mukey)
  
  #################################
  # Check that a cokey was grabbed
  #################################
  if (!chosen_cokey) {
    cat(paste("\n        > No cokey was chosen because there were none available for the mukey", chosen_mukey, ". Site will be skipped."))
    return(FALSE)
  }
  
  ##############
  # Grab chkeys
  ##############
  # Prepare to store chkeys
  corresponding_chkeys <- c()
  for (i in 1:nrow(master)) {
    # Extract current row from master
    cokey <- master$cokey[i]
    chkey <- master$chkey[i]
    # If the cokey of this row matches our chosen one, add the corresponding chkey
    if ((cokey == chosen_cokey) && (chkey %in% corresponding_chkeys == FALSE)) corresponding_chkeys <- c(corresponding_chkeys, chkey)
  }
  
  ##################
  # Finishing steps
  ##################
  # Since the mukey, cokey, and chkeys have unique patterns, there is no need to have an ordered data structure
  return(c(chosen_mukey, chosen_cokey, corresponding_chkeys))
}

#' @title Extract and format soil data
#' @description Extract the needed data fields from the input, but only if the data fields match the chosen keys.
#' Some aspects of this functon are not neccessary, as data could be extracted and immediately populated into the csv, 
#' but storing the data in variables makes for better organization and troubleshooting.
#' @details \preformatted{The following columns of data will be extracted:
#'    > chorizon
#'        > sandtotal.r (percent)
#'        > claytotal.r (percent)
#'        > silttotal.r (percent)
#'        > dbthirdbar.r (g/cm^3)
#'        > hzdepb.r (cm)
#'        > hzname (string)
#'    > chfrags
#'        > fragvol.r (mm)
#'    > muaggatt
#'        > brockdepmin (cm)
#'    > component
#'        > comppct.r
#'
#' To see how keys are chosen, see function choose_keys}
#' @param soil_data A named list of length 2:\preformatted{
#'                  (1) "spatial": A SpatialPolygonsDataFrame of soil mapunits in the template.
#'                  (2) "tabular": A named list of data.frame's with the SSURGO tabular data}
#' @param keys dataframe consisting of the mukey, cokey, and chkeys
#' @note Only tabular is needed, but spatial is usually bundled with tabular
#' @return matrix containing the fields from chorizon, chfrags, and muaggat
#' @export
extract_and_format_soil_data <- function(soil_data, keys) {
  
  ###############
  # Extract data
  ###############
  fields            <- c('sandtotal.r', 'claytotal.r', 'silttotal.r', 'dbthirdbar.r','hzdepb.r', 'chkey')  # The fields to grab
  chorizon          <- soil_data$tabular$chorizon       # Grab the chorizon table
  rows              <- chorizon$cokey %in% keys         # Grab the rows with a matching chkey
  chorizon_data     <- chorizon[rows, ][, fields]       # Grab the correct fields
  fields            <- c('brockdepmin', 'mukey')
  muaggatt          <- soil_data$tabular$muaggatt
  rows              <- muaggatt$mukey %in% keys
  muaggatt_data     <- muaggatt[rows, ][, fields]
  fields            <- c('fragvol.r', 'chkey') 
  chfrags           <- soil_data$tabular$chfrags
  rows              <- chfrags$chkey %in% keys
  chfrags_data      <- chfrags[rows, ][, fields]
  fields            <- c('comppct.r', 'cokey')
  component         <- soil_data$tabular$component
  rows              <- component$cokey %in% keys
  component_data    <- component[rows, ][, fields]
  
  ########
  # Merge
  ########
  # Merge chfrags and chorizon
  horizon_frags     <- merge(chorizon_data, chfrags_data, all = TRUE)
  horizon_frags     <- as.data.frame(horizon_frags)
  
  #########
  # Modify
  #########
  # Mean fragvol.r
  m_horizon_frags   <- aggregate(horizon_frags[, -1], list(chkey = horizon_frags$chkey), mean)
  # Sort by horizon depth
  s_m_horizon_frags <- m_horizon_frags[order(m_horizon_frags[, 6]), ]
  # Finish
  return(c(s_m_horizon_frags, component_data, muaggatt_data))
}

#' @title Populate the CSVs
#' @description Take the given input data and populate the respective CSV's for the SOILWAT R Wrapper
#' @param formatted_data see Value of extract_and_format_soil_data
populate_csv_files <- function(formatted_data, label) {
  
  ###################
  # Helper functions
  ###################
  # Set the first row to a 1 and the given row to the given value (useful for soils_WISE CSVs)
  flag_and_fill <- function(CSV, column_name, row, value) {
    CSV[1, column_name]   <- 1      # Set flag
    CSV[row, column_name] <- value  # Set value 
    return(CSV)
  }
  
  update_input_use <- function(column, value) {
    if (is.not.na(value)) {
      sw_input_soils_use[column] <<- TRUE
    }
  }
  
  update_soil_texture <- function(row, column, value) {
    if (is.not.na(value)) {
      sw_input_soils[row, column] <<- value
    }
  }
  
  ###############
  # Extract data
  ###############
  sand         <- formatted_data$sandtotal.r
  clay         <- formatted_data$claytotal.r
  silt         <- formatted_data$silttotal.r
  dbthirdbar   <- formatted_data$dbthirdbar.r
  hzdepb       <- formatted_data$hzdepb.r
  brockdepmin  <- formatted_data$brockdepmin
  gravel       <- formatted_data$fragvol.r
  column_names <- data.frame(sand="Sand_L", clay="Clay_L", matrix="Matricd_L", depth="depth_L", gravel="GravelContent_L")
  failures     <- 0
  # Read CSV's
  soil         <- read.csv(file.path(dir.sw.dat, datafile.soils),  header = TRUE, stringsAsFactors = FALSE)
  soil_layers  <- read.csv(file.path(dir.in, datafile.soillayers), header = TRUE, stringsAsFactors = FALSE)
  
  ########################################################
  # Insert initial variables to CSVs and global variables
  ########################################################
  # Insert site names in CSVS
  soil[nrow(soil) + 1, "Label"]               <- label
  soil_layers[nrow(soil_layers) + 1, "Label"] <- label
  # Insert one-time data fields
  soil_layers[nrow(soil_layers), "SoilDepth_cm"] <- hzdepb[length(hzdepb)]
  # [Global variables] Create a new row in sw_input_soils
  temprow          <- matrix(c(rep.int(NA, length(sw_input_soils))), nrow=1, ncol=length(sw_input_soils))
  newrow           <- data.frame(temprow)
  colnames(newrow) <- colnames(sw_input_soils)
  sw_input_soils   <<- rbind(sw_input_soils, newrow)
  # [Global variables] Add info
  sw_input_soils[nrow(sw_input_soils), "Label"]                  <<- label
  sw_input_soillayers[nrow(sw_input_soillayers) + 1, "Label"]    <<- label  # First modification has to be at the (0 + 1) row, all other mods must be at current (1st) row
  sw_input_soillayers[nrow(sw_input_soillayers), "SoilDepth_cm"] <<- hzdepb[length(hzdepb)]
  
  #################################
  # Create a dummy layer if needed
  #################################
  dummy <- 0
  if (hzdepb[1] > 15) {
    # We will duplicate the first layer's data into SOILS_WISE, set the first layer depth to 15, THEN insert this site
    cat("\n        > First layer exceeds 15cm; creating a dummy layer")
    dummy <- 1
    # Fill CSV in with sand, clay, and matrix data
    soil <- flag_and_fill(CSV = soil, column_name = paste(column_names$sand, 1, sep = ""),   row = nrow(soil), value = sand[1])
    soil <- flag_and_fill(CSV = soil, column_name = paste(column_names$clay, 1, sep = ""),   row = nrow(soil), value = clay[1])
    soil <- flag_and_fill(CSV = soil, column_name = paste(column_names$matrix, 1, sep = ""), row = nrow(soil), value = dbthirdbar[1])
    soil <- flag_and_fill(CSV = soil, column_name = paste(column_names$gravel, 1, sep = ""), row = nrow(soil), value = gravel[1])
    soil_layers[nrow(soil_layers), paste(column_names$depth, 1, sep = "")] <- 15
    # [Global variables] Update info
    update_input_use(paste(column_names$sand, 1, sep = ""), sand[1])
    update_input_use(paste(column_names$clay, 1, sep = ""), clay[1])
    update_input_use(paste(column_names$matrix, 1, sep = ""), dbthirdbar[1])
    update_input_use(paste(column_names$gravel, 1, sep = ""), gravel[1])
    update_soil_texture(nrow(sw_input_soils), paste(column_names$sand, 1, sep=""), sand[1])
    update_soil_texture(nrow(sw_input_soils), paste(column_names$clay, 1, sep=""), clay[1])
    update_soil_texture(nrow(sw_input_soils), paste(column_names$matrix, 1, sep=""), dbthirdbar[1])  
    update_soil_texture(nrow(sw_input_soils), paste(column_names$gravel, 1, sep=""), gravel[1])  
    sw_input_soillayers[nrow(sw_input_soillayers), paste(column_names$depth, 1, sep="")] <<- 15  # Set the depth for this layer
  }
  
  ############################
  # Insert incremented fields
  ############################
  for (j in 1:length(hzdepb)) {
    k <- j + dummy  # If a dummy layer was created, we want use the CURRENT layer's data but insert it into the NEXT layer
    # Don't record any information if the following fields are empty (because other data is likely also incomplete)
    if (is.na(sand[j]) && is.na(clay[j]) && is.na(silt[j]) && is.na(dbthirdbar[j])) {
      # Discard the last layer and update the max soil depth
      cat("\n        > Soil texture data incomplete; a layer has been discarded")
      soil_layers[nrow(soil_layers), "SoilDepth_cm"]                 <-  hzdepb[j - 1]
      sw_input_soillayers[nrow(sw_input_soillayers), "SoilDepth_cm"] <<- hzdepb[j - 1]
      # Increase the number of failed layers
      failures <- failures + 1
      next
    }
    # Fill in variables with sand, clay, matrix, and gravel data
    # [CSVs]
    soil <- flag_and_fill(CSV = soil, column_name = paste(column_names$sand, k, sep = ""),   row = nrow(soil), value = sand[j])
    soil <- flag_and_fill(CSV = soil, column_name = paste(column_names$clay, k, sep = ""),   row = nrow(soil), value = clay[j])
    soil <- flag_and_fill(CSV = soil, column_name = paste(column_names$matrix, k, sep = ""), row = nrow(soil), value = dbthirdbar[j])
    soil <- flag_and_fill(CSV = soil, column_name = paste(column_names$gravel, k, sep = ""), row = nrow(soil), value = gravel[j])
    soil_layers[nrow(soil_layers), paste(column_names$depth, k, sep = "")] <- hzdepb[j]
    # [Globals]
    update_input_use(paste(column_names$sand, k, sep = ""), sand[j])
    update_input_use(paste(column_names$clay, k, sep = ""), clay[j])
    update_input_use(paste(column_names$matrix, k, sep = ""), dbthirdbar[j])
    update_input_use(paste(column_names$gravel, k, sep = ""), gravel[j])
    update_soil_texture(nrow(sw_input_soils), paste(column_names$sand, k, sep=""), sand[j])
    update_soil_texture(nrow(sw_input_soils), paste(column_names$clay, k, sep=""), clay[j])
    update_soil_texture(nrow(sw_input_soils), paste(column_names$matrix, k, sep=""), dbthirdbar[j])  
    update_soil_texture(nrow(sw_input_soils), paste(column_names$gravel, k, sep=""), gravel[j])  
    sw_input_soillayers[nrow(sw_input_soillayers), paste(column_names$depth, k, sep="")] <<- hzdepb[j]  # Set the depth for this layer
  }
  
  #######################
  # Check if site failed
  #######################
  if(failures == length(hzdepb)) {  # All layers failed
    cat("\n        > All layers failed; will fill with STATSGO")
    flag_statsgo(label)
  }
  
  #############
  # Save files
  #############
  write.csv(file = file.path(dir.sw.dat, datafile.soils), row.names = FALSE, soil)
  write.csv(file = file.path(dir.in, datafile.soillayers), row.names = FALSE, soil_layers)
}

#' @title Convert SSURGO units to our units
#' @details \preformatted{
#'                         | SSURGO  |     Ours     | Conversion
#' > chorizon
#'        > sandtotal.r    | percent |   fraction   | divide by 100
#'        > claytotal.r    | percent |   fraction   | divide by 100
#'        > silttotal.r    | percent |   fraction   | divide by 100
#'        > dbthirdbar.r   | g/cm^3  |   Mg/m^3     | multiply by 10^9
#'        > hzdepb.r       |   cm    |      cm      | none
#'        > hzname         | string  |   string     | none
#' > chfrags
#'        > fragvol.r      | percent |   fraction   | divide by 100
#' > muaggatt
#'        > brockdepmin    |   cm    |      cm      | none}
#' @note Units retreived from SSURGO Metadata: 
#'       http://www.nrcs.usda.gov/wps/PA_NRCSConsumption/download?cid=stelprdb1241114&ext=pdf
#' @return matrix containing the fields from chorizon, chfrags, and muaggat
#' @export
convert_units <- function(formatted_data) {
  for (i in 1:length(formatted_data$chkey)) {  # The length of any field will work
    # Convert percents to fractions
    formatted_data$sandtotal.r[i] <- formatted_data$sandtotal.r[i] / 100
    formatted_data$silttotal.r[i] <- formatted_data$silttotal.r[i] / 100
    formatted_data$claytotal.r[i] <- formatted_data$claytotal.r[i] / 100
    formatted_data$fragvol.r[i]   <- formatted_data$fragvol.r[i]   / 100
    # Convert g/cm^3 to mg/m^3
    formatted_data$dbthirdbar[i]  <- formatted_data$dbthirdbar[i] * 10^9
  }
  return(formatted_data)
}

#' @title Fill CSVs with STATSGO data
#' @description Flag a global variable that will allow STATSGO data to be extracted within part 3 of 5
#' @param label The matching label for this site in Input Master
#' @note flag_statsgo and fill_row_with_NA are written as separate functions for lower coupling, 
#'       but fill_row_with_NA is called within this function to reduce unneeded function calls.
#' @export
flag_statsgo <- function(label) {
  exinfo$ExtractSoilDataFromCONUSSOILFromSTATSGO_USA <<- TRUE
  fill_row_with_NA(label)
}

#####################
# Call main function
#####################
download_and_extract_ssurgo()
