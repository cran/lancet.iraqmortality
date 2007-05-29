################################################################################
##
## prepdata.R -- Private utility functions for loading the raw Lancet
## study data.
##
################################################################################

.onLoad <- function(lib, pkg){

  ## Display a message if the restricted data archive is not present.
  
  if(!.prepdata(test.archive.present = TRUE)){
    cat(.noarchive.message)
  }
}

.noarchive.message <-
  paste("lancet.iraqmortality warning: No data archive found.\n\n",
        "To use functions prep.deaths and prep.houses or to build the vignette,\n",
        "place the restricted dataset archive with name mortality.zip in your\n",
        "working directory.\n", sep = "")

## .prepdata processes the raw data contained in dBase (dbf) files in
## the restricted data archive, and takes the following parameters:
##
## type: "houses" or "deaths" to specify desired sub-dataset.
##
## in.loc: location of the data archive (in.file).  By default, the
## package searches its own data/ directory, but also checks the
## current working directory for in.file (see .archive.path).
##
## in.file: filename of the data archive.  Defaults to
## "mortality.zip", the name for the archive required by a user who
## isn't calling this function directly.
##
## test.archive.present: Causes the function to only check whether the
## archive is present in either in.loc or the current working
## directory.  If set to TRUE, the function will return a logical
## vector.
##
## unzip: Specifies unzip program.  The function will stop if this is
## set to "internal" or if any error is encountered running the unzip
## program.

.prepdata <- function(type,
                      in.loc  = system.file("data", package="lancet.iraqmortality"),
                      in.file = "mortality.zip",
                      test.archive.present = FALSE,
                      unzip = getOption("unzip")){

  require(foreign)
  
  zippath <- .archive.path(in.loc, in.file)
  
  if(test.archive.present){
    return(!is.null(zippath))
  }

  if(is.null(zippath)){
    stop(.noarchive.message)
  }

  ## "internal" is not supported as unzip program.
  
  if(unzip == "internal"){
    stop(paste("No unzip program detected.  Please supply path",
               "to unzip command as 'unzip' parameter."))
  }

  ## Unzip the archive into a subdirectoy "lancet" of the current
  ## session's temp directory.  This will be deleted later.
  
  tmpdir <- file.path(tempdir(), "lancet")
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)

  ## Perform the unzip operation and stop if an error was encountered.
  
  rc <- system(paste(unzip, "-oq", zippath, "-d", tmpdir), ignore.stderr = TRUE)
  if(rc){

    ## The below message isn't very informational, but is cleaner than
    ## allowing ignore.stderr = TRUE as above.
    
    stop(paste("Unzip failed with return code", rc))
  }

  ## Construct paths to both input files and load them up using read.dbf.
  
  deaths.file <- file.path(tmpdir, "Iraq Mortality Public Distribution Files",
                           "Iraq 2006 deaths.dbf")
  houses.file <- file.path(tmpdir, "Iraq Mortality Public Distribution Files",
                           "Iraq 2006 households.dbf")
  
  deaths <- read.dbf(deaths.file)
  houses <- read.dbf(houses.file)

  ## Clean up.
  
  unlink(tmpdir, recursive = TRUE)

  ## Return the desired single sub-dataset.
  
  stopifnot(type %in% c("houses","deaths"))
  
  if(type == "houses") {
    invisible(houses)
  }
  
  else if (type == "deaths") {
    invisible(deaths)
  }
}

## Return path to zip archive.  We check for in.file in (1) in.loc and
## (2) the current working directory.

.archive.path <- function(in.loc, in.file){
  for(i in c(in.loc, ".")){
    path <- file.path(i, in.file)
    if(file.exists(path)){
      return(path)
    }
  }
  return(NULL)
}
