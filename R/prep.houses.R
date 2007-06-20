prep.houses <- function(){

  ## Grab the raw data from the zip files. Perhaps prep.houses and
  ## prep.deaths should be one function?
  
  x <- .prepdata(type = "houses")

  ## Check that the raw data has the variable names that I
  ## expect. Note that the first version of the data which they sent
  ## me was different from this one.

  expect.cols <- c("HH_ID","GOVERN","CLUSTER","HH1_2002",
                   "MID_INT1","HH_SIZE","MALES","FEMALES","BIRTHS",
                   "DEATH_ANY","DEATHS","DEATHS_V","DEATHS_NV","IN_MIG","OUT_MIG")

  ## 2007-06-19 JDE: The order of columns has changed, and HH2_2002
  ## and MID_INT2 have been dropped.  Make the name mapping of the
  ## remaining columns the same as before by adjusting the new column
  ## order to look like the old.  First check to see if any columns
  ## omitted.
  
  stopifnot(all(expect.cols %in% names(x)))

  x <- x[expect.cols]
  
  stopifnot(identical(names(x), expect.cols))
  
  ## Variable renaming and organization.

  ## 2007-06-19 JDE: From the below I have removed "size.2002.mig",
  ## which was "HH2_2002", and "mid.2002.mig", which was "MID_INT2".
  
  names(x) <- c("id", "governorate", "cluster","size.2002",
                "mid.2002", "size", "males", "females", "births",
                "has.death", "deaths", "deaths.violent", "deaths.nonviolent", "immigration", "emigration")

  
  x$governorate <- factor(x$governorate, labels = c("Anbar","Basrah","Qadissiya","Sulaymaniyah",
                                           "Babylon","Baghdad","Thi-Qar","Diyala","Erbil","Kerbala",
                                           "Tameem","Missan","Ninewa","Wassit","Najaf","Saleh Al-Din"))
  x$cluster     <- factor(x$cluster)
  x$has.death   <- factor(x$has.death, labels = c("no","yes"))

  ## There were 6 mistakes in the consistency between size and
  ## size.2002 in the first version of the data. Shannon Doocy
  ## corrected 5 of them by hand but missed one. We have corrected it
  ## here. Looks like size and size.2002 just needed to be swapped.

  x[x$id == 1890, c("size", "size.2002")] <- c(11, 10)
  
  ## Put things in a nice order and then return.

  x <- x[c("id", "governorate", "cluster", "size", "males", "females",
           "births", "deaths", "immigration", "emigration", "size.2002",
           "mid.2002", "has.death", "deaths",
           "deaths.violent", "deaths.nonviolent")]


  return(x)
}
