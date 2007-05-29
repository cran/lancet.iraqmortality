prep.houses <- function(){

  ## Grab the raw data from the zip files. Perhaps prep.houses and
  ## prep.deaths should be one function?
  
  x <- .prepdata(type = "houses")

  ## Check that the raw data has the variable names that I
  ## expect. Note that the first version of the data which they sent
  ## me was different from this one.

  stopifnot(identical(
                      names(x),
                      c("HH_ID","GOVERN","CLUSTER","HH1_2002","HH2_2002",
                        "MID_INT1","MID_INT2","HH_SIZE","MALES","FEMALES","BIRTHS",
                        "DEATH_ANY","DEATHS","DEATHS_V","DEATHS_NV","IN_MIG","OUT_MIG")
                      )
            )
  
  ## Variable renaming and organization.

  names(x) <- c("id", "governorate", "cluster","size.2002", "size.2002.mig",
                "mid.2002", "mid.2002.mig", "size", "males", "females", "births",
                "has.death", "deaths", "deaths.violent", "deaths.nonviolent", "immigration", "emigration")

  
  x$governorate <- factor(x$governorate, labels = c("Anbar","Basrah","Qadissiya","Sulaymaniyah",
                                           "Babylon","Baghdad","Thi-Qar","Diyala","Erbil","Kerbala",
                                           "Tameem","Missan","Ninewa","Wassit","Najaf","Saleh Al-Din"))
  x$cluster     <- factor(x$cluster)
  x$has.death   <- factor(x$has.death, labels = c("no","yes"))

  ## Put things in a nice order and then return.

  x <- x[c("id", "governorate", "cluster", "size", "males", "females",
  "births", "deaths", "immigration", "emigration", "size.2002",
  "size.2002.mig", "mid.2002", "mid.2002.mig", "has.death", "deaths",
  "deaths.violent", "deaths.nonviolent")]


  return(x)
}
