prep.deaths <- function(){

  ## Grab the initial data from the zip.
  
  x <- .prepdata(type = "deaths")

  ## Rename. Note that I worry about them inadvertantly changing the
  ## format/order in later updates.

  stopifnot(identical(
                      names(x),
                      c("HH_ID", "GOVERN", "CLUSTER", "DATE", "YEAR",
                        "IMPUTE", "INVASION", "SEX", "AGE_YRS", "CAUSE_S",
                        "CAUSE_16C", "CAUSE_D", "CERTIFICAT")
                      )
            )
  
  names(x) <- c("id", "governorate", "cluster", "date", "year",
                "impute", "invasion", "sex", "age", "cause.summary",
                "cause.category", "death.nature", "certificate")

  ## Provide specific values for the levels from the code book
  ## distributed with the zip file.
  
  x$governorate <- factor(x$governorate,
                          labels = c("Anbar","Basrah","Qadissiya","Sulaymaniyah",
                            "Babylon","Baghdad","Thi-Qar","Diyala","Erbil","Kerbala",
                            "Tameem","Missan","Ninewa","Wassit","Najaf","Saleh Al-Din"))

  x$cluster       <- as.factor(x$cluster)
  x$sex           <- factor(x$sex, labels = c("male","female"))
  x$impute        <- ifelse(x$impute %in% c(1), TRUE, FALSE)
  x$invasion      <- factor(x$invasion, labels = c("pre","post"))
  x$cause.summary <- as.factor(x$cause.summary)
  x$death.nature  <- factor(x$death.nature, labels = c("violent","non-violent"))
  
  x$cause.category <- factor(x$cause.category, labels =
                             c("gunshot","carbomb","other explosion","air strike",
                               "violent, unknown","old age","accident","cancer",
                               "heart disease or stroke","chronic illness","infection disease",
                               "infant death","non-violent, other"))

  ## NA certificate means the interviewers forgot to ask for one.

  x$certificate[is.na(x$certificate)] <- 2
  
  x$certificate <- factor(x$certificate, labels = c("no","yes","forgot"))
  
  ## Add an age.group variable to go along with Table 2 in the paper
  
  x$age.group <- ifelse(x$age < 15, "child", NA)
  x$age.group <- ifelse(x$age >= 15 & x$age < 60, "adult", x$age.group)
  x$age.group <- ifelse(x$age >= 60, "elderly", x$age.group)
  x$age.group <- factor(x$age.group, levels = c("child","adult","elderly"))

  ## In a private communication, Shannon Doocy points out that there
  ## is a mistake in certificate for one observation. I correct it
  ## here. This makes the number of deaths with a certificate 501,
  ## consistent with the paper.

  x[x$id == 1346 & x$date == as.Date("2005-11-01"), "certificate"] <- "yes"

  ## Put everything in a convenient order.

  x <- x[c("id", "governorate", "cluster", "date", "year", "sex", "age", "age.group",
           "impute", "invasion",
           "cause.summary", "cause.category", "death.nature", "certificate")]

  
  return(x)
}
