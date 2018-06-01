# ==================
# read.aqs function
# ==================


#' Read in and format raw data transactions from Air Quality Systems
#'
#'
#' Reads in a .txt file of raw data transactions with the option for specifying
#' different levels of formatting.
#'
#'
#' @param filename A character string of the name of the raw data file. If the
#'              file is not in the working directory, the file path must be
#'              specified in this argument (see example below).
#'
#' @param level An integer between 0 and 4, specifying the level of simplification
#'              and formatting to be applied. Default is level 2.
#'              \itemize{
#'                 \item \strong{Level 0} simply returns the file as a data frame.
#'                 \item \strong{Level 1} converts the Date and Start.Time columns
#'                       to a single Date.Time column of class POSIXct. Drops: Date,
#'                       Start.Time, RD, Action.Code, Null.Data.Code, Sampling.Frequency,
#'                       Monitor.Protocol..MP..ID, Alternate.Method.Detectable.Limit,
#'                       Uncertainty, and Qualifier columns.
#'                 \item \strong{Level 2} concatenates all monitor ID columns (State.Code,
#'                       County.Code, Site.ID, Parameter, POC, Sample.Duration, Unit,
#'                       Method), separated by a "-". Returns only Date.Time, concatenated
#'                       monitor IDs and Sample.Value columns.
#'                 \item \strong{Level 3} applies names to each monitor ID. Example:
#'                       monitor ID "06-079-0005-44201-1-1-8-87" becomes
#'                       "CA-San_Luis_Obispo-0005-ozone-1-1_hour-ppb-model_400_ozone_analyzer".
#'                       Monitor names used in this function are available
#'                       \href{https://www.epa.gov/aqs/aqs-code-list}{here} on the AQS site.
#'                       This level returns only Date.Time, named monitor IDs and
#'                       Sample.Value columns.
#'                 \item \strong{Level 4} converts level 3 data to wide format with
#'                       Date.Time and named monitor IDs as the columns, and Sample.Value
#'                       as the values in each row.
#'    }
#' @param time.zone A character string specifying the time zone to be used when
#'              date values are converted to POSIXct class in level 1. Default
#'              is set to UTC. See as.POSIXct documentation for more time zone options.
#'
#' @param remove A logical value indicating if columns which contain only a
#'              single value should be dropped. Default is set to FALSE. If TRUE, this
#'              option will be applied at all levels.
#'
#'
#' @return Returns a data frame.
#'
#'
#' @examples
#' aqs <- read.aqs(filename = "Desktop/AMP501_1595753-0.txt",
#'                 level = 4,
#'                 remove = TRUE)
#'
#'
#' @export


read.aqs <- function(filename,
                     level = 2,
                     time.zone = "UTC",
                     remove = FALSE) {

  # LEVEL 0 ----------------------------------------

  data <- read.table(file = filename, # read in data
                     sep = "|",
                     header = TRUE,
                     colClasses = c(rep("character", 12),
                                    "numeric",
                                    rep("character", 13)))

  state.code <- data$State.Code # used in level 3

  if (remove == TRUE) { # if remove = TRUE, drop the columns that contain all the same value

    rcol <- rep(0, ncol(data))

    for (i in 1:ncol(data)) {

      if (length(unique(data[ , i])) == 1) {

        rcol[i] <- 1

      }

    }

    data <- data[ , -which((rcol == 1) & (colnames(data) != "Date"))] # keep Date column

  }

  if (level == 0) {

    attr(data, "level") <- "level 0"

    return(data)

  }

  # LEVEL 1 ----------------------------------------

  data$Date.Time <- as.POSIXct(paste(paste(substr(data$Date, 1, 4),
                                           substr(data$Date, 5, 6),
                                           substr(data$Date, 7, 8),
                                           sep = "-"),
                                     data$Start.Time, sep = " "),
                               format = "%Y-%m-%d %H:%M",
                               tz = time.zone)

  data <- data[ , -c((which(colnames(data) %in% c("Date", # drop unecessary columns
                                                  "Start.Time",
                                                  "RD",
                                                  "Action.Code",
                                                  "Null.Data.Code",
                                                  "Sampling.Frequency",
                                                  "Monitor.Protocol..MP..ID",
                                                  "Alternate.Method.Detectable.Limit",
                                                  "Uncertainty"))),
                     (which(startsWith(colnames(data), "Qualifier") == TRUE)))]


  if (level == 1) {

    attr(data, "level") <- "level 1"

    return(data)

  }

  # LEVEL 2 ----------------------------------------

  paste.args <- c(data[, -c(which(colnames(data) == "Sample.Value"), # concatenate labels
                            which(colnames(data) == "Date.Time"))],
                  sep="-")

  data$Monitor.ID <- do.call(paste, paste.args)

  if (level == 2) {

    data <- data[c("Date.Time", "Monitor.ID", "Sample.Value")]

    attr(data, "level") <- "level 2"

    return(data)

  }

  # LEVEL 3 ----------------------------------------

  to.use <- colnames(data)[!(colnames(data) %in% c("Site.ID",
                                                    "POC",
                                                    "Sample.Value",
                                                    "Date.Time",
                                                    "Monitor.ID"))]

  for (i in to.use) { # load only the label files needed

    if (i == "County.Code") {

      label.file <- County
      data$County.Code <- paste(state.code, data$County.Code, sep = "/")
      matches <- match(data$County.Code, label.file$Code)
      data$County.Code <- label.file$Region[matches]

    } else {

      label.file <- as.name(i)
      var.name <- paste(i, ".Name", sep = "")
      data[[i]] <- label.file[match(data[[i]], label.file[[i]]), 2]

    }
  }

  paste.args1 <- c(data[, -c(which(colnames(data) == "Sample.Value"),
                             which(colnames(data)  == "Date.Time"),
                             which(colnames(data)  == "Monitor.ID"))],
                   sep="-")

  data$Monitor.ID <- do.call(paste, paste.args1) # concatenate labels

  data <- data[, c("Date.Time", "Monitor.ID", "Sample.Value")]

  if (level == 3) {

    attr(data, "level") <- "level 3"

    return(data)

  }

  # LEVEL 4 ----------------------------------------


  data <- reshape2::dcast(data, # wide -> long format
                          Date.Time ~ Monitor.ID,
                          value.var = "Sample.Value",
                          na.rm = TRUE,
                          fill = 0)

  if (level == 4) {

    attr(data, "level") <- "level 4"
    return(data)

  }

}


# IN PROGRESS:
# - DESCRIPTION (package meta data)
# - Vignettes (long-form documentation)
# - Automated testing
