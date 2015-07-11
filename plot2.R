# plot2
# usage:
#   source("plot2.R")
#   plot2("data.csv", "plot2.png")

plot2 <- function(datafile, pngfile) {

    # to suggest the data structure , read head 100 line
    sampling <- read.table(
        datafile,
        sep=";",
        stringsAsFactor=FALSE,
        header = TRUE,
        nrows=100,
        na.strings="?")

    # suggest structure
    classes <- sapply(sampling, class)

    # read all
    dfrm <- read.table(
        datafile,
        sep=";",
        stringsAsFactor=FALSE,
        header = TRUE,
        colClasses=classes,
        na.strings="?")

    # subset Date is "1/2/2007" or "2/2/2007"
    dfrm <- subset(
        dfrm,
        subset=(Date == "1/2/2007" | Date == "2/2/2007"))

    # convert from datestring, timestring to datetime
    date_frame <- as.data.frame(
        strptime(
            paste(dfrm$Date, dfrm$Time),
            "%d/%m/%Y %H:%M:%S")
        )
    colnames(date_frame) <- c("Datetime")
    dfrm <- cbind(dfrm,date_frame)

    # create png file
    png(pngfile, width = 480, height = 480)

    # global parameter backup
    oldmai = par("mai")
    oldps = par("ps")
    oldmar = par("mar")
    oldlwd = par("lwd")
    oldlocale = Sys.getlocale("LC_TIME")

    # set locale(for multi locale)
    Sys.setlocale("LC_TIME","en_US")

    par(mai=c(0.8,0.8,0.2,0.2))
    par(ps=12)
    par(mar=c(4,4,1,1))
    par(lwd=1)
    plot(
        dfrm$Datetime,
        dfrm$Global_active_power,
        type="l",
        ylab="Global Active Power (kilowatts)",
        xlab="")

    # global parameter restore
    par(mai=oldmai)
    par(ps=oldps)
    par(mar=oldmar)
    par(lwd=oldlwd)

    Sys.setlocale("LC_TIME",oldlocale)

    # png file close
    dev.off()

    TRUE
}
