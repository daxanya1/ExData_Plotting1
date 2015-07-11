# plot1
# usage:
#   source("plot1.R")
#   plot1("data.csv", "plot1.png")

plot1 <- function(datafile, pngfile) {

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

    # create png file
    png(pngfile, width = 480, height = 480)

    # global parameter backup
    oldmai = par("mai")
    oldps = par("ps")
    oldmar = par("mar")
    oldlwd = par("lwd")

    par(mai=c(0.8,0.8,0.2,0.2))
    par(ps=12)
    par(mar=c(4,4,1,1))
    par(lwd=1)
    hist(
        dfrm$Global_active_power,
        main="Global Active Power",
        xlab="Global Active Power (kilowatts)",
        ylab="Frequency",
        col="red")

    # global parameter restore
    par(mai=oldmai)
    par(ps=oldps)
    par(mar=oldmar)
    par(lwd=oldlwd)

    # png file close
    dev.off()

    TRUE
}
