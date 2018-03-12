

daylength = function (lat, doy) 
{
    if (class(doy) == "Date" | class(doy) == "character") {
        doy <- as.character(doy)
        doy <- as.numeric(format(as.Date(doy), "%j"))
    }
    else {
        doy <- (doy - 1)%%365 + 1
    }
    lat[lat > 90 | lat < -90] <- NA
    P <- asin(0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * 
        tan(0.0086 * (doy - 186)))))
    a <- (sin(0.8333 * pi/180) + sin(lat * pi/180) * sin(P))/(cos(lat * 
        pi/180) * cos(P))
    a <- pmin(pmax(a, -1), 1)
    DL <- 24 - (24/pi) * acos(a)
    return(DL)
}