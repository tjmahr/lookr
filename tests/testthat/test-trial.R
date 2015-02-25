#
# Trial <- function(df, attrs) {
#   stopifnot(is.element(c("Time", "XMean", "YMean"), names(df)))
# }
# df <- data.frame(Time = 1:80, XMean = 11:90 / 100, YMean = 11:90 / 100)
# # df <- data.frame(Time = 1:99, XMean = seq(0.01, .99, .01), YMean = XMean)
# # df <- data.frame(Time = 1:99, XMean = seq(0.01, .99, .01))
#
# Trial(df, attrs = NA)
#
#
#
#
#
#
#
#
# library(zoo)
# trials <- Session("inst/tests/data/Coartic_WFFArea_2a/001P00XS1/")
# trials <- AdjustTimes(trials)
# trial <- trials[[1]]
# TimeSlice(TimeSlice(trial))
