#dcast breaks a column into more columns according to the values it has

data(ChickWeight)
library(reshape2)
head(ChickWeight)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
head(wideCW)