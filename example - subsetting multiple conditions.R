subsetin example

c<-subset(d,d$Date>="2007-02-01" & d$Date<="2007-02-02")
igual a 
b<-d[d$Date>="2007-02-01" & d$Date<="2007-02-02",]