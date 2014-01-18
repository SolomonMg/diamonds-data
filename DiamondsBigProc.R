# TODO: Add comment
# 
# Author: solomon
###############################################################################

# Change as appropriate
setwd("~/Dropbox/public/Blogposts/Diamonds")

dat = read.csv("data/diamonds0.csv", as.is=TRUE)
for(i in 1:208){
  this_file_name = paste("data/diamonds", i, ".csv", sep="")
  this_file = read.csv(this_file_name, as.is=TRUE)
  dat = rbind(dat, this_file)
}

#save(dat, file="data/BigDiamonds.Rda")

# Fix price:
dat$price = gsub("\\$", "", dat$price)
dat$price = gsub(",", "", dat$price)
dat$price = as.numeric(dat$price)

library('stringr')

regx = "(\\d+(\\.\\d+)?)\\s*[x\\-]\\s*(\\d+(\\.\\d+)?)\\s*x\\s*(\\d+(\\.\\d+)?)"
dat$x = str_replace(dat$measurements, regx, "\\1")
dat$y = str_replace(dat$measurements, regx, "\\3")
dat$z = str_replace(dat$measurements, regx, "\\5")
dat$x = as.numeric(dat$x)
dat$y = as.numeric(dat$y)
dat$z = as.numeric(dat$z)
summary(dat$x)
summary(dat$y)
summary(dat$z)

# make factor levels:
dat$cut[dat$cut=="H&A;"] = "Ideal"
dat$cut = factor(dat$cut, levels = c("Good", "V.Good", "Ideal"))
#summary(dat$cut)

table(dat$color)
dat$color = factor(dat$color)
dat$color = factor(dat$color, levels = rev(levels(dat$color)))

table(dat$clarity)
dat$clarity[dat$clarity=="FL"] = "IF"
clrty = c("IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2", "I1", "I2", "I3")
dat$clarity = factor(dat$clarity, levels = clrty)


diamonds = dat

# WOW xyz is screwed up.
hist(diamonds$y[diamonds$y<20])
diamonds$y[diamonds$y>20] = NA
diamonds$y[diamonds$y==0] = NA
diamonds$x[diamonds$x>20] = NA
diamonds$x[diamonds$x==0] = NA
diamonds$z[diamonds$z>20] = NA
diamonds$z[diamonds$z==0] = NA

#table(diamonds$x > diamonds$z)
#table(diamonds$x > diamonds$y)
#table(diamonds$y > diamonds$z)

# really things should be arranged like this:
#x > y > z
#xyz = diamonds[9:11]
#XYZ = adply(xyz, 1, transform, 
#    X = sort(c(x,y,z))[3],
#    Y = sort(c(x,y,z))[2],
#    Z = sort(c(x,y,z))[1])
#diamonds[9:11] = XYZ

diamonds$price[diamonds$price<300] = NA

diamonds$cert = factor(diamonds$cert, 
    levels = c("GIA", "IGI", "EGL", "EGL USA", "EGL Intl.", "EGL ISRAEL", 
        "HRD", "AGS", "OTHER"))
diamonds$cert[is.na(diamonds$cert)] = "OTHER"

diamonds$color = factor(diamonds$color, levels = rev(levels(diamonds$color)))
diamonds$clarity = factor(diamonds$clarity, levels = rev(levels(diamonds$clarity)))

diamondsbig = diamonds

save(diamondsbig, file="data/BigDiamonds.Rda")









