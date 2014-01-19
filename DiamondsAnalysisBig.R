# TODO: Add comment
#
# Author: solomon
###############################################################################

#install.packages('RCurl')
library('RCurl')
diamondsurl = getBinaryURL("https://raw.github.com/solomonm/diamonds-data/master/BigDiamonds.Rda")
load(rawConnection(diamondsurl))

#diamondsbig = diamondsbig[diamondsbig$cert=="GIA",]

#install.packages('GGally')
#install.packages('ggplot2')
#install.packages('scales')
#install.packages('memisc')

library(ggplot2)
library(GGally)
library(scales)

diasamp = diamondsbig[sample(1:length(diamondsbig$price), 10000), 1:8]
png("plots/ggpairs.png", height=1000, width=1000)
ggpairs(na.omit(diasamp), params = c(shape = I("."), outlier.shape = I(".")))
dev.off()


p = qplot(price, data=diamondsbig, binwidth=0.01) +
		theme_bw() +
		ggtitle("Price")
p

ggsave("plots/price.png", height=5, width=5)


p = qplot(price, data=diamondsbig, binwidth = 0.01) +
		scale_x_log10() +
		theme_bw() +
		ggtitle("Price (log10)")
p

ggsave("plots/pricelog10.png", height=5, width=5)


p = qplot(carat, price, data=diamondsbig, xlim=c(0,3)) +
		theme_bw() +
		ggtitle("Price by Carat")
ggsave("plots/caratprice.png", height=5, width=5)


p = qplot(carat, price, data=diamondsbig) +
		scale_y_continuous(trans=log10_trans() ) +
		theme_bw() +
		ggtitle("Price (log10) by Cubed-Root of Carat")
p

ggsave("plots/caratpricelog10.png", height=5, width=5)


cubroot_trans = function() trans_new("cubroot", transform= function(x) x^(1/3), inverse = function(x) x^3 )

p = qplot(carat, price, data=diamondsbig) +
		scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
				breaks = c(350, 1000, 5000, 10000, 15000)) +
		theme_bw() +
		ggtitle("Price (log10) by Cubed-Root of Carat")
p

ggsave("plots/crcaratpricelog10.png", height=5, width=5)


head(sort(table(diamondsbig$carat), decreasing=T ))
head(sort(table(diamondsbig$price), decreasing=T ))



p = ggplot( data=diamondsbig, aes(carat, price)) +
		geom_point(alpha = 0.5, size = .75, position="jitter") +
		scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
				breaks = c(350, 1000, 5000, 10000, 15000)) +
		theme_bw() +
		ggtitle("Price (log10) by Cubed-Root of Carat")
p

ggsave("plots/crcaratpricejitterlog10.png", height=5, width=6)



p = ggplot( data=diamondsbig, aes(carat, price, colour=cert)) +
		geom_point(alpha = 0.5, size = .75, position="jitter") +
    scale_colour_brewer(type = "div",
        guide = guide_legend(title = NULL, reverse=T,
            override.aes = list(alpha = 1))) +
    scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
				breaks = c(350, 1000, 5000, 10000, 15000)) +
		theme_bw() + theme(legend.key = element_blank()) +
		ggtitle("Price (log10) by Cubed-Root of Carat and Color")
p


ggsave("plots/crcaratpricecertlog10.png", height=5, width=6)


p = ggplot( data=diamondsbig, aes(carat, price, colour=clarity)) +
		geom_point(alpha = 0.5, size = .75, position="jitter") +
		scale_colour_brewer(type = "div",
				guide = guide_legend(title = NULL, reverse=T,
						override.aes = list(alpha = 1))) +
		scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
				breaks = c(350, 1000, 5000, 10000, 15000)) +
		theme_bw() + theme(legend.key = element_blank()) +
		ggtitle("Price (log10) by Cubed-Root of Carat and Color")
p


ggsave("plots/crcaratpriceclairtylog10.png", height=5, width=6)


#p = ggplot( data=diamondsbig, aes(carat, price, colour=clarity)) +
#		geom_point(alpha = 0.5, size = .75, position="jitter") +
#		geom_smooth(method = "loess", size = 1.5) +
#		scale_colour_brewer(type = "div",
#				guide = guide_legend(title = NULL, reverse=T,
#						override.aes = list(alpha = 1))) +
#		scale_x_continuous(trans=cubroot_trans(), limits = c(0,3)) +
#		scale_y_continuous(trans=log10_trans(), limits = c(0,15000)) +
#		theme_bw() + theme(legend.key = element_blank()) +
#		ggtitle("Price (log10) by Cubed-Root of Carat and Clarity")
#p
#ggsave("plots/crcaratpriceclairtylog10lowess.png", height=5, width=6)


p = ggplot( data=diamondsbig, aes(carat, price, colour=cut)) +
		geom_point(alpha = 0.5, size = .75, position="jitter") +
		scale_colour_brewer(type = "div",
				guide = guide_legend(title = NULL, reverse=T,
						override.aes = list(alpha = 1))) +
		scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
				breaks = c(350, 1000, 5000, 10000, 15000)) +
		theme_bw() + theme(legend.key = element_blank()) +
		ggtitle("Price (log10) by Cube-Root of Carat and Cut")
p

ggsave("plots/caratpricecutlog10.png", height=5, width=6.25)


p = ggplot( data=diamondsbig, aes(carat, price, colour=color)) +
		geom_point(alpha = 0.5, size = .75, position="jitter") +
		scale_colour_brewer(type = "div",
				guide = guide_legend(title = NULL, reverse=T,
						override.aes = list(alpha = 1))) +
		scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
				breaks = c(350, 1000, 5000, 10000, 15000)) +
		theme_bw() + theme(legend.key = element_blank()) +
		ggtitle("Price (log10) by Cube-Root of Carat and Color")
p

ggsave("plots/caratpricecolorlog10.png", height=5, width=6.25)

diamondsbig$logprice = log(diamondsbig$price) 


#install.packages('memisc')
library('memisc')
m1 = lm(logprice~  I(carat^(1/3)), 
    data=diamondsbig[diamondsbig$price < 10000 & diamondsbig$cert == "GIA",])
m2 = update(m1, ~ . + carat)
m3 = update(m2, ~ . + cut )
m4 = update(m3, ~ . + color + clarity)
mtable(m1, m2, m3, m4)

# Example from BlueNile
# Round 1.00Very GoodIVS1 Jan 16 $5,601

thisDiamond = data.frame(carat = 1.00, cut = "V.Good", color = "I", clarity="VS1")
modEst = predict(m4, newdata = thisDiamond, interval="prediction", level = .95)
exp(modEst)


# This will take a while to run
#install.packages('DAAG')
library(DAAG)
cv2 = cv.lm(df=na.omit(diamondsbig[,c(1:4, 13)]), m2, m=10) 
cv3 = cv.lm(df=na.omit(diamondsbig[,c(1:4, 13)]), m3, m=10) 
cv4 = cv.lm(df=na.omit(diamondsbig[,c(1:4, 13)]), m4, m=10) 















