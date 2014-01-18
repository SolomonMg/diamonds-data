# TODO: Add comment
#
# Author: solomon
###############################################################################

setwd("~/Dropbox/public/Blogposts/Diamonds")
load( file="data/BigDiamonds.Rda")
diamonds = diamonds[diamonds$cert=="GIA",]

#install.packages('GGally')
#install.packages('ggplot2')
#install.packages('scales')
#install.packages('memisc')

library(ggplot2)
library(GGally)
library(scales)

diasamp = diamonds[sample(1:length(diamonds$price), 10000), 1:8]
png("plots/ggpairs.png", height=1000, width=1000)
ggpairs(na.omit(diasamp), params = c(shape = I("."), outlier.shape = I(".")))
dev.off()


p = qplot(price, data=diamonds, binwidth=100) +
		theme_bw() +
		ggtitle("Price")
p

ggsave("plots/price.png", height=5, width=5)


p = qplot(price, data=diamonds, binwidth = 0.01) +
		scale_x_log10() +
		theme_bw() +
		ggtitle("Price (log10)")
p

ggsave("plots/pricelog10.png", height=5, width=5)


p = qplot(carat, price, data=diamonds, xlim=c(0,3)) +
		theme_bw() +
		ggtitle("Price by Carat")
ggsave("plots/caratprice.png", height=5, width=5)


p = qplot(carat, price, data=diamonds) +
		scale_y_continuous(trans=log10_trans() ) +
		theme_bw() +
		ggtitle("Price (log10) by Cubed-Root of Carat")
p

ggsave("plots/caratpricelog10.png", height=5, width=5)


cubroot_trans = function() trans_new("cubroot", transform= function(x) x^(1/3), inverse = function(x) x^3 )

p = qplot(carat, price, data=diamonds) +
		scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
				breaks = c(350, 1000, 5000, 10000, 15000)) +
		theme_bw() +
		ggtitle("Price (log10) by Cubed-Root of Carat")
p

ggsave("plots/crcaratpricelog10.png", height=5, width=5)


head(sort(table(diamonds$carat), decreasing=T ))
head(sort(table(diamonds$price), decreasing=T ))



p = ggplot( data=diamonds, aes(carat, price)) +
		geom_point(alpha = 0.5, size = .75, position="jitter") +
		scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,15000),
				breaks = c(350, 1000, 5000, 10000, 15000)) +
		theme_bw() +
		ggtitle("Price (log10) by Cubed-Root of Carat")
p

ggsave("plots/crcaratpricejitterlog10.png", height=5, width=6)



p = ggplot( data=diamonds, aes(carat, price, colour=cert)) +
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


p = ggplot( data=diamonds, aes(carat, price, colour=clarity)) +
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


#p = ggplot( data=diamonds, aes(carat, price, colour=clarity)) +
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


p = ggplot( data=diamonds, aes(carat, price, colour=cut)) +
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


p = ggplot( data=diamonds, aes(carat, price, colour=color)) +
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






m1 = lm(I(log(price))~ I(carat^(1/3)), data=diamonds[diamonds$cert=="GIA",])
m2 = update(m1, ~ . + carat)
m3 = update(m2, ~ . + I(as.numeric(clarity)) )
m4 = update(m3, ~ . + I(as.numeric(color)) + I(as.numeric(cut)) )
library(memisc)
mtable(m1, m2, m3, m4)

# m1 = lm(I(log(price))~  I(carat^(1/3)), data=diamonds)
# m2 = update(m1, ~ . + carat)
# m3 = update(m2, ~ . + cut )
# m4 = update(m3, ~ . + color )
# m5 = update(m4, ~ . + clarity )
# library(memisc)
# mtable(m1, m2, m3, m4, m5)

levels(cv$color)

thisDiamond = data.frame(carat = .7, cut = "Ideal", color = "I", clarity="VS2")
modEst = predict(m4, newdata = thisDiamond, interval="prediction", level = .95)
exp(modEst)

# Example from BlueNile
# Round 1.00Very GoodIVS1 Jan 16 $5,601

thisDiamond = data.frame(carat = 1.00, cut = "Very Good", color = "I", clarity="VS1")
modEst = predict(m4, newdata = thisDiamond, interval="prediction", level = .95)
exp(modEst)





















