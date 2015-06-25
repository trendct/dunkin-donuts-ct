library(dplyr)

dunk <- read.csv("dunkindonuts.csv")

new_england <- subset(dunk, state=="CT" |
                        state=="RI" |
                        state=="MA" |
                        state=="VT" |
                        state=="NH" |
                        state=="ME")

ct <- subset(new_england, state=="CT")
attributes(ri <- subset(new_england, state=="RI")
ma <- subset(new_england, state=="MA")
vt <- subset(new_england, state=="VT")
nh <- subset(new_england, state=="NH")
me <- subset(new_england, state=="ME")

ctpop <- read.csv("ctpop.csv")

ct <- dunk %>% filter(state=="CT") %>% count(city)
colnames(ct) <- c("name", "n")

ct_test <- left_join(ct, ctpop)

library(ctnamecleaner)

ctnamecleaner(name, ct)

# HERE IT FUCKING IS!
combined <- read.csv("combined.csv", stringsAsFactors=FALSE)


combined$percapita <- (combined$n/combined$pop2013)*1000
combined$percapita <- round(combined$percapita, digits=2)

combined <- combined[order(-combined$percapita),]
combined$real.town.name <- str_to_title(combined$real.town.name)
combined$id <- combined$real.time.name

require(stringr)
require(gtools)
require(ggplot2)
require(rgdal)
require(scales)
require(ggmap)
require(Cairo)
require(gpclib)
require(maptools)
require(reshape)
gpclibPermit()
gpclibPermitStatus()

towntracts <- readOGR(dsn="townsmap", layer="towns")
towntracts <- fortify(towntracts, region="NAME10")

colnames(combined) <- c("row","name","n","id","pop2013","percapita")
rename(combined, real.town.name=id)
            
Dunk_Data <- left_join(towntracts, combined)

dd <- ggplot() +
  geom_polygon(data = Dunk_Data, aes(x=long, y=lat, group=group, 
                                         fill=percapita), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", palette = "Reds", breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Dunkin' Donut stores per 10,000 residents", fill="")


ggsave(dd, file="dunkinmap.png", width=8, height=4, type="cairo-png")

dd2 <- ggplot() +
  geom_polygon(data = Dunk_Data, aes(x=long, y=lat, group=group, 
                                     fill=n), color = "black", size=0.2) +
  coord_map() +
  scale_fill_distiller(type="seq", palette = "Reds", breaks=pretty_breaks(n=5)) +
  theme_nothing(legend=TRUE) +
  labs(title="Dunkin' Donut stores", fill="")
ggsave(dd, file="dunkinmaptotal.png", width=8, height=4, type="cairo-png")

dc <- read.csv("DunkinCount.csv", stringsAsFactors=FALSE)

p <- ggplot(data=dc, aes(x=Dunkin.Donuts, y=Population)) +
  geom_point(aes(text=paste(Towns, State, sep=", ")))+ 
               geom_smooth(method=lm, formula = y ~ poly(x, 2), size=1) +
               theme_minimal() +
               ggtitle("Dunkin' Donuts versus town population in New England") +
               labs(y="Population", x="Dunkin' Donuts")
             

# take out boston

dcnotboston <- subset(dc, Towns!="Boston")

p <- ggplot(data=dcnotboston, aes(x=Dunkin.Donuts, y=Population)) +
  geom_point(aes(text=paste(Towns, State, sep=", ")))+ 
  geom_smooth(method=lm, formula = y ~ poly(x, 2), size=1) +
  theme_minimal() +
  theme(text=element_text(family="Lato", size=14)) +
  ggtitle("Dunkin' Donuts versus town population in New England") +
  labs(y="Population", x="Dunkin' Donuts")


library(extrafont)
library(extrafontdb)


ddp <- plotly()

ddpy <- ddp$ggplotly(p)

lm(dc$Population~dc$Dunkin.Donuts)

dcm_lm2 <- lm(dcnotboston$Dunkin.Donuts ~ poly(dcnotboston$Population, 2))

summary(dcm_lm1)
summary(dcm_lm2)
summary(dcm_lm3)

dcm_lm <- lm(dcnotboston$Population ~ poly(dcnotboston$Dunkin.Donuts, 2))

summary(dcm_lm)


beta0 (5263,159), beta1 (29,318) and beta2 (-10,589)
$$f(x)=5263.1597+29.318x-10.589x^2$$
  
dunkinsss <-   2738.77 + 4405.57*(5) + 81.51*(5)^2

new <- data.frame(Population = dcnotboston$Population)

#new <- data.frame(Population = ct_test$pop2013)

dammit <- predict(dcm_lm2, newdata = new, interval="confidence")

damn$prediction <- null
damn <- cbind(dcnotboston, dammit)
damnct <- damn %>% filter(State=="CT")
damnct$rounded <- round(damnct$upr, digits=0)
damnct$prediction <-damnct$rounded - damnct$Dunkin.Donuts
  
damn<- damn[order(damn$Towns),]

population <- 182669
dunkins = .0001692 * population + .08

Dunkins = .0001692*population + .08

plot(dc$Population, dc$Dunkin.Donuts)
abline(dcm_lm)

dcct <- dc %>% filter(State=="CT")

ctpop <- read.csv("ctpop.csv", stringsAsFactors=FALSE)
ctpop$ddmodel <- .0001692 * ctpop$total + .08

colnames(ctpop) <- c("Towns", "totalpop", "ddmodel")
newct <- left_join(ctpop, dcct)
newct$diff <- newct$ddmodel - newct$Dunkin.Donuts
newct$diffrounded <- round(newct$diff, digits=0)


newct <- newct[order(-newct$diffrounded),]
