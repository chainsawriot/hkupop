require(foreign)
hkupop <- read.spss("tp1403013.sav", to.data.frame = TRUE, use.value.labels = FALSE)

### 4 person with NA in agegp, should be excluded. Impossible to do weighting
hkupop <- hkupop[!is.na(hkupop$agegp) & !is.na(hkupop$gender),]

hkupop$gp <- paste0(hkupop$gender,hkupop$agegp)

require(plyr)

censuspop <- read.csv("pop.csv")

totalpop <- sum(censuspop$pop)

rawpopdist <- ddply(censuspop, .(gp, gender), summarise, popdist = sum(pop) / totalpop)

popdist <- data.frame(gp = paste0(rawpopdist$gender, rawpopdist$gp), popdist = rawpopdist$popdist, stringsAsFactors = FALSE)

rm(rawpopdist, totalpop, censuspop)

boot.mean <- function(hkupop, popdist) {
    while(TRUE) {
        bootsampleindex <- sample(1:nrow(hkupop), nrow(hkupop), replace=TRUE)
        bootsample <- hkupop[bootsampleindex, ]
        if (sum(table(bootsample$gp) < 3) == 0) {
            res <- ddply(bootsample, .(gp), summarise, mce =  mean(CE_rating, na.rm = TRUE))
            res$weight <- popdist$popdist[match(res$gp, popdist$gp)]
            return(sum(res$mce * res$weight))
        }
    }
}


x <- replicate(10000, { boot.mean(hkupop, popdist) })

quantile(x, probs = c(0.005, 0.5, 0.995))

### even this is not exceeding 50.
max(x)

png("xdist.png")
hist(x)
dev.off()

passrate <- ddply(hkupop, .(gp), summarize, pass = sum(CE_rating >= 50, na.rm = TRUE), n = length(CE_rating), percentage = (sum(CE_rating >= 50, na.rm = TRUE) / length(CE_rating)) * 100)
passrate$weight <- popdist$popdist[match(passrate$gp, popdist$gp)]


hkiaps <- c(53.6, 54.5, 55.7, 51.5, 52.6, 50.0, 48.8 , 49.4 , 50.0 , 48.9 , 46.4 , 48.1 , 48.2 , 49.3 , 46.0 , 46.2 , 45.7 , 44.8 , 45.9, 41.7 , 40.4 , 42.1 , 46.1, 45.4)
hkupop <- c(51.5, 54.0, 54.3, 51.3, 51.2, 49.0, 46.0, 49.1, 52.3, 49.1, 48.1, 48.0, 51.4, 49.9, 46.7, 46.2,  45.1, 43.7, 49.4, 44.0, 40.0,  45.0 ,47.0,  46.4 )

require(lubridate)
require(reshape2)
require(ggplot2)

sur.date <- floor_date(seq(as.Date("2012-03-15"), by = "31 days", length.out = 24), "month")

surveyData <- melt(data.frame(hkiaps, hkupop, sur.date), id.vars = c("sur.date"))
names(surveyData) <- c("date", "organization", "rating")

hkupopvshkiaps <- ggplot(surveyData, aes(x = date, y = rating, group = organization, colour = organization)) + geom_line() + expand_limits(y = 30) + labs(x = "日期", y = "平均評分") + scale_color_discrete(name  ="機構",breaks=c("hkiaps", "hkupop"),labels=c("亞太所", "港大民研"))
ggsave("trend.jpg", hkupopvshkiaps)

cor(hkupop, hkiaps)


pop2 <- read.csv("pop2.csv", stringsAsFactors = FALSE)

pop2$approve <- as.numeric(gsub(" ", "", pop2$approve))

plot(pop2$meanr, pop2$approve, xlab="平均評分", ylab = "反對比率(%)")

ggsave("scatter.jpg", ggplot(pop2, aes(meanr, approve)) + geom_point() + stat_smooth(method="lm", se=FALSE) + labs(x = "平均評分", y = "不支持比率(%)"))

require(ROCR)
pred <- prediction(pop2$meanr, pop2$approve <= 50)
perf <- performance(pred, measure = "sens", "spec")
plot(perf)

perf@alpha.values[[1]][which.max((perf@x.values[[1]]) + perf@y.values[[1]])]
perf@x.values[[1]][which.max((perf@x.values[[1]]) + perf@y.values[[1]])] ### specificity, True Positive. Because the positive event is actually < 50.
perf@y.values[[1]][which.max((perf@x.values[[1]]) + perf@y.values[[1]])] ### False Positive Rate




table(pop2$meanr < 49, pop2$approve > 50)
## ts analysis

hku_ts <- ts(hkupop, start=c(2012, 3), end = c(2014,2), frequency = 12)
iap_ts <- ts(hkiaps, start=c(2012, 3), end = c(2014,2), frequency = 12)

auto.arima(hku_ts, allowdrift = FALSE) #(0,1,2)

hku012 <- arima(hku_ts, order = c(0,1,2))
pwx <- hku012$residuals
ccf(residuals(Arima(iap_ts, model = hku012)), pwx, plot=FALSE)