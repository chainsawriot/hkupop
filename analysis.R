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

