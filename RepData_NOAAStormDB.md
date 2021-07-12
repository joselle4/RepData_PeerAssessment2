# NOAA Storm Database Reproducible Research: An Investigation on how Storm Events affect Population Health and their Economic Consequences
Joselle Abagat  
October 20, 2016  

## I. Synopsis

##### This document investigates the data contained in the NOAA Storm Database.  Two questions are being answered: (1) which storm events are most harmful with respect to population health and (2) which storm events have the greatest economic consequences.  In order to answer which storm events affect population health, a subset of the data is analyzed involving storm events, fatalities and injuries.  Analysis of this subset showed that tornadoes, excessive heat or heat, flash floods or floods and lightning are most harmful with respect to population health.  In order to answer which storm events have the greatest economic consequences, a subset of the data is analyzed involving storm events and property and crop damages.  Analysis of this subset showed that floods, hurricanes/typhoons, tornadoes and storm surges have the greatest economic consequences.  All data processing and analyses are found in Section II.  All results are presented in Section III.  

## II. Data Processing 

#### This section contains information regarding the loading, processing and analysis of the NOAA Storm database.  

### Set global options

```r
knitr::opts_chunk$set(echo = TRUE)
```


### Load libraries

##### The r libraries used in this report are: ggplot, plyr, dplyr, grid


### Loading and preprocessing the data

```r
# load data
filePath <- "/Users/joselle4/Documents/Programming/R/coursera/repdata-data-StormData.csv.bz2"
stormDb <- read.csv(filePath, header = TRUE, na.strings = "NA")
```

### Data analysis

#### 1) Storm Events vs. Population Health
##### In order to answer which storm events are most harmful with respect to population health, we need to look at how the variable EVTYPE relates to variables FATALITIES and INJURIES. We look at FATALITIES because more deaths would lead to a population decline, therefore affecting population health.  Meanwhile, while not as drastic, INJURIES can also affect population health.  Studying the relationship between these variables will allow us to determine which storm events lead to the most catastrophies with respect to population health.


```r
# obtain subset of the data and aggregate to obtain sum of data
subEventFatalatiesInjuries <- subset(stormDb,
                             FATALITIES > 0,
                             select = c(EVTYPE, FATALITIES, INJURIES))

sumEventFatalaties <- aggregate(FATALITIES ~ EVTYPE,
                                subEventFatalatiesInjuries,
                                FUN = sum)
sumEventInjuries <- aggregate(INJURIES ~ EVTYPE,
                              subEventFatalatiesInjuries,
                              FUN = sum)
summary(sumEventFatalaties)
```

```
##           EVTYPE      FATALITIES     
##  AVALANCE    :  1   Min.   :   1.00  
##  AVALANCHE   :  1   1st Qu.:   1.00  
##  BLACK ICE   :  1   Median :   4.00  
##  BLIZZARD    :  1   Mean   :  90.15  
##  blowing snow:  1   3rd Qu.:  19.75  
##  BLOWING SNOW:  1   Max.   :5633.00  
##  (Other)     :162
```

```r
summary(sumEventInjuries)
```

```
##           EVTYPE       INJURIES      
##  AVALANCE    :  1   Min.   :    0.0  
##  AVALANCHE   :  1   1st Qu.:    0.0  
##  BLACK ICE   :  1   Median :    1.0  
##  BLIZZARD    :  1   Mean   :  470.2  
##  blowing snow:  1   3rd Qu.:   18.5  
##  BLOWING SNOW:  1   Max.   :60187.0  
##  (Other)     :162
```

##### Looking at the summary provided above, we can see that certain events lead to very small fatalities.  This allows us to set a threshold.  For the purpose of this analysis, let's set the threshold at the mean: 90.15.  We will also set a similar threshold approach to injuries.


```r
# subset to only include anything greater than the threshold
subsetAtFatalities <- subset(sumEventFatalaties,
                             FATALITIES > mean(FATALITIES))

subsetAtInjuries <- subset(sumEventInjuries,
                           INJURIES > mean(INJURIES))

table(subsetAtFatalities$FATALITIES)
```

```
## 
##   95   96   98  101  103  125  127  133  160  172  204  206  224  248  368 
##    1    1    1    2    1    1    1    1    1    1    1    1    1    1    1 
##  470  504  816  937  978 1903 5633 
##    1    1    1    1    1    1    1
```

```r
table(subsetAtInjuries$INJURIES)
```

```
## 
##   599   641   646   649   718  1219  1420  1720  2679  4791 60187 
##     1     1     1     1     1     1     1     1     1     1     1
```

##### Looking at the table of fatalities and injuries above, we can see that setting a threshold has decreased the data where only events where only large fatalities or injuries are included.  This will allow for a more meaningful look at the results later.


#### 2) Storm Events vs. Economic Consequences

##### In order to answer which storm events have the greatest economic consequences, we need to look at how EVTYPE relates to variables PROPDMG and CROPDMG.  These two variables represent property and crop damages, respectively.  We would also need to look at variables PROPDMG and CROPDMG since they are the associated units to PROPDMG and CROPDMG, respectively.  Variables, PROPDMG and CROPDMG, are estimated in magnitudes of thousands (K), millions (M) and billions (B).


```r
levels(stormDb$PROPDMGEXP)
```

```
##  [1] ""  "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K"
## [18] "m" "M"
```

```r
levels(stormDb$CROPDMGEXP)
```

```
## [1] ""  "?" "0" "2" "B" "k" "K" "m" "M"
```

##### According to National Weather Service's Storm Data Documentation found in https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf Section 2.7, "alphabetical characters used to signify magnitude include “K” for thousands, “M” for millions, and “B” for billions."  It did not mention what the other levels signify.  Therefore, we will look at the data involving only these values: k, K, m, M, b, B, H, h.


```r
# subset per group in order to remove as much of the 0 data as possible
subEventPropDmg <- subset(stormDb,
                          PROPDMG > 0 & (PROPDMGEXP == "K" |
                                             PROPDMGEXP == "M" | 
                                             PROPDMGEXP == "m" | 
                                             PROPDMGEXP == "H" | 
                                             PROPDMGEXP == "h" | 
                                             PROPDMGEXP == "B"),
                          select = c(EVTYPE, PROPDMG, PROPDMGEXP))
subEventCropDmg <- subset(stormDb,
                          CROPDMG > 0 & (CROPDMGEXP == "K" | 
                                             CROPDMGEXP == "k" | 
                                             CROPDMGEXP == "M" | 
                                             CROPDMGEXP == "m" | 
                                             CROPDMGEXP == "B"),
                          select = c(EVTYPE, CROPDMG, CROPDMGEXP))

subEventPropDmg <- aggregate(PROPDMG ~ EVTYPE + PROPDMGEXP,
                             subEventPropDmg,
                             FUN = sum)
subEventCropDmg <- aggregate(CROPDMG ~ EVTYPE + CROPDMGEXP,
                             subEventCropDmg,
                             FUN = sum)

summary(subEventPropDmg)
```

```
##                 EVTYPE      PROPDMGEXP     PROPDMG       
##  HAIL              :  5   K      :368   Min.   :      0  
##  HURRICANE OPAL    :  4   M      :131   1st Qu.:      5  
##  THUNDERSTORM WINDS:  4   B      : 19   Median :     50  
##  TORNADO           :  4   m      :  4   Mean   :  20717  
##  FLASH FLOOD       :  3   H      :  2   3rd Qu.:    500  
##  FLOOD             :  3   h      :  1   Max.   :3163480  
##  (Other)           :502   (Other):  0
```

```r
summary(subEventCropDmg)
```

```
##                EVTYPE      CROPDMGEXP     CROPDMG      
##  DROUGHT          :  3   K      :110   Min.   :     0  
##  FLASH FLOODING   :  3   M      : 72   1st Qu.:    10  
##  FREEZE           :  3   B      :  6   Median :    50  
##  HAIL             :  3   k      :  4   Mean   :  7138  
##  HEAT             :  3   m      :  1   3rd Qu.:   675  
##  HURRICANE/TYPHOON:  3          :  0   Max.   :576707  
##  (Other)          :175   (Other):  0
```

##### Since we are trying to determine the economic consequences, we can combine both the property and crop damages and just look at damages as a whole:


```r
# change coloumn names and bind data
colnames(subEventPropDmg) <- c("EVTYPE", "DMGEXP", "DMG")
colnames(subEventCropDmg) <- c("EVTYPE", "DMGEXP", "DMG")

subEventDmg <- rbind(subEventPropDmg, subEventCropDmg)

summary(subEventDmg)
```

```
##                 EVTYPE        DMGEXP         DMG         
##  HAIL              :  7   K      :478   Min.   :      0  
##  HURRICANE OPAL    :  6   M      :203   1st Qu.:      5  
##  HURRICANE/TYPHOON :  6   B      : 25   Median :     50  
##  RIVER FLOOD       :  6   m      :  5   Mean   :  17138  
##  THUNDERSTORM WINDS:  6   k      :  4   3rd Qu.:    515  
##  TORNADO           :  6          :  0   Max.   :3163480  
##  (Other)           :678   (Other):  0
```

#####   The data needs to be tidied to the same units.  For the purpose of this analysis, we will look at all damages in millions since based on the data summary, there are a few items in billions from both property and crop damages and converting to thousands would lead to large numbers.  


```r
# we ignore all other PROPDMGEXP values except B, M, or K since 
# they don't have units assigned to them
for (i in 1:nrow(subEventDmg)) {
    if(subEventDmg$DMGEXP[i] == "B") {
        subEventDmg$DMG[i] <- subEventDmg$DMG[i]*1000
    } 
    if(subEventDmg$DMGEXP[i] == "K" | subEventDmg$DMGEXP[i] == "k") {
        subEventDmg$DMG[i] <- subEventDmg$DMG[i]/1000
    }
    if(subEventDmg$DMGEXP[i] == "H" | subEventDmg$DMGEXP[i] == "h") {
        subEventDmg$DMG[i] <- subEventDmg$DMG[i]/1000000
    }
}

# change to M since everything has been converted to millions
subEventDmg$DMGEXP <- "M"

summary(subEventDmg)
```

```
##                 EVTYPE       DMGEXP               DMG           
##  HAIL              :  7   Length:715         Min.   :     0.00  
##  HURRICANE OPAL    :  6   Class :character   1st Qu.:     0.05  
##  HURRICANE/TYPHOON :  6   Mode  :character   Median :     0.60  
##  RIVER FLOOD       :  6                      Mean   :   666.33  
##  THUNDERSTORM WINDS:  6                      3rd Qu.:    10.00  
##  TORNADO           :  6                      Max.   :122500.00  
##  (Other)           :678
```

##### Similar to the population health problem, let's set the threshold at the mean of 666.93M so we can see a decreased data with a more meaningful result.


```r
# subset to only include anything greater than the threshold
subDmgAtMean <- subset(subEventDmg, 
                       DMG > mean(DMG))
```

## III. Results

##### This section provides results regarding the data analysis performed above on the NOAA Storm data. 

### 1) Storm Events vs. Population Health

##### From the analysis, we have reduced the storm data to only look at storm events with large fatalities. Let's look at a bar plot of these storm events in relation to fatalities.


```r
gg1 <- ggplot(data = subsetAtFatalities, aes(x = EVTYPE, y = FATALITIES)) + 
    geom_bar(stat = "identity", aes(fill = factor(FATALITIES))) +
    xlab("STORM EVENTS") + 
    ggtitle("Fatalities caused by Storm Events") + 
    theme(axis.text.x = element_text(angle = 90))

gg2 <- ggplot(data = subsetAtInjuries, aes(x = EVTYPE, y = INJURIES)) + 
    geom_bar(stat = "identity", aes(fill = factor(INJURIES))) +
    xlab("STORM EVENTS") + 
    ggtitle("Injuries caused by Storm Events") + 
    theme(axis.text.x = element_text(angle = 90))

grid.newpage()
grid.draw(rbind(ggplotGrob(gg1), ggplotGrob(gg2), size = "last"))
```

![](RepData_NOAAStormDB_files/figure-html/plotEventsFatalities-1.png)<!-- -->

#### Conclusion:

##### Based on the graph, the top storm events that have the most effect on population health are:


```r
subsetAtFatalities <- arrange(subsetAtFatalities, desc(FATALITIES))
subsetAtInjuries <- arrange(subsetAtInjuries, desc(INJURIES))

head(subsetAtFatalities,5)
```

```
##           EVTYPE FATALITIES
## 1        TORNADO       5633
## 2 EXCESSIVE HEAT       1903
## 3    FLASH FLOOD        978
## 4           HEAT        937
## 5      LIGHTNING        816
```

```r
head(subsetAtInjuries, 3)
```

```
##           EVTYPE INJURIES
## 1        TORNADO    60187
## 2 EXCESSIVE HEAT     4791
## 3          FLOOD     2679
```

##### With regards to population health, TORNADO is the most catastrophic storm event.  Tornadoes have lead to the most fatalities and injuries.  Therefore, it is most detrimental effect on population health.


### 2) Storm Events vs. Economic Consequences

##### From the analysis reduced data summary, we can plot the property and crop damages as a function of storm events.  


```r
gg <- ggplot(data = subDmgAtMean, aes(x = EVTYPE, y = DMG, fill = "")) + 
    geom_bar(stat = "identity") + theme(legend.position="none") + 
    xlab("STORM EVENTS") + ylab("DAMAGE [Millions]") + 
    ggtitle("Storm Events vs. Economic Consequence") + 
    theme(axis.text.x = element_text(angle = 90))
gg
```

![](RepData_NOAAStormDB_files/figure-html/plotEventsDmgs-1.png)<!-- -->

#### Conclusion:

##### Based on the graph, the top four storm events that have the most effect on the economy are:


```r
subDmgAtMean <- arrange(subDmgAtMean, desc(DMG))
head(subDmgAtMean, 4)
```

```
##              EVTYPE DMGEXP       DMG
## 1             FLOOD      M 122500.00
## 2 HURRICANE/TYPHOON      M  65500.00
## 3           TORNADO      M  48462.18
## 4       STORM SURGE      M  42560.00
```

##### According to the data shown above, while all FLOOD, HURRICANE/TYPHOON, TORNADO and STORM SURGE lead to the greatest economic consequences, FLOOD is the most catastrophic storm event when we measure damages in millions. 


