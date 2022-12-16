Final Project
================
Joanna Kennedy
2022-11-22

WHEN BUILDING IT ON RSTUDIO NEED TO BUILD WEBSITE BEFOR EYOU PUSH TO GIT
HUB

histograms and scatterplots - min plot number dendrogram

``` r
library(ggplot2)
library(stringr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ tibble  3.1.8      ✔ purrr   0.3.4 
    ## ✔ tidyr   1.2.1      ✔ dplyr   1.0.10
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
library(ggrepel)
library(caret)
```

    ## Warning: package 'caret' was built under R version 4.2.2

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(MLmetrics)
```

    ## Warning: package 'MLmetrics' was built under R version 4.2.2

    ## 
    ## Attaching package: 'MLmetrics'
    ## 
    ## The following objects are masked from 'package:caret':
    ## 
    ##     MAE, RMSE
    ## 
    ## The following object is masked from 'package:base':
    ## 
    ##     Recall

``` r
library(stats)
library(dplyr)
library(glmnet)
```

    ## Warning: package 'glmnet' was built under R version 4.2.2

    ## Loading required package: Matrix

    ## Warning: package 'Matrix' was built under R version 4.2.2

    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-6

``` r
setwd("C:/Users/jjken/Desktop/Harvard/Fall 2022/BST 260/finalprojectwebsite")
df = read.csv(file = 'Student Mental health.csv')
```

Cleaning

``` r
head(df)
```

    ##        Timestamp Choose.your.gender Age What.is.your.course.
    ## 1 8/7/2020 12:02             Female  18          Engineering
    ## 2 8/7/2020 12:04               Male  21    Islamic education
    ## 3 8/7/2020 12:05               Male  19                  BIT
    ## 4 8/7/2020 12:06             Female  22                 Laws
    ## 5 8/7/2020 12:13               Male  23         Mathemathics
    ## 6 8/7/2020 12:31               Male  19          Engineering
    ##   Your.current.year.of.Study What.is.your.CGPA. Marital.status
    ## 1                     year 1        3.00 - 3.49             No
    ## 2                     year 2        3.00 - 3.49             No
    ## 3                     Year 1        3.00 - 3.49             No
    ## 4                     year 3        3.00 - 3.49            Yes
    ## 5                     year 4        3.00 - 3.49             No
    ## 6                     Year 2        3.50 - 4.00             No
    ##   Do.you.have.Depression. Do.you.have.Anxiety. Do.you.have.Panic.attack.
    ## 1                     Yes                   No                       Yes
    ## 2                      No                  Yes                        No
    ## 3                     Yes                  Yes                       Yes
    ## 4                     Yes                   No                        No
    ## 5                      No                   No                        No
    ## 6                      No                   No                       Yes
    ##   Did.you.seek.any.specialist.for.a.treatment.
    ## 1                                           No
    ## 2                                           No
    ## 3                                           No
    ## 4                                           No
    ## 5                                           No
    ## 6                                           No

``` r
apply(df, 2, unique)
```

    ## $Timestamp
    ##  [1] "8/7/2020 12:02"      "8/7/2020 12:04"      "8/7/2020 12:05"     
    ##  [4] "8/7/2020 12:06"      "8/7/2020 12:13"      "8/7/2020 12:31"     
    ##  [7] "8/7/2020 12:32"      "8/7/2020 12:33"      "8/7/2020 12:35"     
    ## [10] "8/7/2020 12:39"      "8/7/2020 12:40"      "8/7/2020 12:41"     
    ## [13] "8/7/2020 12:43"      "8/7/2020 12:46"      "8/7/2020 12:52"     
    ## [16] "8/7/2020 13:05"      "8/7/2020 13:07"      "8/7/2020 13:12"     
    ## [19] "8/7/2020 13:13"      "8/7/2020 13:15"      "8/7/2020 13:17"     
    ## [22] "8/7/2020 13:29"      "8/7/2020 13:35"      "8/7/2020 13:41"     
    ## [25] "8/7/2020 13:58"      "8/7/2020 14:05"      "8/7/2020 14:27"     
    ## [28] "8/7/2020 14:29"      "8/7/2020 14:31"      "8/7/2020 14:41"     
    ## [31] "8/7/2020 14:43"      "8/7/2020 14:45"      "8/7/2020 14:47"     
    ## [34] "8/7/2020 14:56"      "8/7/2020 14:57"      "8/7/2020 14:58"     
    ## [37] "8/7/2020 15:07"      "8/7/2020 15:08"      "8/7/2020 15:09"     
    ## [40] "8/7/2020 15:12"      "8/7/2020 15:14"      "8/7/2020 15:18"     
    ## [43] "8/7/2020 15:27"      "8/7/2020 15:37"      "8/7/2020 15:47"     
    ## [46] "8/7/2020 15:48"      "8/7/2020 15:57"      "8/7/2020 15:58"     
    ## [49] "8/7/2020 16:08"      "8/7/2020 16:21"      "8/7/2020 16:22"     
    ## [52] "8/7/2020 16:34"      "8/7/2020 16:53"      "8/7/2020 17:05"     
    ## [55] "8/7/2020 17:37"      "8/7/2020 17:46"      "8/7/2020 17:50"     
    ## [58] "8/7/2020 18:10"      "8/7/2020 18:11"      "8/7/2020 19:05"     
    ## [61] "8/7/2020 19:32"      "8/7/2020 20:36"      "8/7/2020 21:21"     
    ## [64] "8/7/2020 22:35"      "9/7/2020 6:57"       "9/7/2020 11:43"     
    ## [67] "9/7/2020 11:57"      "9/7/2020 13:15"      "9/7/2020 18:24"     
    ## [70] "13/07/2020 10:07:32" "13/07/2020 10:10:30" "13/07/2020 10:11:26"
    ## [73] "13/07/2020 10:12:18" "13/07/2020 10:12:26" "13/07/2020 10:12:28"
    ## [76] "13/07/2020 10:14:46" "13/07/2020 10:33:47" "13/07/2020 10:34:08"
    ## [79] "13/07/2020 11:46:13" "13/07/2020 11:49:02" "13/07/2020 11:54:58"
    ## [82] "13/07/2020 13:57:11" "13/07/2020 14:38:12" "13/07/2020 14:48:05"
    ## [85] "13/07/2020 16:15:13" "13/07/2020 17:30:44" "13/07/2020 19:08:32"
    ## [88] "13/07/2020 19:56:49" "13/07/2020 21:21:42" "13/07/2020 21:22:56"
    ## [91] "13/07/2020 21:23:57" "18/07/2020 20:16:21"
    ## 
    ## $Choose.your.gender
    ## [1] "Female" "Male"  
    ## 
    ## $Age
    ## [1] "18" "21" "19" "22" "23" "20" "24" NA  
    ## 
    ## $What.is.your.course.
    ##  [1] "Engineering"             "Islamic education"      
    ##  [3] "BIT"                     "Laws"                   
    ##  [5] "Mathemathics"            "Pendidikan islam"       
    ##  [7] "BCS"                     "Human Resources"        
    ##  [9] "Irkhs"                   "Psychology"             
    ## [11] "KENMS"                   "Accounting "            
    ## [13] "ENM"                     "Marine science"         
    ## [15] "KOE"                     "Banking Studies"        
    ## [17] "Business Administration" "Law"                    
    ## [19] "KIRKHS"                  "Usuluddin "             
    ## [21] "TAASL"                   "Engine"                 
    ## [23] "ALA"                     "Biomedical science"     
    ## [25] "koe"                     "Kirkhs"                 
    ## [27] "BENL"                    "Benl"                   
    ## [29] "IT"                      "CTS"                    
    ## [31] "engin"                   "Econs"                  
    ## [33] "MHSC"                    "Malcom"                 
    ## [35] "Kop"                     "Human Sciences "        
    ## [37] "Biotechnology"           "Communication "         
    ## [39] "Diploma Nursing"         "Pendidikan Islam "      
    ## [41] "Radiography"             "psychology"             
    ## [43] "Fiqh fatwa "             "DIPLOMA TESL"           
    ## [45] "Koe"                     "Fiqh"                   
    ## [47] "Islamic Education"       "Nursing "               
    ## [49] "Pendidikan Islam"       
    ## 
    ## $Your.current.year.of.Study
    ## [1] "year 1" "year 2" "Year 1" "year 3" "year 4" "Year 2" "Year 3"
    ## 
    ## $What.is.your.CGPA.
    ## [1] "3.00 - 3.49"  "3.50 - 4.00"  "3.50 - 4.00 " "2.50 - 2.99"  "2.00 - 2.49" 
    ## [6] "0 - 1.99"    
    ## 
    ## $Marital.status
    ## [1] "No"  "Yes"
    ## 
    ## $Do.you.have.Depression.
    ## [1] "Yes" "No" 
    ## 
    ## $Do.you.have.Anxiety.
    ## [1] "No"  "Yes"
    ## 
    ## $Do.you.have.Panic.attack.
    ## [1] "Yes" "No" 
    ## 
    ## $Did.you.seek.any.specialist.for.a.treatment.
    ## [1] "No"  "Yes"

???????WHEN NUMBER OF YES IS SMALL SHOULD I SPLIT THE TRAIN AND TEST TO
INCLUDE ALL YES IN BOTH TRAIN AND TEST (or some for even amount) -
should I double dip –\> should I decrease the size of testing to get
half and half and have all in training (OR 1/3)

``` r
for (i in 1:ncol(df)){
  
  temp <- as.data.frame(df[,i])
  nm <- colnames(df)[i]
  colnames(temp) <- "response"
  
  counts <- temp %>% group_by(response) %>% dplyr::summarise(count = n())
  
  plot<- ggplot(data=counts, aes(x=response, y=count, fill=response)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=count)) +
    theme(axis.text.x = element_text(size = 6, angle = 90)) +
    ggtitle(nm)
  print(plot)
  
}
```

![](Final-Project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

    ## Warning: Removed 1 rows containing missing values (position_stack).

    ## Warning: Removed 1 rows containing missing values (geom_text).

![](Final-Project_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-7.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-8.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-9.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-10.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-5-11.png)<!-- -->

``` r
colnames(df) = c("day","gender","age", "course","grade","gpa","married", "depression","anxiety","panic_attack", "treatment")
```

``` r
sapply(df, function(x) sum(is.na(x)))
```

    ##          day       gender          age       course        grade          gpa 
    ##            0            0            1            0            0            0 
    ##      married   depression      anxiety panic_attack    treatment 
    ##            0            0            0            0            0

``` r
df <- df %>% filter(!is.na(df$age))
```

``` r
df <- as.data.frame(apply(df, 2, tolower))
```

year and month are the same so just day and time are different

``` r
df <- separate(df, col=day, into=c("day", "time"), sep = " ")
df <- separate(df, col=day, into=c("day", "rest"), sep = "/")
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 100 rows [1, 2, 3, 4,
    ## 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
df$rest <- NULL
df <- separate(df, col=time, into=c("hour", "minute"), sep = ":")
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 23 rows [78, 79, 80,
    ## 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, ...].

round hour and get rid of minute

``` r
df$hour <- as.numeric(df$hour)
df$minute <- as.numeric(df$minute)
df$round <- 0
df[which(df$minute >29), 'round'] <- 1
df$hour <- df$hour + df$round
df$round <- NULL
df$minute <- NULL
```

``` r
df$course <- sapply(df$course, str_trim)
df$course <- df$course %>% str_replace("^engin.*","engineering") %>% str_replace("^fiqh.*","fiqh") %>% str_replace("^law.*", "laws") %>% str_replace(".*irkhs$", "kirkhs") %>% str_replace(".*nursing$", "nursing")
df %>% arrange(course) %>% group_by(course) %>% dplyr::summarise(count = n()) %>% arrange(-count)
```

    ## # A tibble: 35 × 2
    ##    course             count
    ##    <chr>              <int>
    ##  1 engineering           20
    ##  2 bcs                   18
    ##  3 bit                    9
    ##  4 koe                    6
    ##  5 biomedical science     4
    ##  6 benl                   3
    ##  7 kirkhs                 3
    ##  8 laws                   3
    ##  9 pendidikan islam       3
    ## 10 psychology             3
    ## # … with 25 more rows

``` r
df$grade <- df$grade %>% str_replace("year 1","9") %>% str_replace("year 2","10") %>% str_replace("year 3", "11") %>% str_replace("year 4", "12")
```

``` r
df$gpa <- sapply(df$gpa, str_trim)
df <- separate(df, col=gpa, into=c("low", "gpa"), sep = " - ")
df$low <- NULL
```

``` r
for (i in 1:ncol(df)){
  
  temp <- as.data.frame(df[,i])
  nm <- colnames(df)[i]
  colnames(temp) <- "response"
  
  counts <- as.data.frame(temp) %>% group_by(response) %>% dplyr::summarise(count = n())
  
  plot<- ggplot(data=counts, aes(x=response, y=count, fill=response)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=count)) +
    theme(axis.text.x = element_text(size = 6, angle = 90)) +
    ggtitle(nm)
  print(plot)
  
}
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-5.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-6.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-7.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-8.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-9.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-10.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-11.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-15-12.png)<!-- -->

``` r
#ggplot(age_counts_mental_health, aes(x=mental_health, y=count)) +
#  geom_point() +
```

``` r
df[,c(1,2,4,6,7)]<-as.data.frame(apply(df[,c(1,2,4,6,7)], 2, as.numeric))
numerical <- df[,c(1,2,4,6,7)]
numerical_colnames <- c("day","hour", "age","grade","gpa")
categorical <- df[,c(5)]
categorical_colnames <- c("course")
binary <- df[,c(3,8:12)]
binary_colnames <- c("gender", "married", "depression", "anxiety", "panic_attack", "treatment") #"engineering", "bcs")
```

``` r
#apply(numerical,2,class)
#apply(numerical,2,hist)
```

``` r
numerical
```

    ##     day hour age grade  gpa
    ## 1     8   12  18     9 3.49
    ## 2     8   12  21    10 3.49
    ## 3     8   12  19     9 3.49
    ## 4     8   12  22    11 3.49
    ## 5     8   12  23    12 3.49
    ## 6     8   13  19    10 4.00
    ## 7     8   13  23    10 4.00
    ## 8     8   13  18     9 4.00
    ## 9     8   13  19    10 2.99
    ## 10    8   13  18     9 4.00
    ## 11    8   13  20     9 4.00
    ## 12    8   13  24    11 4.00
    ## 13    8   13  18     9 3.49
    ## 14    8   13  19     9 3.49
    ## 15    8   13  18    10 4.00
    ## 16    8   13  24    11 4.00
    ## 17    8   13  24    11 3.49
    ## 18    8   13  24    12 3.49
    ## 19    8   13  20    10 4.00
    ## 20    8   13  18    10 4.00
    ## 21    8   13  19     9 3.49
    ## 22    8   13  18    10 3.49
    ## 23    8   13  24     9 4.00
    ## 24    8   13  24     9 3.49
    ## 25    8   13  23    11 4.00
    ## 26    8   13  18     9 4.00
    ## 27    8   14  19     9 4.00
    ## 28    8   14  18    10 3.49
    ## 29    8   14  24    11 4.00
    ## 30    8   14  24    12 4.00
    ## 31    8   14  23    10 3.49
    ## 32    8   14  18    10 3.49
    ## 33    8   14  19     9 4.00
    ## 34    8   15  18    10 4.00
    ## 35    8   15  19     9 3.49
    ## 36    8   15  18     9 2.49
    ## 37    8   15  18    11 3.49
    ## 38    8   15  19     9 2.99
    ## 39    8   15  18     9 4.00
    ## 40    8   15  24    10 2.99
    ## 41    8   15  24    11 3.49
    ## 42    8   15  22    12 4.00
    ## 43    8   15  20    10 3.49
    ## 44    8   15  23    10 4.00
    ## 45    8   15  18     9 4.00
    ## 46    8   15  19     9 4.00
    ## 47    8   15  18    12 4.00
    ## 48    8   15  24    10 3.49
    ## 49    8   15  24    11 4.00
    ## 50    8   15  23     9 2.99
    ## 51    8   16  18    10 4.00
    ## 52    8   16  19    11 3.49
    ## 53    8   16  20    11 3.49
    ## 54    8   16  19     9 4.00
    ## 55    8   16  21     9 3.49
    ## 56    8   16  23    11 4.00
    ## 57    8   16  20    11 3.49
    ## 58    8   16  18     9 4.00
    ## 59    8   17  23     9 3.49
    ## 60    8   17  18    11 3.49
    ## 61    8   17  19     9 4.00
    ## 62    8   17  18     9 4.00
    ## 63    8   18  24     9 4.00
    ## 64    8   18  24     9 4.00
    ## 65    8   18  23     9 4.00
    ## 66    8   18  18    11 3.49
    ## 67    8   18  19    11 3.49
    ## 68    8   19  18     9 4.00
    ## 69    8   20  24    12 3.49
    ## 70    8   21  24     9 3.49
    ## 71    8   21  18    11 4.00
    ## 72    8   23  19    11 3.49
    ## 73    9    7  18     9 1.99
    ## 74    9   12  24    11 4.00
    ## 75    9   12  24     9 4.00
    ## 76    9   13  23     9 3.49
    ## 77    9   18  18    10 3.49
    ## 78   13   10  19    11 1.99
    ## 79   13   10  18    12 4.00
    ## 80   13   10  24    10 4.00
    ## 81   13   10  24    10 4.00
    ## 82   13   10  19     9 3.49
    ## 83   13   10  19    10 3.49
    ## 84   13   10  23     9 3.49
    ## 85   13   11  18     9 4.00
    ## 86   13   11  19    11 3.49
    ## 87   13   12  18     9 4.00
    ## 88   13   12  24     9 3.49
    ## 89   13   12  24    10 2.49
    ## 90   13   14  23    11 4.00
    ## 91   13   15  18    10 3.49
    ## 92   13   15  19    10 3.49
    ## 93   13   16  18     9 3.49
    ## 94   13   18  24    11 1.99
    ## 95   13   19  18     9 4.00
    ## 96   13   20  21     9 4.00
    ## 97   13   21  18    10 3.49
    ## 98   13   21  19    11 4.00
    ## 99   13   21  23    12 4.00
    ## 100  18   20  20    10 3.49

``` r
plot(cbind(numerical,binary))
```

![](Final-Project_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
plot(numerical)
```

![](Final-Project_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
test <- df %>% group_by(grade,course) %>% dplyr::summarise(group_gpa=mean(gpa), count = n()) %>% filter(count>2)
```

    ## `summarise()` has grouped output by 'grade'. You can override using the
    ## `.groups` argument.

``` r
ggplot(test, aes(x=grade,y=group_gpa, color=course)) +
  geom_line() +
  geom_point()
```

![](Final-Project_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
test <- df %>% group_by(grade,course) %>% dplyr::summarise(group_gpa=mean(gpa), count = n()) %>% filter(count>2)
```

    ## `summarise()` has grouped output by 'grade'. You can override using the
    ## `.groups` argument.

``` r
ggplot(test, aes(grade, group_gpa)) + 
  geom_jitter(aes(col=course)) + 
  geom_smooth(aes(col=course), method="lm", se=F)
```

    ## `geom_smooth()` using formula 'y ~ x'

![](Final-Project_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
explore <- df

label_mental_health <- function(df,last_column){
  df$mental_health <- ""
  for (participant in 1:nrow(df)){
    for (condition in 9:last_column){
      if (df[participant,][condition] == "yes"){
        df[participant,][last_column+1] <- paste(df[participant,][last_column+1],colnames(df[condition]))
      }
    }
  }
  df$mental_health <- sapply(df$mental_health, str_squish)
  return (df)
}

explore <- label_mental_health(explore, dim(explore)[2])
```

``` r
explore %>% group_by(mental_health) %>% dplyr::summarise(count = n())
```

    ## # A tibble: 11 × 2
    ##    mental_health                               count
    ##    <chr>                                       <int>
    ##  1 ""                                             36
    ##  2 "anxiety"                                      13
    ##  3 "anxiety panic_attack"                          3
    ##  4 "depression"                                   10
    ##  5 "depression anxiety"                            6
    ##  6 "depression anxiety panic_attack"               9
    ##  7 "depression anxiety panic_attack treatment"     1
    ##  8 "depression anxiety treatment"                  2
    ##  9 "depression panic_attack"                       4
    ## 10 "depression panic_attack treatment"             3
    ## 11 "panic_attack"                                 13

``` r
for(i in 1:(ncol(numerical))){
  
  temp <- as.data.frame(numerical[,i])
  nm <- colnames(numerical)[i]
  colnames(temp) <- "response"
  
  temp$mental_health <- explore$mental_health
  
  counts <- as.data.frame(temp) %>% group_by(response,mental_health) %>% dplyr::summarise(count = n())
  
  plot<- ggplot(data=counts, aes(x=response, y=count, fill=response)) +
    geom_bar(stat="identity") +
    geom_text(aes(label=count)) +
    facet_wrap(~mental_health, ncol=3) +
    theme(axis.text.x = element_text(size = 6, angle = 90)) +
    ggtitle(nm)
  print(plot)
  
}
```

    ## `summarise()` has grouped output by 'response'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'response'. You can override using the
    ## `.groups` argument.

![](Final-Project_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

    ## `summarise()` has grouped output by 'response'. You can override using the
    ## `.groups` argument.

![](Final-Project_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

    ## `summarise()` has grouped output by 'response'. You can override using the
    ## `.groups` argument.

![](Final-Project_files/figure-gfm/unnamed-chunk-24-3.png)<!-- -->

    ## `summarise()` has grouped output by 'response'. You can override using the
    ## `.groups` argument.

![](Final-Project_files/figure-gfm/unnamed-chunk-24-4.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-24-5.png)<!-- -->

Chi Squared Test

``` r
two_by_two <- data.frame(
  depression = c("no", "yes"), 
  men = 
    c(sum(df$gender == "male" & df$depression == "no"), 
      sum(df$gender == "male" & df$depression == "yes")), 
  women = 
    c(sum(df$gender == "female" & df$depression == "no"), 
      sum(df$gender == "female" & df$depression == "yes")))

two_by_two
```

    ##   depression men women
    ## 1         no  19    46
    ## 2        yes   6    29

``` r
chisq_test <- two_by_two |> select(-depression) |> chisq.test()
chisq_test
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  select(two_by_two, -depression)
    ## X-squared = 1.1868, df = 1, p-value = 0.276

``` r
length(binary_colnames)
```

    ## [1] 6

``` r
length(binary)
```

    ## [1] 6

``` r
binary_colnames
```

    ## [1] "gender"       "married"      "depression"   "anxiety"      "panic_attack"
    ## [6] "treatment"

``` r
ncol(binary)
```

    ## [1] 6

``` r
binary
```

    ##     gender married depression anxiety panic_attack treatment
    ## 1   female      no        yes      no          yes        no
    ## 2     male      no         no     yes           no        no
    ## 3     male      no        yes     yes          yes        no
    ## 4   female     yes        yes      no           no        no
    ## 5     male      no         no      no           no        no
    ## 6     male      no         no      no          yes        no
    ## 7   female     yes        yes      no          yes        no
    ## 8   female      no         no     yes           no        no
    ## 9   female      no         no      no           no        no
    ## 10    male      no         no     yes          yes        no
    ## 11  female      no         no      no           no        no
    ## 12  female     yes        yes      no           no        no
    ## 13  female      no        yes      no           no        no
    ## 14    male      no         no      no           no        no
    ## 15  female      no         no     yes           no        no
    ## 16    male      no         no      no           no        no
    ## 17  female      no         no      no           no        no
    ## 18  female     yes        yes     yes          yes        no
    ## 19  female      no         no     yes           no        no
    ## 20  female     yes        yes     yes          yes        no
    ## 21  female      no         no      no          yes        no
    ## 22  female      no         no      no           no        no
    ## 23  female      no         no      no           no        no
    ## 24  female      no         no      no           no        no
    ## 25  female      no        yes     yes          yes        no
    ## 26  female      no         no      no           no        no
    ## 27  female      no         no      no           no        no
    ## 28    male     yes        yes     yes           no        no
    ## 29  female     yes        yes     yes          yes       yes
    ## 30  female      no         no      no           no        no
    ## 31  female      no         no      no           no        no
    ## 32    male      no         no      no           no        no
    ## 33    male      no         no      no          yes        no
    ## 34    male     yes        yes     yes           no       yes
    ## 35  female      no        yes     yes          yes        no
    ## 36  female      no         no      no           no        no
    ## 37  female      no        yes     yes           no        no
    ## 38  female      no        yes     yes          yes        no
    ## 39  female      no         no      no           no        no
    ## 40  female     yes        yes      no          yes       yes
    ## 41  female      no         no     yes           no        no
    ## 42  female      no         no      no           no        no
    ## 43  female      no        yes      no           no        no
    ## 44    male      no         no      no          yes        no
    ## 45    male      no         no     yes          yes        no
    ## 46  female      no         no     yes           no        no
    ## 47  female      no         no      no           no        no
    ## 48    male      no        yes      no           no        no
    ## 49  female      no         no      no          yes        no
    ## 50  female     yes        yes      no          yes       yes
    ## 51  female      no         no     yes           no        no
    ## 52  female      no         no      no           no        no
    ## 53  female     yes        yes     yes          yes        no
    ## 54  female      no        yes      no          yes       yes
    ## 55    male      no         no      no           no        no
    ## 56    male      no         no      no           no        no
    ## 57  female      no        yes     yes           no        no
    ## 58  female      no         no      no           no        no
    ## 59  female      no         no      no           no        no
    ## 60  female      no         no      no          yes        no
    ## 61  female      no         no      no           no        no
    ## 62  female      no         no      no          yes        no
    ## 63  female      no         no      no          yes        no
    ## 64  female      no         no      no           no        no
    ## 65  female      no        yes     yes           no        no
    ## 66  female      no         no     yes           no        no
    ## 67    male     yes        yes      no          yes        no
    ## 68  female      no        yes      no           no        no
    ## 69  female      no         no     yes           no        no
    ## 70  female      no         no      no           no        no
    ## 71  female      no         no      no          yes        no
    ## 72  female     yes        yes      no           no        no
    ## 73    male      no         no      no           no        no
    ## 74    male      no         no     yes           no        no
    ## 75  female      no         no     yes          yes        no
    ## 76  female      no        yes      no           no        no
    ## 77  female      no         no      no          yes        no
    ## 78  female      no         no      no           no        no
    ## 79  female      no         no      no           no        no
    ## 80  female     yes        yes     yes          yes        no
    ## 81  female      no         no      no           no        no
    ## 82  female      no        yes     yes           no        no
    ## 83  female      no         no      no           no        no
    ## 84    male      no         no      no           no        no
    ## 85  female      no        yes     yes           no       yes
    ## 86  female      no         no      no           no        no
    ## 87  female      no        yes     yes          yes        no
    ## 88    male      no         no     yes           no        no
    ## 89    male      no         no      no          yes        no
    ## 90  female      no         no      no          yes        no
    ## 91    male      no         no     yes           no        no
    ## 92  female     yes        yes      no           no        no
    ## 93  female      no        yes      no           no        no
    ## 94  female      no         no      no          yes        no
    ## 95  female      no         no      no           no        no
    ## 96  female      no         no     yes           no        no
    ## 97    male      no        yes     yes           no        no
    ## 98  female     yes        yes      no          yes        no
    ## 99  female      no         no      no           no        no
    ## 100   male      no         no      no           no        no

``` r
association <- c()
for(i in 1:(ncol(binary)-1)){
  for(j in (i+1):(ncol(binary))){
    if (i != j){
      print(binary_colnames[i])
      print(binary_colnames[j])
      test <- chisq.test(df[,i], df[,j])
      print(test)
      if (test$p.value < 0.05) {
        association <- c(association, paste(binary_colnames[i], binary_colnames[j], sep = " vs. "))
      }
    }
    
  }
}
```

    ## [1] "gender"
    ## [1] "married"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 109.74, df = 39, p-value = 1.195e-08
    ## 
    ## [1] "gender"
    ## [1] "depression"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 3.7347, df = 3, p-value = 0.2916
    ## 
    ## [1] "gender"
    ## [1] "anxiety"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 20.909, df = 18, p-value = 0.284
    ## 
    ## [1] "gender"
    ## [1] "panic_attack"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 100.63, df = 102, p-value = 0.5198
    ## 
    ## [1] "gender"
    ## [1] "treatment"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 4.452, df = 9, p-value = 0.8792
    ## 
    ## [1] "married"
    ## [1] "depression"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 14.835, df = 13, p-value = 0.3178
    ## 
    ## [1] "married"
    ## [1] "anxiety"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 57.132, df = 78, p-value = 0.9636
    ## 
    ## [1] "married"
    ## [1] "panic_attack"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 488.61, df = 442, p-value = 0.06203
    ## 
    ## [1] "married"
    ## [1] "treatment"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 27.248, df = 39, p-value = 0.9217
    ## 
    ## [1] "depression"
    ## [1] "anxiety"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 4.0437, df = 6, p-value = 0.6708
    ## 
    ## [1] "depression"
    ## [1] "panic_attack"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 30.519, df = 34, p-value = 0.639
    ## 
    ## [1] "depression"
    ## [1] "treatment"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 5.9951, df = 3, p-value = 0.1118
    ## 
    ## [1] "anxiety"
    ## [1] "panic_attack"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 193.62, df = 204, p-value = 0.6878
    ## 
    ## [1] "anxiety"
    ## [1] "treatment"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 19.949, df = 18, p-value = 0.3357
    ## 
    ## [1] "panic_attack"
    ## [1] "treatment"

    ## Warning in chisq.test(df[, i], df[, j]): Chi-squared approximation may be
    ## incorrect

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  df[, i] and df[, j]
    ## X-squared = 126.77, df = 102, p-value = 0.04878

``` r
association
```

    ## [1] "gender vs. married"         "panic_attack vs. treatment"

Regression

``` r
non_normal_numerical = c()
for (i in 1:ncol(numerical)){
  test <- shapiro.test(numerical[,i])
  print(test)
  
  if (test$p.value < 0.05)
    non_normal_numerical <- c(non_normal_numerical, numerical_colnames[i])
}
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  numerical[, i]
    ## W = 0.57082, p-value = 1.241e-15
    ## 
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  numerical[, i]
    ## W = 0.9583, p-value = 0.003049
    ## 
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  numerical[, i]
    ## W = 0.78617, p-value = 9.519e-11
    ## 
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  numerical[, i]
    ## W = 0.82161, p-value = 1.209e-09
    ## 
    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  numerical[, i]
    ## W = 0.69687, p-value = 4.562e-13

``` r
non_normal_numerical
```

    ## [1] "day"   "hour"  "age"   "grade" "gpa"

``` r
for(i in 1:(ncol(numerical)-1)) {
  for(j in (i+1):ncol(numerical)){
    for(k in binary_colnames){
      plot <- ggplot(df,aes(x=df[,numerical_colnames[i]], y=df[,numerical_colnames[j]], color=df[,k], group = df[,k])) +
        #geom_boxplot() +
        geom_point(position=position_jitter(h=0.07,w=0.07)) +
        ylab(numerical_colnames[j]) +
        xlab(numerical_colnames[i]) +
        labs(color = k)
      print(plot)
    }
  }
}
```

![](Final-Project_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-4.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-5.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-6.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-7.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-8.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-9.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-10.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-11.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-12.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-13.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-14.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-15.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-16.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-17.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-18.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-19.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-20.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-21.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-22.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-23.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-24.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-25.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-26.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-27.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-28.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-29.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-30.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-31.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-32.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-33.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-34.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-35.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-36.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-37.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-38.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-39.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-40.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-41.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-42.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-43.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-44.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-45.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-46.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-47.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-48.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-49.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-50.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-51.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-52.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-53.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-54.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-55.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-56.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-57.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-58.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-59.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-32-60.png)<!-- -->

I acknowledge gender as a categorical value rather than a binary value,
but within the context of this datasets, students were categorized as
either female or male.

``` r
df
```

    ##     day hour gender age                  course grade  gpa married depression
    ## 1     8   12 female  18             engineering     9 3.49      no        yes
    ## 2     8   12   male  21       islamic education    10 3.49      no         no
    ## 3     8   12   male  19                     bit     9 3.49      no        yes
    ## 4     8   12 female  22                    laws    11 3.49     yes        yes
    ## 5     8   12   male  23            mathemathics    12 3.49      no         no
    ## 6     8   13   male  19             engineering    10 4.00      no         no
    ## 7     8   13 female  23        pendidikan islam    10 4.00     yes        yes
    ## 8     8   13 female  18                     bcs     9 4.00      no         no
    ## 9     8   13 female  19         human resources    10 2.99      no         no
    ## 10    8   13   male  18                  kirkhs     9 4.00      no         no
    ## 11    8   13 female  20              psychology     9 4.00      no         no
    ## 12    8   13 female  24             engineering    11 4.00     yes        yes
    ## 13    8   13 female  18                     bcs     9 3.49      no        yes
    ## 14    8   13   male  19             engineering     9 3.49      no         no
    ## 15    8   13 female  18                   kenms    10 4.00      no         no
    ## 16    8   13   male  24                     bcs    11 4.00      no         no
    ## 17    8   13 female  24              accounting    11 3.49      no         no
    ## 18    8   13 female  24                     enm    12 3.49     yes        yes
    ## 19    8   13 female  20                     bit    10 4.00      no         no
    ## 20    8   13 female  18          marine science    10 4.00     yes        yes
    ## 21    8   13 female  19             engineering     9 3.49      no         no
    ## 22    8   13 female  18                     koe    10 3.49      no         no
    ## 23    8   13 female  24                     bcs     9 4.00      no         no
    ## 24    8   13 female  24             engineering     9 3.49      no         no
    ## 25    8   13 female  23                     bcs    11 4.00      no        yes
    ## 26    8   13 female  18         banking studies     9 4.00      no         no
    ## 27    8   14 female  19             engineering     9 4.00      no         no
    ## 28    8   14   male  18             engineering    10 3.49     yes        yes
    ## 29    8   14 female  24                     bit    11 4.00     yes        yes
    ## 30    8   14 female  24                     bcs    12 4.00      no         no
    ## 31    8   14 female  23 business administration    10 3.49      no         no
    ## 32    8   14   male  18                     bcs    10 3.49      no         no
    ## 33    8   14   male  19                     bcs     9 4.00      no         no
    ## 34    8   15   male  18                     bcs    10 4.00     yes        yes
    ## 35    8   15 female  19                     bit     9 3.49      no        yes
    ## 36    8   15 female  18             engineering     9 2.49      no         no
    ## 37    8   15 female  18                    laws    11 3.49      no        yes
    ## 38    8   15 female  19                     bit     9 2.99      no        yes
    ## 39    8   15 female  18                  kirkhs     9 4.00      no         no
    ## 40    8   15 female  24             engineering    10 2.99     yes        yes
    ## 41    8   15 female  24                     bit    11 3.49      no         no
    ## 42    8   15 female  22             engineering    12 4.00      no         no
    ## 43    8   15 female  20               usuluddin    10 3.49      no        yes
    ## 44    8   15   male  23                   taasl    10 4.00      no         no
    ## 45    8   15   male  18                     bcs     9 4.00      no         no
    ## 46    8   15 female  19             engineering     9 4.00      no         no
    ## 47    8   15 female  18             engineering    12 4.00      no         no
    ## 48    8   15   male  24                     bcs    10 3.49      no        yes
    ## 49    8   15 female  24                     bcs    11 4.00      no         no
    ## 50    8   15 female  23                     ala     9 2.99     yes        yes
    ## 51    8   16 female  18                     bcs    10 4.00      no         no
    ## 52    8   16 female  19      biomedical science    11 3.49      no         no
    ## 53    8   16 female  20                     koe    11 3.49     yes        yes
    ## 54    8   16 female  19                     bcs     9 4.00      no        yes
    ## 55    8   16   male  21                     bcs     9 3.49      no         no
    ## 56    8   16   male  23                  kirkhs    11 4.00      no         no
    ## 57    8   16 female  20                    benl    11 3.49      no        yes
    ## 58    8   16 female  18                     bcs     9 4.00      no         no
    ## 59    8   17 female  23                    benl     9 3.49      no         no
    ## 60    8   17 female  18                      it    11 3.49      no         no
    ## 61    8   17 female  19                     bcs     9 4.00      no         no
    ## 62    8   17 female  18                     cts     9 4.00      no         no
    ## 63    8   18 female  24             engineering     9 4.00      no         no
    ## 64    8   18 female  24             engineering     9 4.00      no         no
    ## 65    8   18 female  23                   econs     9 4.00      no        yes
    ## 66    8   18 female  18                     koe    11 3.49      no         no
    ## 67    8   18   male  19                    mhsc    11 3.49     yes        yes
    ## 68    8   19 female  18                  malcom     9 4.00      no        yes
    ## 69    8   20 female  24                     kop    12 3.49      no         no
    ## 70    8   21 female  24      biomedical science     9 3.49      no         no
    ## 71    8   21 female  18                    laws    11 4.00      no         no
    ## 72    8   23 female  19                     bit    11 3.49     yes        yes
    ## 73    9    7   male  18      biomedical science     9 1.99      no         no
    ## 74    9   12   male  24                     bit    11 4.00      no         no
    ## 75    9   12 female  24                     koe     9 4.00      no         no
    ## 76    9   13 female  23             engineering     9 3.49      no        yes
    ## 77    9   18 female  18          human sciences    10 3.49      no         no
    ## 78   13   10 female  19           biotechnology    11 1.99      no         no
    ## 79   13   10 female  18             engineering    12 4.00      no         no
    ## 80   13   10 female  24           communication    10 4.00     yes        yes
    ## 81   13   10 female  24                 nursing    10 4.00      no         no
    ## 82   13   10 female  19             engineering     9 3.49      no        yes
    ## 83   13   10 female  19        pendidikan islam    10 3.49      no         no
    ## 84   13   10   male  23             radiography     9 3.49      no         no
    ## 85   13   11 female  18              psychology     9 4.00      no        yes
    ## 86   13   11 female  19                    fiqh    11 3.49      no         no
    ## 87   13   12 female  18              psychology     9 4.00      no        yes
    ## 88   13   12   male  24                     bit     9 3.49      no         no
    ## 89   13   12   male  24             engineering    10 2.49      no         no
    ## 90   13   14 female  23            diploma tesl    11 4.00      no         no
    ## 91   13   15   male  18                     koe    10 3.49      no         no
    ## 92   13   15 female  19                     koe    10 3.49     yes        yes
    ## 93   13   16 female  18                    benl     9 3.49      no        yes
    ## 94   13   18 female  24                    fiqh    11 1.99      no         no
    ## 95   13   19 female  18       islamic education     9 4.00      no         no
    ## 96   13   20 female  21                     bcs     9 4.00      no         no
    ## 97   13   21   male  18             engineering    10 3.49      no        yes
    ## 98   13   21 female  19                 nursing    11 4.00     yes        yes
    ## 99   13   21 female  23        pendidikan islam    12 4.00      no         no
    ## 100  18   20   male  20      biomedical science    10 3.49      no         no
    ##     anxiety panic_attack treatment
    ## 1        no          yes        no
    ## 2       yes           no        no
    ## 3       yes          yes        no
    ## 4        no           no        no
    ## 5        no           no        no
    ## 6        no          yes        no
    ## 7        no          yes        no
    ## 8       yes           no        no
    ## 9        no           no        no
    ## 10      yes          yes        no
    ## 11       no           no        no
    ## 12       no           no        no
    ## 13       no           no        no
    ## 14       no           no        no
    ## 15      yes           no        no
    ## 16       no           no        no
    ## 17       no           no        no
    ## 18      yes          yes        no
    ## 19      yes           no        no
    ## 20      yes          yes        no
    ## 21       no          yes        no
    ## 22       no           no        no
    ## 23       no           no        no
    ## 24       no           no        no
    ## 25      yes          yes        no
    ## 26       no           no        no
    ## 27       no           no        no
    ## 28      yes           no        no
    ## 29      yes          yes       yes
    ## 30       no           no        no
    ## 31       no           no        no
    ## 32       no           no        no
    ## 33       no          yes        no
    ## 34      yes           no       yes
    ## 35      yes          yes        no
    ## 36       no           no        no
    ## 37      yes           no        no
    ## 38      yes          yes        no
    ## 39       no           no        no
    ## 40       no          yes       yes
    ## 41      yes           no        no
    ## 42       no           no        no
    ## 43       no           no        no
    ## 44       no          yes        no
    ## 45      yes          yes        no
    ## 46      yes           no        no
    ## 47       no           no        no
    ## 48       no           no        no
    ## 49       no          yes        no
    ## 50       no          yes       yes
    ## 51      yes           no        no
    ## 52       no           no        no
    ## 53      yes          yes        no
    ## 54       no          yes       yes
    ## 55       no           no        no
    ## 56       no           no        no
    ## 57      yes           no        no
    ## 58       no           no        no
    ## 59       no           no        no
    ## 60       no          yes        no
    ## 61       no           no        no
    ## 62       no          yes        no
    ## 63       no          yes        no
    ## 64       no           no        no
    ## 65      yes           no        no
    ## 66      yes           no        no
    ## 67       no          yes        no
    ## 68       no           no        no
    ## 69      yes           no        no
    ## 70       no           no        no
    ## 71       no          yes        no
    ## 72       no           no        no
    ## 73       no           no        no
    ## 74      yes           no        no
    ## 75      yes          yes        no
    ## 76       no           no        no
    ## 77       no          yes        no
    ## 78       no           no        no
    ## 79       no           no        no
    ## 80      yes          yes        no
    ## 81       no           no        no
    ## 82      yes           no        no
    ## 83       no           no        no
    ## 84       no           no        no
    ## 85      yes           no       yes
    ## 86       no           no        no
    ## 87      yes          yes        no
    ## 88      yes           no        no
    ## 89       no          yes        no
    ## 90       no          yes        no
    ## 91      yes           no        no
    ## 92       no           no        no
    ## 93       no           no        no
    ## 94       no          yes        no
    ## 95       no           no        no
    ## 96      yes           no        no
    ## 97      yes           no        no
    ## 98       no          yes        no
    ## 99       no           no        no
    ## 100      no           no        no

``` r
head(train)
```

    ##                         
    ## 1 function (x, ...)     
    ## 2 {                     
    ## 3     UseMethod("train")
    ## 4 }

``` r
#c("gender", "married", "depression", "anxiety", "panic_attack", "treatment", "engineering")

traini <- sample(nrow(df), round(0.8*nrow(df)))
testi <- c(1:nrow(df))[-traini]
train <- df[traini, ]
test <- df[testi, ]


train$gender <- ifelse(train$gender == 'female',1,0)
train$married <- ifelse(train$married == 'yes',1,0)
train$depression <- ifelse(train$depression == 'yes',1,0)
train$anxiety <- ifelse(train$anxiety == 'yes',1,0)
train$panic_attack <- ifelse(train$panic_attack == 'yes',1,0)
train$treatment <- ifelse(train$treatment == 'yes',1,0)
train$engineering <- ifelse(train$course == 'engineering',1,0)
train$bcs <- ifelse(train$course == 'bcs',1,0)

test$gender <- ifelse(test$gender == 'female',1,0)
test$married <- ifelse(test$married == 'yes',1,0)
test$depression <- ifelse(test$depression == 'yes',1,0)
test$anxiety <- ifelse(test$anxiety == 'yes',1,0)
test$panic_attack <- ifelse(test$panic_attack == 'yes',1,0)
test$treatment <- ifelse(test$treatment == 'yes',1,0)
test$engineering <- ifelse(test$course == 'engineering',1,0)
test$bcs <- ifelse(test$course == 'bcs',1,0)

train <- train[,-5]
test <- test[,-5]

train <- train[,c(1,2,4:6,3,7:ncol(train))]
test <- test[,c(1,2,4:6,3,7:ncol(test))]

train
```

    ##     day hour age grade  gpa gender married depression anxiety panic_attack
    ## 56    8   16  23    11 4.00      0       0          0       0            0
    ## 76    9   13  23     9 3.49      1       0          1       0            0
    ## 45    8   15  18     9 4.00      0       0          0       1            1
    ## 47    8   15  18    12 4.00      1       0          0       0            0
    ## 28    8   14  18    10 3.49      0       1          1       1            0
    ## 36    8   15  18     9 2.49      1       0          0       0            0
    ## 4     8   12  22    11 3.49      1       1          1       0            0
    ## 17    8   13  24    11 3.49      1       0          0       0            0
    ## 48    8   15  24    10 3.49      0       0          1       0            0
    ## 81   13   10  24    10 4.00      1       0          0       0            0
    ## 55    8   16  21     9 3.49      0       0          0       0            0
    ## 1     8   12  18     9 3.49      1       0          1       0            1
    ## 39    8   15  18     9 4.00      1       0          0       0            0
    ## 35    8   15  19     9 3.49      1       0          1       1            1
    ## 52    8   16  19    11 3.49      1       0          0       0            0
    ## 21    8   13  19     9 3.49      1       0          0       0            1
    ## 90   13   14  23    11 4.00      1       0          0       0            1
    ## 100  18   20  20    10 3.49      0       0          0       0            0
    ## 58    8   16  18     9 4.00      1       0          0       0            0
    ## 57    8   16  20    11 3.49      1       0          1       1            0
    ## 97   13   21  18    10 3.49      0       0          1       1            0
    ## 75    9   12  24     9 4.00      1       0          0       1            1
    ## 85   13   11  18     9 4.00      1       0          1       1            0
    ## 49    8   15  24    11 4.00      1       0          0       0            1
    ## 30    8   14  24    12 4.00      1       0          0       0            0
    ## 98   13   21  19    11 4.00      1       1          1       0            1
    ## 26    8   13  18     9 4.00      1       0          0       0            0
    ## 32    8   14  18    10 3.49      0       0          0       0            0
    ## 33    8   14  19     9 4.00      0       0          0       0            1
    ## 34    8   15  18    10 4.00      0       1          1       1            0
    ## 53    8   16  20    11 3.49      1       1          1       1            1
    ## 3     8   12  19     9 3.49      0       0          1       1            1
    ## 83   13   10  19    10 3.49      1       0          0       0            0
    ## 18    8   13  24    12 3.49      1       1          1       1            1
    ## 6     8   13  19    10 4.00      0       0          0       0            1
    ## 12    8   13  24    11 4.00      1       1          1       0            0
    ## 24    8   13  24     9 3.49      1       0          0       0            0
    ## 69    8   20  24    12 3.49      1       0          0       1            0
    ## 16    8   13  24    11 4.00      0       0          0       0            0
    ## 65    8   18  23     9 4.00      1       0          1       1            0
    ## 86   13   11  19    11 3.49      1       0          0       0            0
    ## 66    8   18  18    11 3.49      1       0          0       1            0
    ## 40    8   15  24    10 2.99      1       1          1       0            1
    ## 77    9   18  18    10 3.49      1       0          0       0            1
    ## 46    8   15  19     9 4.00      1       0          0       1            0
    ## 87   13   12  18     9 4.00      1       0          1       1            1
    ## 54    8   16  19     9 4.00      1       0          1       0            1
    ## 95   13   19  18     9 4.00      1       0          0       0            0
    ## 51    8   16  18    10 4.00      1       0          0       1            0
    ## 11    8   13  20     9 4.00      1       0          0       0            0
    ## 79   13   10  18    12 4.00      1       0          0       0            0
    ## 60    8   17  18    11 3.49      1       0          0       0            1
    ## 19    8   13  20    10 4.00      1       0          0       1            0
    ## 14    8   13  19     9 3.49      0       0          0       0            0
    ## 99   13   21  23    12 4.00      1       0          0       0            0
    ## 27    8   14  19     9 4.00      1       0          0       0            0
    ## 74    9   12  24    11 4.00      0       0          0       1            0
    ## 88   13   12  24     9 3.49      0       0          0       1            0
    ## 7     8   13  23    10 4.00      1       1          1       0            1
    ## 68    8   19  18     9 4.00      1       0          1       0            0
    ## 93   13   16  18     9 3.49      1       0          1       0            0
    ## 63    8   18  24     9 4.00      1       0          0       0            1
    ## 38    8   15  19     9 2.99      1       0          1       1            1
    ## 22    8   13  18    10 3.49      1       0          0       0            0
    ## 29    8   14  24    11 4.00      1       1          1       1            1
    ## 62    8   17  18     9 4.00      1       0          0       0            1
    ## 5     8   12  23    12 3.49      0       0          0       0            0
    ## 59    8   17  23     9 3.49      1       0          0       0            0
    ## 2     8   12  21    10 3.49      0       0          0       1            0
    ## 15    8   13  18    10 4.00      1       0          0       1            0
    ## 50    8   15  23     9 2.99      1       1          1       0            1
    ## 37    8   15  18    11 3.49      1       0          1       1            0
    ## 73    9    7  18     9 1.99      0       0          0       0            0
    ## 41    8   15  24    11 3.49      1       0          0       1            0
    ## 8     8   13  18     9 4.00      1       0          0       1            0
    ## 96   13   20  21     9 4.00      1       0          0       1            0
    ## 78   13   10  19    11 1.99      1       0          0       0            0
    ## 20    8   13  18    10 4.00      1       1          1       1            1
    ## 67    8   18  19    11 3.49      0       1          1       0            1
    ## 82   13   10  19     9 3.49      1       0          1       1            0
    ##     treatment engineering bcs
    ## 56          0           0   0
    ## 76          0           1   0
    ## 45          0           0   1
    ## 47          0           1   0
    ## 28          0           1   0
    ## 36          0           1   0
    ## 4           0           0   0
    ## 17          0           0   0
    ## 48          0           0   1
    ## 81          0           0   0
    ## 55          0           0   1
    ## 1           0           1   0
    ## 39          0           0   0
    ## 35          0           0   0
    ## 52          0           0   0
    ## 21          0           1   0
    ## 90          0           0   0
    ## 100         0           0   0
    ## 58          0           0   1
    ## 57          0           0   0
    ## 97          0           1   0
    ## 75          0           0   0
    ## 85          1           0   0
    ## 49          0           0   1
    ## 30          0           0   1
    ## 98          0           0   0
    ## 26          0           0   0
    ## 32          0           0   1
    ## 33          0           0   1
    ## 34          1           0   1
    ## 53          0           0   0
    ## 3           0           0   0
    ## 83          0           0   0
    ## 18          0           0   0
    ## 6           0           1   0
    ## 12          0           1   0
    ## 24          0           1   0
    ## 69          0           0   0
    ## 16          0           0   1
    ## 65          0           0   0
    ## 86          0           0   0
    ## 66          0           0   0
    ## 40          1           1   0
    ## 77          0           0   0
    ## 46          0           1   0
    ## 87          0           0   0
    ## 54          1           0   1
    ## 95          0           0   0
    ## 51          0           0   1
    ## 11          0           0   0
    ## 79          0           1   0
    ## 60          0           0   0
    ## 19          0           0   0
    ## 14          0           1   0
    ## 99          0           0   0
    ## 27          0           1   0
    ## 74          0           0   0
    ## 88          0           0   0
    ## 7           0           0   0
    ## 68          0           0   0
    ## 93          0           0   0
    ## 63          0           1   0
    ## 38          0           0   0
    ## 22          0           0   0
    ## 29          1           0   0
    ## 62          0           0   0
    ## 5           0           0   0
    ## 59          0           0   0
    ## 2           0           0   0
    ## 15          0           0   0
    ## 50          1           0   0
    ## 37          0           0   0
    ## 73          0           0   0
    ## 41          0           0   0
    ## 8           0           0   1
    ## 96          0           0   1
    ## 78          0           0   0
    ## 20          0           0   0
    ## 67          0           0   0
    ## 82          0           1   0

``` r
test
```

    ##    day hour age grade  gpa gender married depression anxiety panic_attack
    ## 9    8   13  19    10 2.99      1       0          0       0            0
    ## 10   8   13  18     9 4.00      0       0          0       1            1
    ## 13   8   13  18     9 3.49      1       0          1       0            0
    ## 23   8   13  24     9 4.00      1       0          0       0            0
    ## 25   8   13  23    11 4.00      1       0          1       1            1
    ## 31   8   14  23    10 3.49      1       0          0       0            0
    ## 42   8   15  22    12 4.00      1       0          0       0            0
    ## 43   8   15  20    10 3.49      1       0          1       0            0
    ## 44   8   15  23    10 4.00      0       0          0       0            1
    ## 61   8   17  19     9 4.00      1       0          0       0            0
    ## 64   8   18  24     9 4.00      1       0          0       0            0
    ## 70   8   21  24     9 3.49      1       0          0       0            0
    ## 71   8   21  18    11 4.00      1       0          0       0            1
    ## 72   8   23  19    11 3.49      1       1          1       0            0
    ## 80  13   10  24    10 4.00      1       1          1       1            1
    ## 84  13   10  23     9 3.49      0       0          0       0            0
    ## 89  13   12  24    10 2.49      0       0          0       0            1
    ## 91  13   15  18    10 3.49      0       0          0       1            0
    ## 92  13   15  19    10 3.49      1       1          1       0            0
    ## 94  13   18  24    11 1.99      1       0          0       0            1
    ##    treatment engineering bcs
    ## 9          0           0   0
    ## 10         0           0   0
    ## 13         0           0   1
    ## 23         0           0   1
    ## 25         0           0   1
    ## 31         0           0   0
    ## 42         0           1   0
    ## 43         0           0   0
    ## 44         0           0   0
    ## 61         0           0   1
    ## 64         0           1   0
    ## 70         0           0   0
    ## 71         0           0   0
    ## 72         0           0   0
    ## 80         0           0   0
    ## 84         0           0   0
    ## 89         0           1   0
    ## 91         0           0   0
    ## 92         0           0   0
    ## 94         0           0   0

Lasso Regression

source:
<https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r>

``` r
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))

  
  # Model performance metrics
data.frame(
  RMSE = RMSE,
  Rsquare = R_square
)
  
}
```

``` r
dummies_train <- dummyVars(depression~ ., data = train)
dummies_test <- dummyVars(depression~ ., data = test)
train_dummies = predict(dummies_train, newdata = train)
test_dummies = predict(dummies_test, newdata = test)

x = as.matrix(train_dummies)
y_train = train$depression

x_test = as.matrix(test_dummies)
y_test = test$depression
```

``` r
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 10)

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best
```

    ## [1] 0.05011872

``` r
plot(lasso_reg)
```

![](Final-Project_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

want smaller RMSE

``` r
lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)
```

    ##        RMSE   Rsquare
    ## 1 0.3685567 0.4122125

``` r
predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)
```

    ##        RMSE   Rsquare
    ## 1 0.3608508 0.3799366

``` r
lasso_model$beta
```

    ## 12 x 1 sparse Matrix of class "dgCMatrix"
    ##                       s0
    ## day           .         
    ## hour          .         
    ## age           .         
    ## grade        -0.02481326
    ## gpa           .         
    ## gender        .         
    ## married       0.57386740
    ## anxiety       0.12685143
    ## panic_attack  0.03007372
    ## treatment     0.14926646
    ## engineering   .         
    ## bcs           .

``` r
colnames(train)
```

    ##  [1] "day"          "hour"         "age"          "grade"        "gpa"         
    ##  [6] "gender"       "married"      "depression"   "anxiety"      "panic_attack"
    ## [11] "treatment"    "engineering"  "bcs"

GLM AND KNN

k accuracy binary

``` r
for (k in 1:1){
  knn_fit <- knn3(depression~., data=train, k=k) 
  p_hat_knn_train <- predict(knn_fit, train, type="prob")
  y_hat_knn_train <- factor(ifelse(p_hat_knn_train[,2]>0.5,1,0))
  confusionMatrix(y_hat_knn_train, factor(train$depression))
  #print(k)
  #print(confusionMatrix(y_hat_knn_train, factor(train$depression))$overall["Accuracy"])
  
  
  p_hat_knn_test <- predict(knn_fit, test, type="prob")
  y_hat_knn_test <- factor(ifelse(p_hat_knn_test[,2]>0.5,1,0))
  confusionMatrix(y_hat_knn_test, factor(test$depression))
  #print(confusionMatrix(y_hat_knn_test, factor(test$depression))$overall["Accuracy"])
}



for (k in 1:10){
  knn_fit <- knn3(anxiety~., data=train, k=k) 
  p_hat_knn_train <- predict(knn_fit, train, type="prob")
  y_hat_knn_train <- factor(ifelse(p_hat_knn_train[,2]>0.5,1,0))
  confusionMatrix(y_hat_knn_train, factor(train$anxiety))
  #print(k)
  #print(confusionMatrix(y_hat_knn_train, factor(train$anxiety))$overall["Accuracy"])
  
  
  p_hat_knn_test <- predict(knn_fit, test, type="prob")
  y_hat_knn_test <- factor(ifelse(p_hat_knn_test[,2]>0.5,1,0))
  confusionMatrix(y_hat_knn_test, factor(test$anxiety))
  #print(confusionMatrix(y_hat_knn_test, factor(test$anxiety))$overall["Accuracy"])
}
```

??????????????????????WARNING

``` r
df
```

    ##     day hour gender age                  course grade  gpa married depression
    ## 1     8   12 female  18             engineering     9 3.49      no        yes
    ## 2     8   12   male  21       islamic education    10 3.49      no         no
    ## 3     8   12   male  19                     bit     9 3.49      no        yes
    ## 4     8   12 female  22                    laws    11 3.49     yes        yes
    ## 5     8   12   male  23            mathemathics    12 3.49      no         no
    ## 6     8   13   male  19             engineering    10 4.00      no         no
    ## 7     8   13 female  23        pendidikan islam    10 4.00     yes        yes
    ## 8     8   13 female  18                     bcs     9 4.00      no         no
    ## 9     8   13 female  19         human resources    10 2.99      no         no
    ## 10    8   13   male  18                  kirkhs     9 4.00      no         no
    ## 11    8   13 female  20              psychology     9 4.00      no         no
    ## 12    8   13 female  24             engineering    11 4.00     yes        yes
    ## 13    8   13 female  18                     bcs     9 3.49      no        yes
    ## 14    8   13   male  19             engineering     9 3.49      no         no
    ## 15    8   13 female  18                   kenms    10 4.00      no         no
    ## 16    8   13   male  24                     bcs    11 4.00      no         no
    ## 17    8   13 female  24              accounting    11 3.49      no         no
    ## 18    8   13 female  24                     enm    12 3.49     yes        yes
    ## 19    8   13 female  20                     bit    10 4.00      no         no
    ## 20    8   13 female  18          marine science    10 4.00     yes        yes
    ## 21    8   13 female  19             engineering     9 3.49      no         no
    ## 22    8   13 female  18                     koe    10 3.49      no         no
    ## 23    8   13 female  24                     bcs     9 4.00      no         no
    ## 24    8   13 female  24             engineering     9 3.49      no         no
    ## 25    8   13 female  23                     bcs    11 4.00      no        yes
    ## 26    8   13 female  18         banking studies     9 4.00      no         no
    ## 27    8   14 female  19             engineering     9 4.00      no         no
    ## 28    8   14   male  18             engineering    10 3.49     yes        yes
    ## 29    8   14 female  24                     bit    11 4.00     yes        yes
    ## 30    8   14 female  24                     bcs    12 4.00      no         no
    ## 31    8   14 female  23 business administration    10 3.49      no         no
    ## 32    8   14   male  18                     bcs    10 3.49      no         no
    ## 33    8   14   male  19                     bcs     9 4.00      no         no
    ## 34    8   15   male  18                     bcs    10 4.00     yes        yes
    ## 35    8   15 female  19                     bit     9 3.49      no        yes
    ## 36    8   15 female  18             engineering     9 2.49      no         no
    ## 37    8   15 female  18                    laws    11 3.49      no        yes
    ## 38    8   15 female  19                     bit     9 2.99      no        yes
    ## 39    8   15 female  18                  kirkhs     9 4.00      no         no
    ## 40    8   15 female  24             engineering    10 2.99     yes        yes
    ## 41    8   15 female  24                     bit    11 3.49      no         no
    ## 42    8   15 female  22             engineering    12 4.00      no         no
    ## 43    8   15 female  20               usuluddin    10 3.49      no        yes
    ## 44    8   15   male  23                   taasl    10 4.00      no         no
    ## 45    8   15   male  18                     bcs     9 4.00      no         no
    ## 46    8   15 female  19             engineering     9 4.00      no         no
    ## 47    8   15 female  18             engineering    12 4.00      no         no
    ## 48    8   15   male  24                     bcs    10 3.49      no        yes
    ## 49    8   15 female  24                     bcs    11 4.00      no         no
    ## 50    8   15 female  23                     ala     9 2.99     yes        yes
    ## 51    8   16 female  18                     bcs    10 4.00      no         no
    ## 52    8   16 female  19      biomedical science    11 3.49      no         no
    ## 53    8   16 female  20                     koe    11 3.49     yes        yes
    ## 54    8   16 female  19                     bcs     9 4.00      no        yes
    ## 55    8   16   male  21                     bcs     9 3.49      no         no
    ## 56    8   16   male  23                  kirkhs    11 4.00      no         no
    ## 57    8   16 female  20                    benl    11 3.49      no        yes
    ## 58    8   16 female  18                     bcs     9 4.00      no         no
    ## 59    8   17 female  23                    benl     9 3.49      no         no
    ## 60    8   17 female  18                      it    11 3.49      no         no
    ## 61    8   17 female  19                     bcs     9 4.00      no         no
    ## 62    8   17 female  18                     cts     9 4.00      no         no
    ## 63    8   18 female  24             engineering     9 4.00      no         no
    ## 64    8   18 female  24             engineering     9 4.00      no         no
    ## 65    8   18 female  23                   econs     9 4.00      no        yes
    ## 66    8   18 female  18                     koe    11 3.49      no         no
    ## 67    8   18   male  19                    mhsc    11 3.49     yes        yes
    ## 68    8   19 female  18                  malcom     9 4.00      no        yes
    ## 69    8   20 female  24                     kop    12 3.49      no         no
    ## 70    8   21 female  24      biomedical science     9 3.49      no         no
    ## 71    8   21 female  18                    laws    11 4.00      no         no
    ## 72    8   23 female  19                     bit    11 3.49     yes        yes
    ## 73    9    7   male  18      biomedical science     9 1.99      no         no
    ## 74    9   12   male  24                     bit    11 4.00      no         no
    ## 75    9   12 female  24                     koe     9 4.00      no         no
    ## 76    9   13 female  23             engineering     9 3.49      no        yes
    ## 77    9   18 female  18          human sciences    10 3.49      no         no
    ## 78   13   10 female  19           biotechnology    11 1.99      no         no
    ## 79   13   10 female  18             engineering    12 4.00      no         no
    ## 80   13   10 female  24           communication    10 4.00     yes        yes
    ## 81   13   10 female  24                 nursing    10 4.00      no         no
    ## 82   13   10 female  19             engineering     9 3.49      no        yes
    ## 83   13   10 female  19        pendidikan islam    10 3.49      no         no
    ## 84   13   10   male  23             radiography     9 3.49      no         no
    ## 85   13   11 female  18              psychology     9 4.00      no        yes
    ## 86   13   11 female  19                    fiqh    11 3.49      no         no
    ## 87   13   12 female  18              psychology     9 4.00      no        yes
    ## 88   13   12   male  24                     bit     9 3.49      no         no
    ## 89   13   12   male  24             engineering    10 2.49      no         no
    ## 90   13   14 female  23            diploma tesl    11 4.00      no         no
    ## 91   13   15   male  18                     koe    10 3.49      no         no
    ## 92   13   15 female  19                     koe    10 3.49     yes        yes
    ## 93   13   16 female  18                    benl     9 3.49      no        yes
    ## 94   13   18 female  24                    fiqh    11 1.99      no         no
    ## 95   13   19 female  18       islamic education     9 4.00      no         no
    ## 96   13   20 female  21                     bcs     9 4.00      no         no
    ## 97   13   21   male  18             engineering    10 3.49      no        yes
    ## 98   13   21 female  19                 nursing    11 4.00     yes        yes
    ## 99   13   21 female  23        pendidikan islam    12 4.00      no         no
    ## 100  18   20   male  20      biomedical science    10 3.49      no         no
    ##     anxiety panic_attack treatment
    ## 1        no          yes        no
    ## 2       yes           no        no
    ## 3       yes          yes        no
    ## 4        no           no        no
    ## 5        no           no        no
    ## 6        no          yes        no
    ## 7        no          yes        no
    ## 8       yes           no        no
    ## 9        no           no        no
    ## 10      yes          yes        no
    ## 11       no           no        no
    ## 12       no           no        no
    ## 13       no           no        no
    ## 14       no           no        no
    ## 15      yes           no        no
    ## 16       no           no        no
    ## 17       no           no        no
    ## 18      yes          yes        no
    ## 19      yes           no        no
    ## 20      yes          yes        no
    ## 21       no          yes        no
    ## 22       no           no        no
    ## 23       no           no        no
    ## 24       no           no        no
    ## 25      yes          yes        no
    ## 26       no           no        no
    ## 27       no           no        no
    ## 28      yes           no        no
    ## 29      yes          yes       yes
    ## 30       no           no        no
    ## 31       no           no        no
    ## 32       no           no        no
    ## 33       no          yes        no
    ## 34      yes           no       yes
    ## 35      yes          yes        no
    ## 36       no           no        no
    ## 37      yes           no        no
    ## 38      yes          yes        no
    ## 39       no           no        no
    ## 40       no          yes       yes
    ## 41      yes           no        no
    ## 42       no           no        no
    ## 43       no           no        no
    ## 44       no          yes        no
    ## 45      yes          yes        no
    ## 46      yes           no        no
    ## 47       no           no        no
    ## 48       no           no        no
    ## 49       no          yes        no
    ## 50       no          yes       yes
    ## 51      yes           no        no
    ## 52       no           no        no
    ## 53      yes          yes        no
    ## 54       no          yes       yes
    ## 55       no           no        no
    ## 56       no           no        no
    ## 57      yes           no        no
    ## 58       no           no        no
    ## 59       no           no        no
    ## 60       no          yes        no
    ## 61       no           no        no
    ## 62       no          yes        no
    ## 63       no          yes        no
    ## 64       no           no        no
    ## 65      yes           no        no
    ## 66      yes           no        no
    ## 67       no          yes        no
    ## 68       no           no        no
    ## 69      yes           no        no
    ## 70       no           no        no
    ## 71       no          yes        no
    ## 72       no           no        no
    ## 73       no           no        no
    ## 74      yes           no        no
    ## 75      yes          yes        no
    ## 76       no           no        no
    ## 77       no          yes        no
    ## 78       no           no        no
    ## 79       no           no        no
    ## 80      yes          yes        no
    ## 81       no           no        no
    ## 82      yes           no        no
    ## 83       no           no        no
    ## 84       no           no        no
    ## 85      yes           no       yes
    ## 86       no           no        no
    ## 87      yes          yes        no
    ## 88      yes           no        no
    ## 89       no          yes        no
    ## 90       no          yes        no
    ## 91      yes           no        no
    ## 92       no           no        no
    ## 93       no           no        no
    ## 94       no          yes        no
    ## 95       no           no        no
    ## 96      yes           no        no
    ## 97      yes           no        no
    ## 98       no          yes        no
    ## 99       no           no        no
    ## 100      no           no        no

``` r
train
```

    ##     day hour age grade  gpa gender married depression anxiety panic_attack
    ## 56    8   16  23    11 4.00      0       0          0       0            0
    ## 76    9   13  23     9 3.49      1       0          1       0            0
    ## 45    8   15  18     9 4.00      0       0          0       1            1
    ## 47    8   15  18    12 4.00      1       0          0       0            0
    ## 28    8   14  18    10 3.49      0       1          1       1            0
    ## 36    8   15  18     9 2.49      1       0          0       0            0
    ## 4     8   12  22    11 3.49      1       1          1       0            0
    ## 17    8   13  24    11 3.49      1       0          0       0            0
    ## 48    8   15  24    10 3.49      0       0          1       0            0
    ## 81   13   10  24    10 4.00      1       0          0       0            0
    ## 55    8   16  21     9 3.49      0       0          0       0            0
    ## 1     8   12  18     9 3.49      1       0          1       0            1
    ## 39    8   15  18     9 4.00      1       0          0       0            0
    ## 35    8   15  19     9 3.49      1       0          1       1            1
    ## 52    8   16  19    11 3.49      1       0          0       0            0
    ## 21    8   13  19     9 3.49      1       0          0       0            1
    ## 90   13   14  23    11 4.00      1       0          0       0            1
    ## 100  18   20  20    10 3.49      0       0          0       0            0
    ## 58    8   16  18     9 4.00      1       0          0       0            0
    ## 57    8   16  20    11 3.49      1       0          1       1            0
    ## 97   13   21  18    10 3.49      0       0          1       1            0
    ## 75    9   12  24     9 4.00      1       0          0       1            1
    ## 85   13   11  18     9 4.00      1       0          1       1            0
    ## 49    8   15  24    11 4.00      1       0          0       0            1
    ## 30    8   14  24    12 4.00      1       0          0       0            0
    ## 98   13   21  19    11 4.00      1       1          1       0            1
    ## 26    8   13  18     9 4.00      1       0          0       0            0
    ## 32    8   14  18    10 3.49      0       0          0       0            0
    ## 33    8   14  19     9 4.00      0       0          0       0            1
    ## 34    8   15  18    10 4.00      0       1          1       1            0
    ## 53    8   16  20    11 3.49      1       1          1       1            1
    ## 3     8   12  19     9 3.49      0       0          1       1            1
    ## 83   13   10  19    10 3.49      1       0          0       0            0
    ## 18    8   13  24    12 3.49      1       1          1       1            1
    ## 6     8   13  19    10 4.00      0       0          0       0            1
    ## 12    8   13  24    11 4.00      1       1          1       0            0
    ## 24    8   13  24     9 3.49      1       0          0       0            0
    ## 69    8   20  24    12 3.49      1       0          0       1            0
    ## 16    8   13  24    11 4.00      0       0          0       0            0
    ## 65    8   18  23     9 4.00      1       0          1       1            0
    ## 86   13   11  19    11 3.49      1       0          0       0            0
    ## 66    8   18  18    11 3.49      1       0          0       1            0
    ## 40    8   15  24    10 2.99      1       1          1       0            1
    ## 77    9   18  18    10 3.49      1       0          0       0            1
    ## 46    8   15  19     9 4.00      1       0          0       1            0
    ## 87   13   12  18     9 4.00      1       0          1       1            1
    ## 54    8   16  19     9 4.00      1       0          1       0            1
    ## 95   13   19  18     9 4.00      1       0          0       0            0
    ## 51    8   16  18    10 4.00      1       0          0       1            0
    ## 11    8   13  20     9 4.00      1       0          0       0            0
    ## 79   13   10  18    12 4.00      1       0          0       0            0
    ## 60    8   17  18    11 3.49      1       0          0       0            1
    ## 19    8   13  20    10 4.00      1       0          0       1            0
    ## 14    8   13  19     9 3.49      0       0          0       0            0
    ## 99   13   21  23    12 4.00      1       0          0       0            0
    ## 27    8   14  19     9 4.00      1       0          0       0            0
    ## 74    9   12  24    11 4.00      0       0          0       1            0
    ## 88   13   12  24     9 3.49      0       0          0       1            0
    ## 7     8   13  23    10 4.00      1       1          1       0            1
    ## 68    8   19  18     9 4.00      1       0          1       0            0
    ## 93   13   16  18     9 3.49      1       0          1       0            0
    ## 63    8   18  24     9 4.00      1       0          0       0            1
    ## 38    8   15  19     9 2.99      1       0          1       1            1
    ## 22    8   13  18    10 3.49      1       0          0       0            0
    ## 29    8   14  24    11 4.00      1       1          1       1            1
    ## 62    8   17  18     9 4.00      1       0          0       0            1
    ## 5     8   12  23    12 3.49      0       0          0       0            0
    ## 59    8   17  23     9 3.49      1       0          0       0            0
    ## 2     8   12  21    10 3.49      0       0          0       1            0
    ## 15    8   13  18    10 4.00      1       0          0       1            0
    ## 50    8   15  23     9 2.99      1       1          1       0            1
    ## 37    8   15  18    11 3.49      1       0          1       1            0
    ## 73    9    7  18     9 1.99      0       0          0       0            0
    ## 41    8   15  24    11 3.49      1       0          0       1            0
    ## 8     8   13  18     9 4.00      1       0          0       1            0
    ## 96   13   20  21     9 4.00      1       0          0       1            0
    ## 78   13   10  19    11 1.99      1       0          0       0            0
    ## 20    8   13  18    10 4.00      1       1          1       1            1
    ## 67    8   18  19    11 3.49      0       1          1       0            1
    ## 82   13   10  19     9 3.49      1       0          1       1            0
    ##     treatment engineering bcs
    ## 56          0           0   0
    ## 76          0           1   0
    ## 45          0           0   1
    ## 47          0           1   0
    ## 28          0           1   0
    ## 36          0           1   0
    ## 4           0           0   0
    ## 17          0           0   0
    ## 48          0           0   1
    ## 81          0           0   0
    ## 55          0           0   1
    ## 1           0           1   0
    ## 39          0           0   0
    ## 35          0           0   0
    ## 52          0           0   0
    ## 21          0           1   0
    ## 90          0           0   0
    ## 100         0           0   0
    ## 58          0           0   1
    ## 57          0           0   0
    ## 97          0           1   0
    ## 75          0           0   0
    ## 85          1           0   0
    ## 49          0           0   1
    ## 30          0           0   1
    ## 98          0           0   0
    ## 26          0           0   0
    ## 32          0           0   1
    ## 33          0           0   1
    ## 34          1           0   1
    ## 53          0           0   0
    ## 3           0           0   0
    ## 83          0           0   0
    ## 18          0           0   0
    ## 6           0           1   0
    ## 12          0           1   0
    ## 24          0           1   0
    ## 69          0           0   0
    ## 16          0           0   1
    ## 65          0           0   0
    ## 86          0           0   0
    ## 66          0           0   0
    ## 40          1           1   0
    ## 77          0           0   0
    ## 46          0           1   0
    ## 87          0           0   0
    ## 54          1           0   1
    ## 95          0           0   0
    ## 51          0           0   1
    ## 11          0           0   0
    ## 79          0           1   0
    ## 60          0           0   0
    ## 19          0           0   0
    ## 14          0           1   0
    ## 99          0           0   0
    ## 27          0           1   0
    ## 74          0           0   0
    ## 88          0           0   0
    ## 7           0           0   0
    ## 68          0           0   0
    ## 93          0           0   0
    ## 63          0           1   0
    ## 38          0           0   0
    ## 22          0           0   0
    ## 29          1           0   0
    ## 62          0           0   0
    ## 5           0           0   0
    ## 59          0           0   0
    ## 2           0           0   0
    ## 15          0           0   0
    ## 50          1           0   0
    ## 37          0           0   0
    ## 73          0           0   0
    ## 41          0           0   0
    ## 8           0           0   1
    ## 96          0           0   1
    ## 78          0           0   0
    ## 20          0           0   0
    ## 67          0           0   0
    ## 82          0           1   0

``` r
test
```

    ##    day hour age grade  gpa gender married depression anxiety panic_attack
    ## 9    8   13  19    10 2.99      1       0          0       0            0
    ## 10   8   13  18     9 4.00      0       0          0       1            1
    ## 13   8   13  18     9 3.49      1       0          1       0            0
    ## 23   8   13  24     9 4.00      1       0          0       0            0
    ## 25   8   13  23    11 4.00      1       0          1       1            1
    ## 31   8   14  23    10 3.49      1       0          0       0            0
    ## 42   8   15  22    12 4.00      1       0          0       0            0
    ## 43   8   15  20    10 3.49      1       0          1       0            0
    ## 44   8   15  23    10 4.00      0       0          0       0            1
    ## 61   8   17  19     9 4.00      1       0          0       0            0
    ## 64   8   18  24     9 4.00      1       0          0       0            0
    ## 70   8   21  24     9 3.49      1       0          0       0            0
    ## 71   8   21  18    11 4.00      1       0          0       0            1
    ## 72   8   23  19    11 3.49      1       1          1       0            0
    ## 80  13   10  24    10 4.00      1       1          1       1            1
    ## 84  13   10  23     9 3.49      0       0          0       0            0
    ## 89  13   12  24    10 2.49      0       0          0       0            1
    ## 91  13   15  18    10 3.49      0       0          0       1            0
    ## 92  13   15  19    10 3.49      1       1          1       0            0
    ## 94  13   18  24    11 1.99      1       0          0       0            1
    ##    treatment engineering bcs
    ## 9          0           0   0
    ## 10         0           0   0
    ## 13         0           0   1
    ## 23         0           0   1
    ## 25         0           0   1
    ## 31         0           0   0
    ## 42         0           1   0
    ## 43         0           0   0
    ## 44         0           0   0
    ## 61         0           0   1
    ## 64         0           1   0
    ## 70         0           0   0
    ## 71         0           0   0
    ## 72         0           0   0
    ## 80         0           0   0
    ## 84         0           0   0
    ## 89         0           1   0
    ## 91         0           0   0
    ## 92         0           0   0
    ## 94         0           0   0

``` r
head(train)
```

    ##    day hour age grade  gpa gender married depression anxiety panic_attack
    ## 56   8   16  23    11 4.00      0       0          0       0            0
    ## 76   9   13  23     9 3.49      1       0          1       0            0
    ## 45   8   15  18     9 4.00      0       0          0       1            1
    ## 47   8   15  18    12 4.00      1       0          0       0            0
    ## 28   8   14  18    10 3.49      0       1          1       1            0
    ## 36   8   15  18     9 2.49      1       0          0       0            0
    ##    treatment engineering bcs
    ## 56         0           0   0
    ## 76         0           1   0
    ## 45         0           0   1
    ## 47         0           1   0
    ## 28         0           1   0
    ## 36         0           1   0

``` r
ks = c()
train_accuracy = c()
test_accuracy = c()
binary_labels = c()

for (i in 6:ncol(train)){
  
  colname <- colnames(train)[i]
  if (colname == "treatment"){
    next
  }
  
  colnames(train)[i] <- "label"
  colnames(test)[i] <- "label"
  
  print(colname)
  
  glm_1 <- glm(label~., data=train, family="binomial")
  #predict(glm_1, train)
  plot(glm_1)
  
  for (k in 1:10){
    
    knn_fit <- knn3(label~., data=train, k=k) 
    p_hat_knn_train <- predict(knn_fit, train, type="prob")
    y_hat_knn_train <- factor(ifelse(p_hat_knn_train[,2]>0.5,1,0))
    confusionMatrix(y_hat_knn_train, factor(train$label))
    #print(confusionMatrix(y_hat_knn_train, factor(train$label))$overall["Accuracy"])
    
    p_hat_knn_test <- predict(knn_fit, test, type="prob")
    y_hat_knn_test <- factor(ifelse(p_hat_knn_test[,2]>0.5,1,0))
    confusionMatrix(y_hat_knn_test, factor(test$label))
    #print(confusionMatrix(y_hat_knn_test, factor(test$label))$overall["Accuracy"])
    
    ks <- c(ks, k)
    train_accuracy <- c(train_accuracy, confusionMatrix(y_hat_knn_train, factor(train$label))$overall["Accuracy"])
    test_accuracy <- c(test_accuracy, confusionMatrix(y_hat_knn_test, factor(test$label))$overall["Accuracy"])
    binary_labels <- c(binary_labels, colname)
  
  }
  
  colnames(train)[i] <- colname
  colnames(test)[i] <- colname
}
```

    ## [1] "gender"

![](Final-Project_files/figure-gfm/unnamed-chunk-49-1.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-2.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-3.png)<!-- -->

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## [1] "married"

    ## Warning: glm.fit: algorithm did not converge

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

![](Final-Project_files/figure-gfm/unnamed-chunk-49-4.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-5.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-6.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-7.png)<!-- -->

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_train, factor(train$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_train, factor(train$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## [1] "depression"

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

![](Final-Project_files/figure-gfm/unnamed-chunk-49-8.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-9.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-10.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-11.png)<!-- -->

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

![](Final-Project_files/figure-gfm/unnamed-chunk-49-12.png)<!-- -->

    ## [1] "anxiety"

![](Final-Project_files/figure-gfm/unnamed-chunk-49-13.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-14.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-15.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-16.png)<!-- -->

    ## [1] "panic_attack"

![](Final-Project_files/figure-gfm/unnamed-chunk-49-17.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-18.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-19.png)<!-- -->

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

![](Final-Project_files/figure-gfm/unnamed-chunk-49-20.png)<!-- -->

    ## [1] "engineering"

![](Final-Project_files/figure-gfm/unnamed-chunk-49-21.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-22.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-23.png)<!-- -->

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_train, factor(train$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_train, factor(train$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

![](Final-Project_files/figure-gfm/unnamed-chunk-49-24.png)<!-- -->

    ## [1] "bcs"

![](Final-Project_files/figure-gfm/unnamed-chunk-49-25.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-26.png)<!-- -->![](Final-Project_files/figure-gfm/unnamed-chunk-49-27.png)<!-- -->

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

    ## Warning in confusionMatrix.default(y_hat_knn_test, factor(test$label)): Levels
    ## are not in the same order for reference and data. Refactoring data to match.

![](Final-Project_files/figure-gfm/unnamed-chunk-49-28.png)<!-- -->

``` r
binary_labels
```

    ##  [1] "gender"       "gender"       "gender"       "gender"       "gender"      
    ##  [6] "gender"       "gender"       "gender"       "gender"       "gender"      
    ## [11] "married"      "married"      "married"      "married"      "married"     
    ## [16] "married"      "married"      "married"      "married"      "married"     
    ## [21] "depression"   "depression"   "depression"   "depression"   "depression"  
    ## [26] "depression"   "depression"   "depression"   "depression"   "depression"  
    ## [31] "anxiety"      "anxiety"      "anxiety"      "anxiety"      "anxiety"     
    ## [36] "anxiety"      "anxiety"      "anxiety"      "anxiety"      "anxiety"     
    ## [41] "panic_attack" "panic_attack" "panic_attack" "panic_attack" "panic_attack"
    ## [46] "panic_attack" "panic_attack" "panic_attack" "panic_attack" "panic_attack"
    ## [51] "engineering"  "engineering"  "engineering"  "engineering"  "engineering" 
    ## [56] "engineering"  "engineering"  "engineering"  "engineering"  "engineering" 
    ## [61] "bcs"          "bcs"          "bcs"          "bcs"          "bcs"         
    ## [66] "bcs"          "bcs"          "bcs"          "bcs"          "bcs"

``` r
ks
```

    ##  [1]  1  2  3  4  5  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10  1  2  3  4  5
    ## [26]  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10
    ## [51]  1  2  3  4  5  6  7  8  9 10  1  2  3  4  5  6  7  8  9 10

``` r
train_accuracy
```

    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   1.0000   0.8625   0.7750   0.7250   0.7750   0.7375   0.7625   0.7625 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   0.7625   0.7625   1.0000   0.9000   0.9125   0.9000   0.9125   0.8875 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   0.9000   0.8625   0.8500   0.8375   1.0000   0.8000   0.8375   0.7500 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   0.7500   0.7125   0.7125   0.7250   0.7250   0.6875   1.0000   0.7625 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   0.7875   0.7000   0.7250   0.6625   0.6250   0.6500   0.6375   0.6125 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   1.0000   0.8000   0.8000   0.7250   0.7125   0.7250   0.7375   0.7000 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   0.7000   0.6625   1.0000   0.8750   0.8500   0.8000   0.7750   0.7875 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   0.8000   0.8000   0.8000   0.7875   1.0000   0.8500   0.8375   0.8250 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##   0.8625   0.8625   0.8375   0.8125   0.8250   0.8500

``` r
test_accuracy
```

    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.75     0.60     0.80     0.80     0.75     0.75     0.75     0.75 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.75     0.75     0.80     0.80     0.80     0.80     0.80     0.80 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.80     0.80     0.85     0.85     0.65     0.75     0.80     0.70 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.70     0.70     0.60     0.65     0.65     0.70     0.60     0.65 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.65     0.70     0.75     0.75     0.80     0.75     0.75     0.75 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.55     0.65     0.55     0.60     0.55     0.70     0.70     0.70 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.65     0.65     0.80     0.75     0.80     0.85     0.85     0.85 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.85     0.85     0.85     0.85     0.75     0.80     0.75     0.85 
    ## Accuracy Accuracy Accuracy Accuracy Accuracy Accuracy 
    ##     0.85     0.80     0.80     0.80     0.80     0.80

``` r
knn_dataframe <- data.frame(ks,train_accuracy,test_accuracy,binary_labels)
knn_dataframe
```

    ##    ks train_accuracy test_accuracy binary_labels
    ## 1   1         1.0000          0.75        gender
    ## 2   2         0.8625          0.60        gender
    ## 3   3         0.7750          0.80        gender
    ## 4   4         0.7250          0.80        gender
    ## 5   5         0.7750          0.75        gender
    ## 6   6         0.7375          0.75        gender
    ## 7   7         0.7625          0.75        gender
    ## 8   8         0.7625          0.75        gender
    ## 9   9         0.7625          0.75        gender
    ## 10 10         0.7625          0.75        gender
    ## 11  1         1.0000          0.80       married
    ## 12  2         0.9000          0.80       married
    ## 13  3         0.9125          0.80       married
    ## 14  4         0.9000          0.80       married
    ## 15  5         0.9125          0.80       married
    ## 16  6         0.8875          0.80       married
    ## 17  7         0.9000          0.80       married
    ## 18  8         0.8625          0.80       married
    ## 19  9         0.8500          0.85       married
    ## 20 10         0.8375          0.85       married
    ## 21  1         1.0000          0.65    depression
    ## 22  2         0.8000          0.75    depression
    ## 23  3         0.8375          0.80    depression
    ## 24  4         0.7500          0.70    depression
    ## 25  5         0.7500          0.70    depression
    ## 26  6         0.7125          0.70    depression
    ## 27  7         0.7125          0.60    depression
    ## 28  8         0.7250          0.65    depression
    ## 29  9         0.7250          0.65    depression
    ## 30 10         0.6875          0.70    depression
    ## 31  1         1.0000          0.60       anxiety
    ## 32  2         0.7625          0.65       anxiety
    ## 33  3         0.7875          0.65       anxiety
    ## 34  4         0.7000          0.70       anxiety
    ## 35  5         0.7250          0.75       anxiety
    ## 36  6         0.6625          0.75       anxiety
    ## 37  7         0.6250          0.80       anxiety
    ## 38  8         0.6500          0.75       anxiety
    ## 39  9         0.6375          0.75       anxiety
    ## 40 10         0.6125          0.75       anxiety
    ## 41  1         1.0000          0.55  panic_attack
    ## 42  2         0.8000          0.65  panic_attack
    ## 43  3         0.8000          0.55  panic_attack
    ## 44  4         0.7250          0.60  panic_attack
    ## 45  5         0.7125          0.55  panic_attack
    ## 46  6         0.7250          0.70  panic_attack
    ## 47  7         0.7375          0.70  panic_attack
    ## 48  8         0.7000          0.70  panic_attack
    ## 49  9         0.7000          0.65  panic_attack
    ## 50 10         0.6625          0.65  panic_attack
    ## 51  1         1.0000          0.80   engineering
    ## 52  2         0.8750          0.75   engineering
    ## 53  3         0.8500          0.80   engineering
    ## 54  4         0.8000          0.85   engineering
    ## 55  5         0.7750          0.85   engineering
    ## 56  6         0.7875          0.85   engineering
    ## 57  7         0.8000          0.85   engineering
    ## 58  8         0.8000          0.85   engineering
    ## 59  9         0.8000          0.85   engineering
    ## 60 10         0.7875          0.85   engineering
    ## 61  1         1.0000          0.75           bcs
    ## 62  2         0.8500          0.80           bcs
    ## 63  3         0.8375          0.75           bcs
    ## 64  4         0.8250          0.85           bcs
    ## 65  5         0.8625          0.85           bcs
    ## 66  6         0.8625          0.80           bcs
    ## 67  7         0.8375          0.80           bcs
    ## 68  8         0.8125          0.80           bcs
    ## 69  9         0.8250          0.80           bcs
    ## 70 10         0.8500          0.80           bcs

``` r
ggplot(knn_dataframe, aes(x=ks)) +
  geom_line(aes(y = train_accuracy), color = "red") + 
  geom_line(aes(y = test_accuracy), color = "blue") +
  facet_wrap(~binary_labels)
```

![](Final-Project_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->
