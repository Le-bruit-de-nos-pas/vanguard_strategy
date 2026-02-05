library(tidyverse, data.table)
options(scipen = 999)

ccpts_18plus_sel_Visibility_Stock_histories <- fread("ccpts_18plus_sel_Visibility_Stock_histories.txt", colClasses = "character")
ccpts_18plus_sel_Visibility_Stock_histories <- gather(ccpts_18plus_sel_Visibility_Stock_histories, Month, Stock, month1:month75, factor_key=TRUE)

ccpts_18plus_sel_Visibility_Stock_histories$Month <- parse_number(as.character(ccpts_18plus_sel_Visibility_Stock_histories$Month))

ccpts_18plus_sel_Visibility_Stock_histories <- data.table(ccpts_18plus_sel_Visibility_Stock_histories)

Starts <- ccpts_18plus_sel_Visibility_Stock_histories[ccpts_18plus_sel_Visibility_Stock_histories$Stock %in% c("I", "O"), ]
Starts <- Starts[, .SD[Month == min(Month)], by = PATIENTID]
Starts <- Starts[, .(PATIENTID, Start = Month)] 

Ends <- ccpts_18plus_sel_Visibility_Stock_histories[ccpts_18plus_sel_Visibility_Stock_histories$Stock %in% c("I", "O"), ]
Ends <- Ends[, .SD[Month == max(Month)], by = PATIENTID]
Ends <- Ends[, .(PATIENTID, End = Month)] 

Left <- ccpts_18plus_sel_Visibility_Stock_histories[Starts, on = .(PATIENTID), nomatch = 0]
Left <- Left[Ends, on = .(PATIENTID), nomatch = 0]
Left <- Left[Month >= Start & Month <= End]
Left <- Left[, .(Total = .N), by = PATIENTID]

Right <- ccpts_18plus_sel_Visibility_Stock_histories[Starts, on = .(PATIENTID), nomatch = 0]
Right <- Right[Ends, on = .(PATIENTID), nomatch = 0]
Right <- Right[Month >= Start & Month <= End & (Stock=="I"|Stock=="O")] 
Right <- Right[, .(n = .N), by = PATIENTID]

temp <- Left[Right, on = .(PATIENTID), nomatch = 0]
temp[, percent := 100 * n / Total]

mean(temp$percent) ; median(temp$percent) # 71.78  ; 75

hist(temp$percent, breaks = 40, col = "midnightblue", border = "white", xlab = "Visibility Percentage", ylab = "Patient Count")

temp[, ind := 1/613276][order(percent), cum := cumsum(ind)]

plot(temp$percent, 100 * temp$cum, type = "l", lwd = 2, col = "black", xlab = "Visibility Percentage", ylab = "Patient Count Cumulative %")

plot(density(Right$n, na.rm = TRUE), main = "", xlab = "Total True Visibility (months)", ylab = "Patient Count", col = "firebrick", fill="firebrick", lwd = 2)



ccpts_18plus_sel_Visibility_Stock_histories <- fread("ccpts_18plus_sel_Visibility_Stock_histories.txt", colClasses = "character")
ccpts_18plus_sel_Visibility_Stock_histories <- gather(ccpts_18plus_sel_Visibility_Stock_histories, Month, Stock, month1:month75, factor_key=TRUE)

ccpts_18plus_sel_Visibility_Stock_histories$Month <- parse_number(as.character(ccpts_18plus_sel_Visibility_Stock_histories$Month))

ccpts_18plus_sel_Visibility_Stock_histories <- data.table(ccpts_18plus_sel_Visibility_Stock_histories)

length(unique(ccpts_18plus_sel_Visibility_Stock_histories$PATIENTID)) #613276

unique_patients <- ccpts_18plus_sel_Visibility_Stock_histories[Stock == "T", unique(PATIENTID)]
unique_patients <- data.table(unique_patients)
names(unique_patients)[1] <- "PATIENTID"

ccpts_18plus_sel_Visibility_Stock_histories <- unique_patients[ccpts_18plus_sel_Visibility_Stock_histories, on = .(PATIENTID), nomatch = 0]

length(unique(ccpts_18plus_sel_Visibility_Stock_histories$PATIENTID)) #151638

ccpts_18plus_sel_Visibility_Stock_histories[, Stock := ifelse(Stock %in% c("T", "D"), Stock, "B")]

ccpts_18plus_sel_Visibility_Stock_histories[, grp :=  rle(Stock)$lengths %>% {rep(seq(length(.)), .)}, by = PATIENTID]

Ended <- ccpts_18plus_sel_Visibility_Stock_histories[Month == 60 & Stock == "T", .(PATIENTID, grp) ]

length(unique(Ended$PATIENTID)) ; dim(Ended)[1] # 55936 

T_durations <- ccpts_18plus_sel_Visibility_Stock_histories[Stock == "T", .(PATIENTID, grp) ]
T_durations <- T_durations[, .(n = .N), by = .(PATIENTID, grp)]
T_durations <- T_durations[!Ended, on = .(PATIENTID, grp)]

length(unique(T_durations$n)) # 56

mean(T_durations$n) ; median(T_durations$n)  # 4.525431 ; 2

hist(T_durations$n, breaks = 56, col = "midnightblue", border = "white", xlab = "Total Transfer Period Duration (months)", ylab = "Episode Count")
