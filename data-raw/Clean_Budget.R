library(haven)
library(xlsx)
library(dplyr)
library(ggplot2)
library(vietnamcode)
library(vietnamdata)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/Budget/")
#source("../../Code/Functions.R")

#### Import Budget Plans and Finals

# guides contains info on which rows/columns to keep
guide.plan <- read.csv("Plan_Import.csv", stringsAsFactors=F)
guide.final <- read.csv("Final_Import.csv", stringsAsFactors=F)
guide.prov <- read.csv("../Vietnam Provincial Budget/Province_Import.csv", stringsAsFactors=F)

# read in budget and plan finals for every year using guides
plan <- do.call(rbind, lapply(1:nrow(guide.plan), function(i) {
  x<- guide.plan[i,]

  file.plan <- paste("Plan_", x$year, ".xls", sep="")

  plan <- read.xlsx2(file.plan, sheetIndex=x$sheetIndex, startRow=x$startRow,
                     colClasses=c("numeric", "character", rep("numeric", 10)), encoding="UTF-8")
  plan <- plan[find.numeric(plan[,1]), eval(parse(text=paste("c(",x$keepRow,")", sep="")))]
  plan[,1] <- as.character(plan[,1])

  names(plan) <- c("prov", "total.rev", "ratio", "total.exp", "balance.trans", "target.trans", "project.trans")

  plan[is.nan(plan)] <- 0
  plan$net.trans <- plan$total.exp - plan$total.rev
  plan$trans_rev <- plan$total.exp/plan$total.rev * 100 # as specified by Malesky and Schuler (2011)
  plan$year <- x$year

  return(plan)
}))

final <- do.call(rbind, lapply(1:nrow(guide.final), function(i) {
  x<- guide.final[i,]

  file.final <- paste("Final_", x$year, ".xls", sep="")

  final <- read.xlsx2(file.final, sheetIndex=x$sheetIndex, startRow=x$startRow,
                     colClasses=c("numeric", "character", rep("numeric", 9)), encoding="UTF-8")
  final <- final[find.numeric(final[,1]), eval(parse(text=paste("c(",x$keepRow,")", sep="")))]
  final[,1] <- as.character(final[,1])

  # for 2011-2013, target transfer is reported in 2 columns
  if(x$year > 2010) final[,6] <- final[,6] + final[,7]
  final[,7] <- NULL

  names(final) <- c("prov", "total.rev", "ratio", "total.exp", "balance.trans", "target.trans")

  final[is.nan(final)] <- 0
  final$net.trans <- final$total.exp - final$total.rev
  final$trans_rev <- final$total.exp/final$total.rev * 100 # as specified by Malesky and Schuler (2011)
  final$year <- x$year

  return(final)
}))

prov <- do.call(rbind, lapply(1:nrow(guide.prov), function(i) {
  x<- guide.prov[i,]

  file.prov <- paste("../Vietnam Provincial Budget/MOF Budget ", x$year, ".xlsx", sep="")

  prov <- read.xlsx2(file.prov, sheetIndex=x$sheetIndex, startRow=x$startRow,
                      colClasses=c("numeric", "numeric", "character", rep("numeric", 3)))
  prov <- prov[find.numeric(prov[,2]), eval(parse(text=paste("c(",x$keepRow,")", sep="")))]
  prov[,2] <- as.character(prov[,2])

  names(prov) <- c("year", "prov", "total.exp", "dev.exp", "admin.exp")

  prov[is.nan(prov)] <- NA

  return(prov)
}))

# clean up province names using a look up table
# provnames <- read.csv("Provinces.csv", stringsAsFactors=F)
#
# plan$prov <- lookup.clean(plan$prov, provnames)
# final$prov <- lookup.clean(final$prov, provnames)
# prov$prov <- lookup.clean(prov$prov, provnames)

# clean up province names using the Vietnam Code function
plan$prov <- vietnamcode(plan$prov, origin = "province_name", destination = "province_name")
final$prov <- vietnamcode(final$prov, origin = "province_name", destination = "province_name")
prov$prov <- vietnamcode(prov$prov, origin = "province_name", destination = "province_name")

# create leads and lags, changes, percentage changes variables
plan <- plan %>%
  group_by(prov) %>%
  mutate(net.trans.lag = lag(net.trans, order_by=year)) %>%
  mutate(trans_rev.lag = lag(trans_rev, order_by=year)) %>%
  mutate(project.trans.lag = lag(project.trans, order_by=year)) %>%
  mutate(total.rev.lag = lag(total.rev, order_by=year)) %>%
  mutate(total.rev.change = total.rev - total.rev.lag) %>%
  mutate(total.rev.change.log = neglog(total.rev.change)) %>%
  mutate(total.rev.change.pct = total.rev.change/abs(total.rev.lag)) %>%
  mutate(net.trans.change = net.trans - net.trans.lag) %>%
  mutate(net.trans.change.log = neglog(net.trans.change)) %>%
  mutate(net.trans.change.pct = net.trans.change/abs(net.trans.lag)) %>%
  mutate(net.trans.change.lag = lag(net.trans.change, order_by=year)) %>%
  mutate(trans_rev.change = trans_rev - trans_rev.lag) %>%
  mutate(trans_rev.change.log = neglog(trans_rev.change)) %>%
  mutate(trans_rev.change.pct = trans_rev.change/abs(trans_rev.lag)) %>%
  mutate(project.trans.change = project.trans - project.trans.lag) %>%
  mutate(project.trans.change.log = neglog(project.trans.change)) %>%
  mutate(project.trans.change.pct = project.trans.change/abs(project.trans.lag)) %>%
  mutate(pct.change.rev = net.trans.change/total.rev) %>%
  # 2 year lag
  mutate(net.trans.lag2 = lag(net.trans, n=2, order_by=year)) %>%
  mutate(trans_rev.lag2 = lag(trans_rev, n=2, order_by=year)) %>%
  mutate(project.trans.lag2 = lag(project.trans, n=2, order_by=year)) %>%
  mutate(net.trans.change.lag2 = lag(net.trans.change, n=2, order_by = year)) %>%
  mutate(total.rev.lag2 = lag(total.rev, n=2, order_by=year)) %>%
  mutate(total.rev.change2 = total.rev - total.rev.lag2) %>%
  mutate(total.rev.change2.log = neglog(total.rev.change2)) %>%
  mutate(total.rev.change2.pct = total.rev.change2/abs(total.rev.lag2)) %>%
  mutate(net.trans.change2 = net.trans - net.trans.lag2) %>%
  mutate(net.trans.change2.log = neglog(net.trans.change2)) %>%
  mutate(net.trans.change2.pct = net.trans.change2/abs(net.trans.lag2)) %>%
  mutate(trans_rev.change2 = trans_rev - trans_rev.lag2) %>%
  mutate(trans_rev.change2.log = neglog(trans_rev.change2)) %>%
  mutate(trans_rev.change2.pct = trans_rev.change2/abs(trans_rev.lag2)) %>%
  mutate(project.trans.change2 = project.trans - project.trans.lag2) %>%
  mutate(project.trans.change2.log = neglog(project.trans.change2)) %>%
  mutate(project.trans.change2.pct = project.trans.change/abs(project.trans.lag2))


final <- final %>%
  group_by(prov) %>%
  mutate(net.trans.lag = lag(net.trans, order_by=year)) %>%
  mutate(trans_rev.lag = lag(trans_rev, order_by=year)) %>%
  mutate(total.rev.lag = lag(total.rev, order_by=year)) %>%
  mutate(total.rev.change = total.rev - total.rev.lag) %>%
  mutate(total.rev.change.log = neglog(total.rev.change)) %>%
  mutate(total.rev.change.pct = total.rev.change/abs(total.rev.lag)) %>%
  mutate(net.trans.change = net.trans - net.trans.lag) %>%
  mutate(net.trans.change.log = neglog(net.trans.change)) %>%
  mutate(net.trans.change.pct = net.trans.change/abs(net.trans.lag)) %>%
  mutate(net.trans.change.lag = lag(net.trans.change, order_by=year)) %>%
  mutate(trans_rev.change = trans_rev - trans_rev.lag) %>%
  mutate(trans_rev.change.log = neglog(trans_rev.change)) %>%
  mutate(trans_rev.change.pct = trans_rev.change/abs(trans_rev.lag)) %>%
  mutate(pct.change.rev = net.trans.change/total.rev)%>%
  # 2 year lag
  mutate(net.trans.lag2 = lag(net.trans, n=2, order_by=year)) %>%
  mutate(trans_rev.lag2 = lag(trans_rev, n=2, order_by=year)) %>%
  mutate(net.trans.change.lag2 = lag(net.trans.change, n=2, order_by = year)) %>%
  mutate(total.rev.lag2 = lag(total.rev, n=2, order_by=year)) %>%
  mutate(total.rev.change2 = total.rev - total.rev.lag2) %>%
  mutate(total.rev.change2.log = neglog(total.rev.change2)) %>%
  mutate(total.rev.change2.pct = total.rev.change2/abs(total.rev.lag2)) %>%
  mutate(net.trans.change2 = net.trans - net.trans.lag2) %>%
  mutate(net.trans.change2.log = neglog(net.trans.change2)) %>%
  mutate(net.trans.change2.pct = net.trans.change2/abs(net.trans.lag2)) %>%
  mutate(trans_rev.change2 = trans_rev - trans_rev.lag2) %>%
  mutate(trans_rev.change2.log = neglog(trans_rev.change2)) %>%
  mutate(trans_rev.change2.pct = trans_rev.change2/abs(trans_rev.lag2))

prov <- prov %>%
  group_by(prov) %>%
  mutate(total.exp.lag = lag(total.exp, order_by=year)) %>%
  mutate(dev.exp.lag = lag(dev.exp, order_by=year)) %>%
  mutate(admin.exp.lag = lag(admin.exp, order_by=year)) %>%
  mutate(admin.exp.change = admin.exp - admin.exp.lag) %>%
  mutate(admin.exp.change.log = neglog(admin.exp.change)) %>%
  mutate(admin.exp.change.pct = admin.exp.change/abs(admin.exp.lag)) %>%
  mutate(total.exp.change = total.exp - total.exp.lag) %>%
  mutate(total.exp.change.log = neglog(total.exp.change)) %>%
  mutate(total.exp.change.pct = total.exp.change/abs(total.exp.lag)) %>%
  mutate(dev.exp.change = dev.exp - dev.exp.lag) %>%
  mutate(dev.exp.change.log = neglog(dev.exp.change)) %>%
  mutate(dev.exp.change.pct = dev.exp.change/abs(dev.exp.lag)) %>%
  mutate(pct.change.rev = total.exp.change/admin.exp)%>%
  # 2 year lag
  mutate(total.exp.lag2 = lag(total.exp, n=2, order_by=year)) %>%
  mutate(dev.exp.lag2 = lag(dev.exp, n=2, order_by=year)) %>%
  mutate(admin.exp.lag2 = lag(admin.exp, n=2, order_by=year)) %>%
  mutate(admin.exp.change2 = admin.exp - admin.exp.lag2) %>%
  mutate(admin.exp.change2.log = neglog(admin.exp.change2)) %>%
  mutate(admin.exp.change2.pct = admin.exp.change2/abs(admin.exp.lag2)) %>%
  mutate(total.exp.change2 = total.exp - total.exp.lag2) %>%
  mutate(total.exp.change2.log = neglog(total.exp.change2)) %>%
  mutate(total.exp.change2.pct = total.exp.change2/abs(total.exp.lag2)) %>%
  mutate(dev.exp.change2 = dev.exp - dev.exp.lag2) %>%
  mutate(dev.exp.change2.log = neglog(dev.exp.change2)) %>%
  mutate(dev.exp.change2.pct = dev.exp.change2/abs(dev.exp.lag2))

# indicators for southern provinces
south <- read.csv("south.csv", stringsAsFactors=F)

plan <- merge(plan, south, by="prov", all.x=T)

final <- merge(final, south, by="prov", all.x=T)

prov <- merge(prov, south, by="prov", all.x=T)

# merge in revenue variables from plan to prov
prov <- prov %>%
  merge(plan %>% select(total.rev, total.rev.change, total.rev.lag, prov, year), by = c("prov", "year"))

# some plots

ggplot(final, aes(y=net.trans/total.rev, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()

ggplot(final, aes(y=total.rev, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()

ggplot(plan, aes(y=pct.change.rev, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()

ggplot(prov, aes(y=dev.exp, x = factor(year), group=prov)) +
  geom_line() +
  theme_bw()
