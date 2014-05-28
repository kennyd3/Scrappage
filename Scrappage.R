library(plyr)
library(reshape)

#prior month
mo <- as.numeric(format(Sys.Date(),"%m"))
yr <- as.numeric(format(Sys.Date(),"%y"))
#get prior month if prior month is december
if(mo == 1){
  mo <- "12"
  yr <- yr - 1
#else if prior month is anything other than december
} else if(length(mo) == 1){
  mo <- as.character(paste(0,mo-1,sep=""))
} else mo <- as.character(mo-1)


#read in data
scrap <- read.csv("C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Scrappage/Downloads/curr.csv", header=TRUE)
#change VH to CA
scrap$Loc.[scrap$Loc. == 180000]<-170000

#Get only pertinent columns
loc <- which(colnames(scrap)=="Loc.")
gra <- which(colnames(scrap)=="GA.")
rcd <- which(colnames(scrap)=="Reason..Code")
rds <- which(colnames(scrap)=="Reason..Description")
itm <- which(colnames(scrap)=="Item")
ids <- which(colnames(scrap)=="Description")
tqy <- which(colnames(scrap)=="Trans..Qty")
teq <- which(colnames(scrap)=="Trans..EQUs")
exc <- which(colnames(scrap)=="Extended..Cost")
tdt <- which(colnames(scrap)=="Trans..Date")
cat <- which(colnames(scrap)=="GL..Class")
#put column numbers in vector
clmns <- c(loc,gra,rcd,rds,itm,ids,tqy,teq,exc,tdt,cat)
#create new matrix with just pertinent columns 
scrap.1 <- scrap[,clmns]
#delete unneeded rows
scrap.2 <- scrap.1[!(scrap.1$Reason..Code==""),]
#create columns for month and year
###find length of date and use if to get dates
dt.1 <- as.numeric(substring(as.character(scrap.2$Trans..Date[1]),7,8))
#to handle whether date is held like "xx/xx/xx" or like "xxxxxxxxx"
if(dt.1 + 1 == yr){
  scrap.2$st.dt <- as.character(scrap.2$Trans..Date)
  scrap.2$mo <- substring(scrap.2$st.dt, 1, 2)
  scrap.2$yr <- substring(scrap.2$st.dt, 7, 8)
}else{
  library(date)
  scrap.2$st.dt <- as.Date(scrap.2$Trans..Date,origin="1899-12-30") 
  #start month
  scrap.2$mo <- format(scrap.2$st.dt,"%m")
  #start year
  scrap.2$yr <- format(scrap.2$st.dt,"%y")
}
#remove comma characters so that columns are no longer factors and can have functions applied
scrap.2$Trans..Qty <- as.numeric(gsub(",","", scrap.2$Trans..Qty))
scrap.2$Trans..EQUs <- as.numeric(gsub(",","", scrap.2$Trans..EQUs))
scrap.2$Extended..Cost <- as.numeric(gsub(",","", scrap.2$Extended..Cost))

#set up reason column
scrap.2 <- transform(scrap.2,
                 Reason=ifelse(Reason..Code == 7,"Experimental Plants",
                        ifelse(Reason..Code %in% 10:19,"Dead",
                        ifelse(Reason..Code %in% 20:29,"Disease",
                        ifelse(Reason..Code %in% 30:39,"Insects",
                        ifelse(Reason..Code %in% 40:49,"Weeds",
                        ifelse(Reason..Code %in% 50:59,"Poor Quality",
                        ifelse(Reason..Code %in% 60:69,"Surplus Stock",
                        ifelse(Reason..Code %in% 70:79,"Fertilizer Damage","Prop Yield" ) 
                        ))))))))

######################################
#REASON DATA
######################################

library(data.table)
a <- data.table(loc=scrap.2$Loc.,  
                month=scrap.2$mo, 
                year=scrap.2$yr,
                grw=scrap.2$GA.,
                equ=scrap.2$Trans..EQUs,
                cost=scrap.2$Extended..Cost,
                Reason=scrap.2$Reason,
                cat=scrap.2$GL..Class)

gr.or <- c(500001,500003,500004,500007)
gr.ga <- c(500001,500002,500007)

#function for last month numbers
last.mo <- function(yr,location=NULL,grow.area=NULL,n=1){
  gr.rng <- 1:length(grow.area)
  kbys <- c("Reason","cat")
  mo <- as.numeric(format(Sys.Date(),"%m"))
  if(mo == 1){
    mo <- "12"
  } else if(length(mo) == 1){
    mo <- as.character(paste(0,mo-1,sep=""))
  } else mo <- as.character(mo-1)
  cnt <- 0
  outPut <- list()
  if(is.null(location)){
    temp.a <- a[year == yr & month == mo, 
                list(EQUs=sum(as.numeric(equ)),Cost=sum(as.numeric(cost))), 
                keyby = eval(kbys[n],all=TRUE)]
    #add totals row
    c1 <- "zTotals"
    c2 <- sum(temp.a$EQUs)
    c3 <- sum(temp.a$Cost)
    temp.a$Perc.Tot <- temp.a$Cost/c3
    c4 <- c2/c2
    if(n==1){
      newrow <- data.frame(Reason = c1, EQUs = c2, Cost = c3, Perc.Tot = c4)
    } else newrow <- data.frame(cat = c1, EQUs = c2, Cost = c3, Perc.Tot = c4)
    temp.a <- rbind(temp.a, newrow)
    cnt <- cnt + 1
    outPut[[cnt]] <- temp.a
  } else {
    for(i in gr.rng){
      temp.a <- a[loc == location & year == yr & month == mo & grw == grow.area[i], 
                  list(EQUs=sum(as.numeric(equ)),Cost=sum(as.numeric(cost))), 
                  keyby = eval(kbys[n],all=TRUE)]
      #add totals row
      c1 <- "zTotals"
      c2 <- sum(temp.a$EQUs)
      c3 <- sum(temp.a$Cost)
      temp.a$Perc.Tot <- temp.a$Cost/c3
      c4 <- c2/c2
      if(n==1){
        newrow <- data.frame(Reason = c1, EQUs = c2, Cost = c3, Perc.Tot = c4)
      } else newrow <- data.frame(cat = c1, EQUs = c2, Cost = c3, Perc.Tot = c4)
      temp.a <- rbind(temp.a, newrow)
      cnt <- cnt + 1
      outPut[[cnt]] <- temp.a
    }
  }
  return(outPut)
}
#function for YTD cumulative numbers
year.data <- function(yr,location=NULL,grow.area=NULL,b=1){
  gr.rng <- 1:length(grow.area)
  kbys <- c("Reason","cat")
  mo <- as.numeric(format(Sys.Date(),"%m"))-1
  mos <- 1:mo
  for(i in 1:mo){
    if(length(mos[i]) == 1){
      temp.mo <- as.character(paste(0,mos[i],sep=""))
    } else temp.mo <- as.character(mos[i])
    mos[i] <- temp.mo
  }
  cnt <- 0
  outPut <- list()
  if(!is.null(location)){
    for(i in gr.rng){
      for(n in 1:mo){
        mnth <- mos[n]
        temp.a <- a[loc == location & year == yr & month == mnth & grw == grow.area[i], 
                    list(EQUs=sum(as.numeric(equ)),Cost=sum(as.numeric(cost))), 
                    keyby = eval(kbys[b],all=TRUE)]
        if(n > 1){
          mrg.1 <- as.data.frame(merge(temp.a,temp.b,by=kbys[b],all=TRUE))
          eqs <- ddply(mrg.1,kbys[b], summarise, EQUs=sum(EQUs.x,EQUs.y,na.rm=TRUE))
          cst <- ddply(mrg.1,kbys[b], summarise, Cost=sum(Cost.x,Cost.y,na.rm=TRUE))
          temp.a <- as.data.frame(merge(eqs,cst,by=kbys[b],all=TRUE))
        }
        temp.b <- temp.a
      }
      #add totals row
      c1 <- "zTotals"
      c2 <- sum(temp.a$EQUs)
      c3 <- sum(temp.a$Cost)
      temp.a$Perc.Tot <- temp.a$Cost/c3  
      c4 <- c2/c2
      if(b==1){
        newrow <- data.frame(Reason = c1, EQUs = c2, Cost = c3, Perc.Tot = c4)
      } else newrow <- data.frame(cat = c1, EQUs = c2, Cost = c3, Perc.Tot = c4)
      temp.a <- rbind(temp.a, newrow)
      outPut[[i]] <- temp.a
    }
  } else {
    for(n in 1:mo){
      mnth <- mos[n]
      temp.a <- a[year == yr & month == mnth, 
                  list(EQUs=sum(as.numeric(equ)),Cost=sum(as.numeric(cost))), 
                  keyby = eval(kbys[b],all=TRUE)]
      if(n > 1){
        mrg.1 <- as.data.frame(merge(temp.a,temp.b,by=kbys[b],all=TRUE))
        eqs <- ddply(mrg.1,kbys[b], summarise, EQUs=sum(EQUs.x,EQUs.y,na.rm=TRUE))
        cst <- ddply(mrg.1,kbys[b], summarise, Cost=sum(Cost.x,Cost.y,na.rm=TRUE))
        temp.a <- as.data.frame(merge(eqs,cst,by=kbys[b],all=TRUE))
      }
      temp.b <- temp.a
    }
    #add totals row
    c1 <- "zTotals"
    c2 <- sum(temp.a$EQUs)
    c3 <- sum(temp.a$Cost)
    temp.a$Perc.Tot <- temp.a$Cost/c3  
    c4 <- c2/c2
    if(b==1){
      newrow <- data.frame(Reason = c1, EQUs = c2, Cost = c3, Perc.Tot = c4)
    } else newrow <- data.frame(cat = c1, EQUs = c2, Cost = c3, Perc.Tot = c4)
    temp.a <- rbind(temp.a, newrow)
    outPut[[1]] <- temp.a
  }
  return(outPut)
}
#function for creating a list of growing areas' data
cr.gr.list <- function(mo.ly,yr.ly,mo.ty,yr.ty,b=1){
  totals <- list()
  ars.tots <- list()
  ars <- length(mo.ly)
  for(n in 1:ars){
    frames <- list(mo.ly,yr.ly,mo.ty,yr.ty)
    for(i in 1:4){
      totals[[i]] <- data.frame(frames[[i]][[n]])
    }
    tlln <- length(totals)
    for(l in 1:tlln){
      if(l != 1){
        if(b == 1){
          one <- merge(one, totals[[l]], by="Reason", all=TRUE)
        } else {
          one <- merge(one, totals[[l]],by="cat",all=TRUE)
        }
      } else one <- totals[[1]]
      setnames(one,"EQUs",paste("EQUs.",l,sep=""))
      setnames(one,"Cost",paste("Cost.",l,sep=""))
      setnames(one,"Perc.Tot",paste("Percent.Tot.",l,sep=""))   
    }
    ars.tots[[n]] <- one
  }
  return(ars.tots)
}

#month totals last year and this year
or.mo.ty <- last.mo(yr,160000,gr.or)
or.mo.ly <- last.mo(yr-1,160000,gr.or)
or.yr.ty <- year.data(yr,160000,gr.or)
or.yr.ly <- year.data(yr-1,160000,gr.or)

ca.mo.ty <- last.mo(yr,170000,gr.or)
ca.mo.ly <- last.mo(yr-1,170000,gr.or)
ca.yr.ty <- year.data(yr,170000,gr.or)
ca.yr.ly <- year.data(yr-1,170000,gr.or)

ga.mo.ty <- last.mo(yr,550000,gr.ga)
ga.mo.ly <- last.mo(yr-1,550000,gr.ga)
ga.yr.ty <- year.data(yr,550000,gr.ga)
ga.yr.ly <- year.data(yr-1,550000,gr.ga)

or.rsn <- cr.gr.list(or.mo.ly,or.yr.ly,or.mo.ty,or.yr.ty)
ca.rsn <- cr.gr.list(ca.mo.ly,ca.yr.ly,ca.mo.ty,ca.yr.ty)
ga.rsn <- cr.gr.list(ga.mo.ly,ga.yr.ly,ga.mo.ty,ga.yr.ty)

######################################
#CATEGORY DATA
######################################

#month totals last year and this year
or.mo.ty.cat <- last.mo(yr,160000,gr.or,n=2)
or.mo.ly.cat <- last.mo(yr-1,160000,gr.or,n=2)
or.yr.ty.cat <- year.data(yr,160000,gr.or,b=2)
or.yr.ly.cat <- year.data(yr-1,160000,gr.or,b=2)

ca.mo.ty.cat <- last.mo(yr,170000,gr.or,n=2)
ca.mo.ly.cat <- last.mo(yr-1,170000,gr.or,n=2)
ca.yr.ty.cat <- year.data(yr,170000,gr.or,b=2)
ca.yr.ly.cat <- year.data(yr-1,170000,gr.or,b=2)

ga.mo.ty.cat <- last.mo(yr,550000,gr.ga,n=2)
ga.mo.ly.cat <- last.mo(yr-1,550000,gr.ga,n=2)
ga.yr.ty.cat <- year.data(yr,550000,gr.ga,b=2)
ga.yr.ly.cat <- year.data(yr-1,550000,gr.ga,b=2)

or.cat <- cr.gr.list(or.mo.ly.cat,or.yr.ly.cat,or.mo.ty.cat,or.yr.ty.cat,b=2)
ca.cat <- cr.gr.list(ca.mo.ly.cat,ca.yr.ly.cat,ca.mo.ty.cat,ca.yr.ty.cat,b=2)
ga.cat <- cr.gr.list(ga.mo.ly.cat,ga.yr.ly.cat,ga.mo.ty.cat,ga.yr.ty.cat,b=2)

########################################
#Top Ten Lists
########################################
top.ten <- data.table(loc=scrap.2$Loc., 
                      month=scrap.2$mo, 
                      year=scrap.2$yr,
                      grw=scrap.2$GA.,
                      qty=scrap.2$Trans..Qty,
                      equ=scrap.2$Trans..EQUs,
                      cost=scrap.2$Extended..Cost,
                      reason=scrap.2$Reason..Description,
                      item=scrap.2$Item,
                      it.desc=scrap.2$Description)


top.lists <- function(year,month,location=NULL,gr.area=NULL){
  top.list <- list()
  if(!is.null(location)){
    for(i in 1:length(gr.area)){
      top.ten$tot.scrap <- top.ten[loc == location & year == year & month == month & grw == gr.area[i], 
                                   list(EQUs=sum(as.numeric(equ)))]
      
      tp.ten <- top.ten[loc == location & year == year & month == month & grw == gr.area[i], 
                        list(Description=it.desc,
                             Reason=reason,
                             Qty=qty,
                             EQUs=equ,
                             Cost=cost,
                             per.tot=equ/tot.scrap), 
                        keyby = item]
      
      top.list[[i]] <- tp.ten[order(-EQUs),][1:10]
    }
  }
  if(is.null(location)){
    top.ten$tot.scrap <- top.ten[year == year & month == month, 
                                 list(EQUs=sum(as.numeric(equ)))]
    
    tp.ten <- top.ten[year == year & month == month, 
                      list(Description=it.desc,
                           Reason=reason,
                           Qty=qty,
                           EQUs=equ,
                           Cost=cost,
                           per.tot=equ/tot.scrap), 
                      keyby = item]
    
    top.list[[1]] <- tp.ten[order(-EQUs),][1:10]
  }
  return(top.list)  
}

or.top <- top.lists(yr,mo,160000,gr.or)
ca.top <- top.lists(yr,mo,170000,gr.or)
ga.top <- top.lists(yr,mo,550000,gr.ga)

######################################
#Write csvs to file
######################################
files <- list()
files <- c(or.rsn,ca.rsn,ga.rsn,or.cat,ca.cat,ga.cat,or.top,ca.top,ga.top) 
#get rid of NAs in all files
i = 1
while(i < length(files) + 1){
  files[[i]][is.na(files[[i]])]<-0
  i = i + 1 
}

file.names <- c("or.rsn"=1:4,
                "ca.rsn"=5:8,
                "ga.rsn"=9:11,
                "or.cat"=12:15,
                "ca.cat"=16:19,
                "ga.cat"=20:22,
                "or.top"=23:26,
                "ca.top"=27:30,
                "ga.top"=31:33)

i = 1
while(i < length(files) + 1){
  file.path <- paste("C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Scrappage/csv_files/",
                     names(file.names[i]),
                     ".csv",
                     sep="")
  write.csv(files[[i]],file=file.path)
  i <- i + 1
}

#make summaries
sum.top.ten <- function(location,year,month){
  top.ten$tot.scrap <- top.ten[loc == location & year == year & month == month, 
                               list(EQUs=sum(as.numeric(equ)))]
  tp.ten <- top.ten[loc == location & year == year & month == month, 
                    list(Description=it.desc,
                         Reason=reason,
                         Qty=qty,
                         EQUs=equ,
                         Cost=cost,
                         per.tot=equ/tot.scrap), 
                    keyby = item]
  top.list <- list()
  top.list[[1]] <- tp.ten[order(-EQUs),][1:10]
  return(top.list)
}

or.tp.sum <- sum.top.ten(160000,year,mo)
ca.tp.sum <- sum.top.ten(170000,year,mo)
ga.tp.sum <- sum.top.ten(550000,year,mo)

summ <- function(df,location=NULL){
  month <- as.numeric(format(Sys.Date(),"%m"))
  yr <- as.numeric(format(Sys.Date(),"%y"))
  if(month == 1){
    mo <- "12"
    month <- 12
    yr <- yr - 1
  } else if(length(month) == 1){
    mo <- as.character(paste(0,month-1,sep=""))
    month <- month - 1 
  } else if(length(month) == 2){
    mo <- as.character(month-1)
    month <- month - 1
  }
  yrs <- c(yr - 1, yr)
  by.mo <- list()
  #for month dfs 
  for(i in 1:2){
    years <- as.character(yrs[i])
    if(is.null(location)){
      by.mo[[i]] <- df[year == years & month == mo,list(EQUs=sum(equ),Cost=sum(cost)),keyby = Reason]
      by.mo[[i]]$perc <- by.mo[[i]]$EQUs/sum(by.mo[[i]]$EQUs)
    } else {
      by.mo[[i]] <- df[loc == location & year == years & month == mo,list(EQUs=sum(equ),Cost=sum(cost)),keyby = Reason]
      by.mo[[i]]$perc <- by.mo[[i]]$EQUs/sum(by.mo[[i]]$EQUs)
    }
  }
  #for YTD dfs
  mo.rng <- 1:month
  mo.chars <- list()
  for(n in 1:month){
    if(length(mo.rng[n]) == 1){
      temp.mo <- paste("0",mo.rng[n],sep="")
    } else temp.mo <- as.char(mo.rng[n])
    mo.chars[n] <- temp.mo
  }
  mo.chars <- unlist(mo.chars)
  by.yr <- list()
  for(i in 1:2){
    years <- as.character(yrs[i])
    if(is.null(location)){
      by.yr[[i]] <- df[year == years & month %in% mo.chars ,list(EQUs=sum(equ),Cost=sum(cost)),keyby = Reason]
      by.yr[[i]]$perc <- by.yr[[i]]$Cost/sum(by.yr[[i]]$Cost)
    } else {
      by.yr[[i]] <- df[loc == location & year == years & month %in% mo.chars ,list(EQUs=sum(equ),Cost=sum(cost)),keyby = Reason]
      by.yr[[i]]$perc <- by.yr[[i]]$Cost/sum(by.yr[[i]]$Cost)
    }
  }
  mrg.1 <- as.data.frame(merge(by.mo[[1]],by.yr[[1]],by="Reason",all=TRUE))  
  mrg.2 <- as.data.frame(merge(by.mo[[2]],by.yr[[2]],by="Reason",all=TRUE))
  mrg <- as.data.frame(merge(mrg.1,mrg.2,by="Reason",all=TRUE))
  mrg[is.na(mrg)]<-0
  newrow <- data.frame(Reason = "Totals", 
                       EQUs.x.x = sum(mrg$EQUs.x.x), 
                       Cost.x.x = sum(mrg$Cost.x.x), 
                       perc.x.x = sum(mrg$perc.x.x),
                       EQUs.y.x = sum(mrg$EQUs.y.x),
                       Cost.y.x = sum(mrg$Cost.y.x),
                       perc.y.x = sum(mrg$perc.y.x),
                       EQUs.x.y = sum(mrg$EQUs.x.y),
                       Cost.x.y = sum(mrg$Cost.x.y),  
                       perc.x.y = sum(mrg$perc.x.y),
                       EQUs.y.y = sum(mrg$EQUs.y.y),
                       Cost.y.y = sum(mrg$Cost.y.y),   
                       perc.y.y = sum(mrg$perc.y.y))
  
  mrg <- rbind(mrg,newrow)
  mrgr <- list()
  mrgr[[1]] <- mrg
  return(mrgr)
}

or.sum.rsn <- summ(a,160000)
ca.sum.rsn <- summ(a,170000)
ga.sum.rsn <- summ(a,550000)

files <- list()
files <- c(or.tp.sum,or.sum.rsn,ca.tp.sum,ca.sum.rsn,ga.tp.sum,ga.sum.rsn) 

file.names <- c("or.sum"=1:2,"ca.sum"=3:4,"ga.sum"=5:6)
                
i = 1
while(i < length(files) + 1){
  file.path <- paste("C:/Users/kadavison/Desktop/Reports/Mim_Recreations/Scrappage/csv_files/",
                     names(file.names[i]),
                     ".csv",
                     sep="")
  write.csv(files[[i]],file=file.path)
  i <- i + 1
}
