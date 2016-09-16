#####CODE FOR CEU FINAL PROJECT BY ISTVAN ZSOLDOS

####DATA DOWNLOAD AND PREPARATION####
#installing necessary packages

ipak <- function(pkg){
        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
        if (length(new.pkg)) 
                install.packages(new.pkg, dependencies = TRUE)
        sapply(pkg, require, character.only = TRUE)
  }

packages <-c("readr","dplyr","ggplot2","xlsx","tidyr","data.table","gdata","readxl","glmnet","stargazer","maptools","rgeos","rgdal","sp","rmarkdown","shiny")
ipak(packages)

#reading in previously parsed data, stripping out leading and lagging white space in cause_name
#original data source: http://regi.oefi.hu/halalozas/
healthdata<-read.csv('https://raw.githubusercontent.com/Zsopi/CEU_final_project/master/smr.csv',strip.white=TRUE)

#filtering out empty rows
healthdata<-healthdata%>%filter(!is.na(year))

#creating smr_difference (males and females mortality difference variable)
healthdata<-healthdata%>%mutate(diff_smr=male_smr-female_smr)

#creating narrow version of dataframe
healthdata_narrow<-healthdata%>%gather(smr_type,smr, male_smr, female_smr, total_smr, diff_smr,-year,-cause_name,-jaras)

#adding new cause aggregation variables (non-external causes, non-external,
#non smoking causes, not preventable casuses) 
healthdata_wide<-spread(healthdata_narrow,cause_name,smr)
healthdata_wide$belso<-healthdata_wide[,28]-healthdata_wide[,9]
healthdata_wide$belso_nemdohanyzo<-healthdata_wide[,33]-healthdata_wide[,17]
healthdata_wide$nemmegelozheto<-healthdata_wide[,28]-healthdata_wide[,18]

#recreating new narrow version, it was necessary to rename column
hd_name4<-names(healthdata_wide)[[4]]
names(healthdata_wide)[4]<-"tudorak"

healthdata_narrow<-healthdata_wide%>%gather(cause_name, smr, tudorak:nemmegelozheto,-year,-jaras, -smr_type)
healthdata_narrow$cause_name[healthdata_narrow$cause_name=="tudorak"]<-hd_name4

#creating new wide version by years, 
#creating averages by years
#taking differences in averages (time difference between first and second period)

healthdata_wide1<-spread(healthdata_narrow,year,smr)
healthdata_wide1$avg2005_2008<-rowMeans(healthdata_wide1[,4:7])
healthdata_wide1$avg2009_2013<-rowMeans(healthdata_wide1[,8:12])
healthdata_wide1$t_diff<-healthdata_wide1$avg2009_2013-healthdata_wide1$avg2005_2008

#creating reduced database with only period averages and time differences, cutting off _smr from smr_type
healthdata_reduced<-healthdata_wide1[,c(1:3,13:15)]
healthdata_reduced<- as.data.frame(lapply(healthdata_reduced,function(x) if(is.character(x)) gsub("_smr","",x) else x))

#creating "tall" datafile from reduced, which I called long and sticked with the name
healthdata_reduced_long<-healthdata_reduced%>%gather(period,smr, avg2005_2008,avg2009_2013,t_diff)

#collecting possible years, cause and place(jaras) names
hd_years<-unique(healthdata$year)%>%sort()
hd_jaras_names<-unique(as.character(healthdata$jaras))%>%sort()
hd_cause_names<-unique(as.character(healthdata_wide1$cause_name))

#original download from here, does not always work - Hungarian Central Statistical Office (KSH)
#download.file("https://www.ksh.hu/docs/teruletiatlasz/jarasok.xls", "jarasok_data.xls",mode="wb")
#downloading saved Stats office file
download.file('https://raw.githubusercontent.com/Zsopi/CEU_final_project/master/jarasok.xls','jarasok_data.xls',mode="wb")
jarasok_expvars<- read_excel('jarasok_data.xls', sheet=1)

#collecting names in a dataframe
names_jarasok<-droplevels(jarasok_expvars[1,])

#fixing some issues with names all over the place
names_jarasok[,4]<-"Városok száma"
names_jarasok[,9]<-paste("A", jarasok_expvars[1,9],jarasok_expvars[2,9])
names_jarasok[,10]<-paste("A", jarasok_expvars[1,10],jarasok_expvars[2,9])
names_jarasok[,11]<-paste("A", jarasok_expvars[1,11],jarasok_expvars[2,9])
names_jarasok[,13]<-paste(jarasok_expvars[1,13],jarasok_expvars[2,13])
names_jarasok[,14]<-paste(jarasok_expvars[1,13],jarasok_expvars[2,14])
names_jarasok[,19]<-paste(jarasok_expvars[1,18],jarasok_expvars[2,19])
names_jarasok[,28]<-paste(jarasok_expvars[1,28],jarasok_expvars[2,28])
names_jarasok[,29]<-paste(jarasok_expvars[1,29],jarasok_expvars[2,28])
names_jarasok[,30]<-paste(jarasok_expvars[1,30],jarasok_expvars[2,28])
names_jarasok[,31]<-paste(jarasok_expvars[1,31],jarasok_expvars[2,28])

#getting rid of rows that are not needed
jarasok_expvars<-jarasok_expvars[3:240,]
jarasok_expvars<-jarasok_expvars[!is.na(jarasok_expvars[,2]),]
jarasok_expvars<-jarasok_expvars[!is.na(jarasok_expvars[,3]),]

jarasok_expvars[]<-droplevels(jarasok_expvars[])

jarasok_expvars_final<-jarasok_expvars

names_jarasok[] <- lapply(names_jarasok, as.character)
names(jarasok_expvars_final)<-names_jarasok

#ordering jarasok by name
names(jarasok_expvars_final)[1]<-"jars"
jarasok_expvars_final<-jarasok_expvars_final%>%arrange(jars)

#fixing typo in Budapest second district aid recipient data (decimal is very likely at wrong place)
jarasok_expvars_final[24,27]<-2.45

#converting to numeric, replacing resulting NAs with zeros
indx <-3:34
jarasok_expvars_final[indx]<-lapply(jarasok_expvars_final[indx], function(x) as.numeric(as.character(x)))
jarasok_expvars_final[is.na(jarasok_expvars_final)] <- 0

#fixing some name issues again
jarasok_expvars_final[5,1]<-"Baktalórántházai járás"
jarasok_expvars_final[14,1]<-"Bélapátfalvai járás"
jarasok_expvars_final[83,1]<-"Jánoshalmai járás"
jarasok_expvars_final[115,1]<-"Mezokovácsházai járás"


#downloading xls file regarding education achievement of the populaiton aged 7+ in each "jaras"
#original link
#download.file("http://www.ksh.hu/nepszamlalas/docs/tablak/nepesseg_iskolazottsaga/14_03_01.xls", "jarasok_edu.xls",mode="wb")
#saved link
download.file('https://raw.githubusercontent.com/Zsopi/CEU_final_project/master/jarasok_edu.xls', "jarasok_edu.xls",mode="wb")
jarasok_edu<- read_excel('jarasok_edu.xls', sheet=1)

#renaming, creating variables for tertiary education ratio and average years of educaiton
names(jarasok_edu)[1:8]<-c("jars","zero","prim_min","prim","sec","sec_bac","tert","tot")
jarasok_edu<-jarasok_edu[c(5:346),]
indx <-2:8
jarasok_edu[indx]<-lapply(jarasok_edu[indx], function(x) as.numeric(as.character(x)))
jarasok_edu<-jarasok_edu%>%mutate(tert_edu=tert/tot)
jarasok_edu<-jarasok_edu%>%mutate(years_edu=(prim_min*4+prim*8+sec*11+sec_bac*12+tert*16)/tot)
jarasok_edu[1]<-lapply(jarasok_edu[1],function(x) as.character(x))
jarasok_edu[1]<-lapply(jarasok_edu[1],function(x) paste(x,"járás"))

jarasok_edu[1:23,1]<-jarasok_expvars_final[20:42,1]

#fixing names again
jarasok_expvars_final[115,1]<-jarasok_edu[71,1]

#merging other explonatory variables and education data
jarasok_expvars_final<-merge(jarasok_expvars_final,jarasok_edu[,c(1,9,10)],by.x="jars", by.y="jars")

#narrowing down explonatory variables
jarasok_regvars<-jarasok_expvars_final[,c(6,7,8,15,16,17,18,20,26,27,34,35,36)]
names(jarasok_regvars)<-c("pop_dens","pop","pop_chg","unempl","lt_unempl","income", "houses", "house_building","pensioners","social_aid","cars","tert_edu","years_edu")

#generating log variable names, really could be done on the fly later on
jarasok_regvars<-jarasok_regvars%>%mutate(house_pop=houses/pop)%>%mutate(log_income=log(income))%>%
        mutate(log_cars=log(cars))%>%mutate(log_social_aid=log(social_aid))

#shortening jaras names
healthdata_reduced_long$jaras<-as.character(healthdata_reduced_long$jaras)
healthdata_reduced_long<-as.data.frame(lapply(healthdata_reduced_long,function(x) if(is.character(x)) gsub("_jaras","",x) else x),stringsAsFactors=FALSE)
healthdata_reduced_long<-as.data.frame(lapply(healthdata_reduced_long,function(x) if(is.character(x)) gsub("Budapest","Bp",x) else x),stringsAsFactors=FALSE)

#gathering short names 
hd_jaras_names_shrt<-unique(as.character(healthdata_reduced_long$jaras))

#####data generation ends here

#####PREPARING FOR MAP DRAWING#####
#downloading shapefile
wd<-getwd()
#original download
#download.file("http://data2.openstreetmap.hu/hatarok/kozighatarok.zip","kzghatarok.zip")
download.file('https://raw.githubusercontent.com/Zsopi/CEU_final_project/master/kzghatarok.zip',"kzghatarok.zip",mode = "wb")
unzip("kzghatarok.zip")

jarasok_shp <- readShapeSpatial(paste(wd,"/kozighatarok/admin7.shp",sep = ""))
keruletek_shp <- readShapeSpatial(paste(wd,"/kozighatarok/admin9.shp",sep = ""))

#eliminating duplicate row in jarasok
jarasok_shp<-(jarasok_shp[c(1:71,73:176),])

#for merging the two files, we need different rownames
row.names(keruletek_shp)<-as.character((176:198))

#merging the two shapefiles
jar_ker_shp<-spRbind(jarasok_shp,keruletek_shp)

jar_ker_shp$var1<-runif(198)
spplot(jar_ker_shp,z="var1")

#generating converter file for jarasok
temp_names_jarasok<-as.data.frame(jarasok_shp)
temp_names_jarasok$orig_order<-seq(175)
temp_names_jarasok$abc_order<-c(3, 166, 60, 27, 32, 46, 72, 44, 170, 74, 2, 68, 97, 104, 115, 137, 20, 73, 75, 125, 55, 105, 98, 62, 48, 11, 76, 160, 118, 138, 61, 24, 151, 83, 57, 111, 42, 35, 143, 58, 14, 165, 87, 121, 94, 82, 136, 67, 93, 95, 34, 66, 109, 159, 47, 12, 91, 92, 9, 13, 161, 149, 36, 139, 146, 54, 122, 53, 45, 162, 50, 15, 51, 163, 127, 128, 130, 108, 28, 29, 59, 52, 22, 102, 69, 106, 77, 5, 107, 172, 90, 168, 40, 23, 144, 80, 85, 152, 134, 81, 173, 86, 169, 129, 103, 26, 65, 174, 21, 100, 70, 135, 25, 114, 10, 101, 88, 30, 155, 49, 1, 158, 41, 64, 171, 8, 148, 131, 113, 133, 175, 153, 71, 145, 78, 7, 56, 167, 31, 99, 116, 132, 154, 79, 110, 157, 140, 37, 120, 156, 18, 117, 17, 126, 16, 39, 96, 43, 141, 112, 89, 33, 19, 164, 4, 119, 150, 38, 142, 63, 123, 147, 84, 6, 124)
temp_names_jarasok<-temp_names_jarasok%>%arrange(abc_order)
temp_names_jarasok$jarasok<-hd_jaras_names[c(1:19,43:198)]
temp_names_jarasok$jarasok_shrt<-hd_jaras_names_shrt[c(1:19,43:198)]

#generating converter file for keruletek
temp_names_keruletek<-as.data.frame(keruletek_shp)
temp_names_keruletek$orig_order<-seq(1:23)
temp_names_keruletek$abc_order<-c(9,6,7,8,4,14,20,23,10,15,19,16,18,17,2,12,22,11,3,21,1,13,5)
temp_names_keruletek<-temp_names_keruletek%>%arrange(abc_order)
temp_names_keruletek$keruletek<-hd_jaras_names[20:42]
temp_names_keruletek$keruletek_shrt<-hd_jaras_names_shrt[20:42]

#generating converter file for jarasok and keruletek combined
temp_names_jarasok<-temp_names_jarasok%>%arrange(orig_order)
temp_names_keruletek_mod<-temp_names_keruletek%>%arrange(orig_order)
temp_names_keruletek_mod$orig_order<-176:198
names(temp_names_keruletek_mod)[5:6]<-c("jarasok","jarasok_shrt")
temp_names_jar_ker<-rbind(temp_names_jarasok,temp_names_keruletek_mod)
temp_names_jar_ker<-temp_names_jar_ker%>%arrange(jarasok)
temp_names_jar_ker$abc_order<-1:198

#preparing shapefiles for ggplot

#the whole country
jar_ker_shp$jarasok_shrt<-temp_names_jar_ker%>%arrange(orig_order)%>%select(jarasok_shrt)
rownames(jar_ker_shp@data)<-jar_ker_shp$jarasok_shrt[1:198,]
jar_ker_shp@data$id <-rownames(jar_ker_shp@data)
jar_ker_shp_ff<-fortify(jar_ker_shp, region = "id")

#Budapest 
keruletek_shp$keruletek_shrt<-temp_names_keruletek%>%arrange(orig_order)%>%select(keruletek_shrt)
rownames(keruletek_shp@data)<-keruletek_shp$keruletek_shrt[1:23,]
keruletek_shp@data$id <-rownames(keruletek_shp@data)
keruletek_shp_ff<-fortify(keruletek_shp, region = "id")

####MAP PREPARATION ENDS HERE

#####FIRST LOOK AT THE DATA######

View(healthdata_wide1%>%filter(smr_type=="total_smr"&cause_name=="Összes halálozás")%>%arrange(avg2009_2013))

View(healthdata_wide1%>%filter(smr_type=="female_smr"&cause_name=="belso_nemdohanyzo")%>%arrange(avg2009_2013))

View(healthdata_wide1%>%filter(smr_type=="total_smr"&cause_name=="Keringési rendszer betegségei")%>%arrange(avg2009_2013))

View(healthdata_wide1%>%filter(smr_type=="female_smr"&cause_name=="Rosszindulatú daganatok")%>%arrange(avg2009_2013))

View(healthdata_wide1%>%filter(smr_type=="male_smr"&cause_name== "Dohányzásnak tulajdonítható")%>%arrange(avg2009_2013))

View(healthdata_wide1%>%filter(smr_type=="total_smr"&cause_name== "Krónikus májbetegségek és cirrózis" )%>%arrange(avg2009_2013))

View(healthdata_wide1%>%filter(smr_type=="total_smr"&cause_name== "Elkerülhetõ halálozások" )%>%arrange(avg2009_2013))


#description and summaries of variables
stargazer(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),type = "text")

#saving html file of variable description
stargazer(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),type = "html", out = "var_desc.html")

#selecting data to examine
regr_data<-healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás")
regr_data<-cbind(regr_data, jarasok_regvars)

#summarizing by the top 10%, middle 80% and bottom 10%
#creating groups first
groups=ifelse(regr_data$smr<=quantile(regr_data$smr,0.1),1,ifelse(regr_data$smr>=quantile(regr_data$smr,0.9),3,2))

table_10_80_10<-as.data.frame((t(as.data.frame(aggregate(regr_data[,c(5:22)],FUN=mean,by=list(groups))))[-1,]))
names(table_10_80_10)<-c("best 10%","middle 80%","worst 10%")
View(table_10_80_10)

#writing data for the table in the paper
#write.table(table_10_80_10, "C:/Users/Zsopi/Google Drive/R/CEU_final_project/deciles_data.txt", sep=",")

#summarizing by period some causes
healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Összes halálozás")%>%group_by(period) %>%select(c(period,smr))%>%
        summarise_each(funs(mean, median, min, max, sd))

healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Rosszindulatú daganatok")%>%group_by(period) %>%select(c(period,smr))%>%
        summarise_each(funs(mean, median, min, max, sd))

healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%group_by(period) %>%select(c(period,smr))%>%
        summarise_each(funs(mean, median, min, max, sd))


####REGRESSIONS####

#lasso regression for variable order
regr_data<-healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás")
regr_data<-cbind(regr_data, jarasok_regvars)
lasso_reg<-glmnet(as.matrix(regr_data[,c(6,9:10,13:16,18:20)]),as.matrix(regr_data$smr),weights = regr_data$pop)
summary(lasso_reg)
coef.glmnet(lasso_reg)

#regressions based on lasso order, first equally weighted
reg1<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),
                    smr~years_edu)
summary(reg1)

reg2<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),
         smr~years_edu+cars)
summary(reg2)

reg3<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),
         smr~years_edu+cars+social_aid+pop_dens+log_income)
summary(reg3)

#population weighted regressions
reg1w<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),
         smr~years_edu, weights = pop)
summary(reg1w)

reg2w<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),
         smr~years_edu+cars, weights = pop)
summary(reg2w)

reg3w<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),
         smr~years_edu+cars+social_aid+pop_dens+log_income, weights = pop)
summary(reg3w)

#presenting some of the regressions1
stargazer(reg2, reg3, reg2w, reg3w,title="Regression Results",
          covariate.labels = c("Education (avg. years)","Cars per '000 population","Social aid recepients '000 population","Population density","Log of average income per taxpayer"),
          dep.var.caption  = "Mortality decreases with education and car ownership",
          dep.var.labels   = "Age standardized mortality rate",
          add.lines = list(c("Weights", "equal", "equal","population","population")),
                type = "html",out = "regr_table1.html")

#adding the amenable mortality rate to the equally weighted regressions

reg4<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
        select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),
        smr_nr~years_edu+smr_amenable)
summary(reg4)

reg5<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
        select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),
        smr_nr~years_edu+cars+smr_amenable)
summary(reg5)

reg6<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                             select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),
         smr_nr~years_edu+cars+social_aid+pop_dens+log_income+smr_amenable)
summary(reg6)

#adding the amenable mortality rate to the population weighted regressions
reg4w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                             select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),
         smr_nr~years_edu+smr_amenable, weights = pop)
summary(reg4w)


reg5w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                              select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),
          smr_nr~years_edu+cars+smr_amenable, weights = pop)
summary(reg5w)

reg6w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                             select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),
         smr_nr~years_edu+cars+social_aid+pop_dens+log_income+smr_amenable, weights = pop)
summary(reg6w)


#presenting some of the regressions 2
stargazer(reg5, reg6, reg5w, reg6w,title="Regression Results",
          covariate.labels = c("Education (avg. years)","Cars per '000 population","Social aid recepients '000 population","Population density","Log of average income per taxpayer","Amenable mortality rate"),
          dep.var.caption  = "Mortality decreases with education and car ownership",
          dep.var.labels   = "Age standardized mortality rate",
          add.lines = list(c("Weights", "equal", "equal","population","population")),
          type = "html",out = "regr_table2.html")

#everyting else vs cancer regression
reg15w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Összes halálozás"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Rosszindulatú daganatok")%>%
                               select(smr_cr=smr),jarasok_regvars))%>%mutate(smr=smr-smr_cr),
           smr_cr~smr)
summary(reg15w)

#lasso regression for amneable mortality
regr_data_amen<-healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")
regr_data_amen<-cbind(regr_data_amen, jarasok_regvars)
lasso_amen<-glmnet(as.matrix(regr_data_amen[,c(6,9:10,13:16,18:20)]),as.matrix(regr_data_amen$smr),weights = regr_data_amen$pop)
summary(lasso_amen)
coef.glmnet(lasso_amen)


#population weighted regressions for amenable mortality
reg1w_amen<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások"),jarasok_regvars),
          smr~years_edu, weights = pop)
summary(reg1w_amen)

reg2w_amen<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások"),jarasok_regvars),
          smr~years_edu+cars, weights = pop)
summary(reg2w_amen)

reg3w_amen<-lm(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások"),jarasok_regvars),
          smr~years_edu+cars+social_aid+pop_dens+log_income, weights = pop)
summary(reg3w_amen)

#presenting the regressions3
stargazer(reg2w_amen, reg3w_amen, title="Regression Results",
          covariate.labels = c("Education (avg. years)","Cars per '000 population","Social aid recepients '000 population","Population density","Log of average income per taxpayer"),
          dep.var.caption  = "Amenable mortality also decreases with education",
          dep.var.labels   = "Age standardized amenable mortality rate",
          add.lines = list(c("Weights", "population","population","population")),
          type = "html",out = "regr_table3_amen.html")

#a look at residuals
View(cbind(as.data.frame(regr_data),resid(reg2w),resid(reg3w),resid(reg5w),resid(reg6w),resid(reg2w_amen),resid(reg3w_amen))%>%arrange(resid(reg2w)))

resids<-cbind(as.data.frame(regr_data),resid(reg2w),resid(reg3w),resid(reg5w),resid(reg6w),resid(reg2w_amen),resid(reg3w_amen))%>%arrange(resid(reg2w_amen))

cor(resids$`resid(reg2w)`,resids$`resid(reg2w_amen)`)


###########################END OF REGRESSIONS


####PLOTS####

#demonstarting noisy data and broad time trends
g1 <- subset(healthdata_narrow%>%filter(cause_name=="Összes halálozás")%>%filter(smr_type=="total_smr"), jaras == "Budapest_02._ker.")
g2 <- subset(healthdata_narrow%>%filter(cause_name=="Összes halálozás")%>%filter(smr_type=="total_smr"), jaras == "Belapatfalvi_jaras")

ggplot(data = healthdata_narrow%>%filter(cause_name=="Összes halálozás")%>%filter(smr_type=="total_smr"), aes(year,smr))+
  geom_line(aes(fill=jaras))+ scale_colour_gradientn(colours=rainbow(4))+stat_summary(fun.y = mean, geom="line",color="red",size=1.5)+
  geom_line(data=g1, colour="blue",size=1.5)+geom_line(data=g2, colour="yellow",size=1.5)+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+labs(title="Standardized total mortality rate over time")

#Totally random map to test your senses
set.seed(13)
plotdata<-cbind(jaras=hd_jaras_names_shrt,data.frame(smr=runif(198)))
ggplot(jar_ker_shp_ff)+geom_map(data = plotdata,aes(map_id=jaras,fill=smr),map=jar_ker_shp_ff)+ expand_limits(x = jar_ker_shp_ff$long, y = jar_ker_shp_ff$lat)+
  scale_fill_gradient2(name="Random variable - \nread the legend",low = "blue",  mid = "white", midpoint = median(plotdata$smr), high = "red", limits = c(min(plotdata$smr),max(plotdata$smr)))+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank())

#Country map of total mortality rate
plotdata<-healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name==    "Összes halálozás" )
ggplot(jar_ker_shp_ff)+geom_map(data = plotdata,aes(map_id=jaras,fill=smr),map=jar_ker_shp_ff)+ expand_limits(x = jar_ker_shp_ff$long, y = jar_ker_shp_ff$lat)+
  scale_fill_gradient2(name="Average standardized \nmortality 2009-13",low = "blue",  mid = "white", midpoint = median(plotdata$smr), high = "red", limits = c(min(plotdata$smr),max(plotdata$smr)))+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank())

#Budapest plot of total mortality rate
ggplot(keruletek_shp_ff)+geom_map(data = plotdata,aes(map_id=jaras,fill=smr),map=keruletek_shp_ff)+ expand_limits(x = keruletek_shp_ff$long, y = keruletek_shp_ff$lat)+
  scale_fill_gradient2(name="Average standardized \nmortality 2009-13",low = "blue",  mid = "white", midpoint = median(plotdata$smr), high = "red", limits = c(min(plotdata$smr),max(plotdata$smr)))+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank())

#Country map of suicide mortality rate
plotdata<-healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name==    "Szándékos önártalom" )
ggplot(jar_ker_shp_ff)+geom_map(data = plotdata,aes(map_id=jaras,fill=smr),map=jar_ker_shp_ff)+ expand_limits(x = jar_ker_shp_ff$long, y = jar_ker_shp_ff$lat)+
  scale_fill_gradient2(name="Average standardized \nmortality 2009-13",low = "blue",  mid = "white", midpoint = median(plotdata$smr), high = "red", limits = c(min(plotdata$smr),max(plotdata$smr)))+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank())

#country regression residual map plot
plotdata<-as.data.frame(cbind(jaras=hd_jaras_names_shrt,smr=(resid(reg2w))),stringsAsFactors = F)
plotdata$smr<-as.numeric(plotdata$smr)
ggplot(jar_ker_shp_ff)+geom_map(data = plotdata,aes(map_id=jaras,fill=smr),map=jar_ker_shp_ff)+ expand_limits(x = jar_ker_shp_ff$long, y = jar_ker_shp_ff$lat)+
  scale_fill_gradient2(name="Residuals from \nmortality regression",low = "blue",  mid = "white", midpoint = median(plotdata$smr), high = "red", limits = c(min(plotdata$smr),max(plotdata$smr)))+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank())

#budapest regression residual map plot
plotdata<-as.data.frame(cbind(jaras=hd_jaras_names_shrt,smr=(resid(reg2w))),stringsAsFactors = F)
plotdata$smr<-as.numeric(plotdata$smr)
ggplot(keruletek_shp_ff)+geom_map(data = plotdata,aes(map_id=jaras,fill=smr),map=keruletek_shp_ff)+ expand_limits(x = keruletek_shp_ff$long, y = keruletek_shp_ff$lat)+
  scale_fill_gradient2(name="Residuals from \nmortality regression",low = "blue",  mid = "white", midpoint = median(plotdata$smr), high = "red", limits = c(min(plotdata$smr),max(plotdata$smr)))+
  theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank())

#income and total mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(income,smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and standardized mortality rate")+
        xlab("average taxable income per taxpayer, HUF thousand, 2011")+ylab("average smr, 2009-13")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income and total mortality, no labels
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(income,smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and standardized mortality rate")+
        xlab("average taxable income per taxpayer, HUF thousand, 2011")+ylab("average smr, 2009-13")+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))+geom_smooth()

#education and mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and standardized mortality rate")+
        xlab("average education, years, 2011")+ylab("average smr, 2009-13")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

#education and mortality, no labels
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and standardized mortality rate")+
        xlab("average education, years, 2011")+ylab("average smr, 2009-13")+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

#log pop density and mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(log(pop_dens),smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Population density and standardized mortality rate")+
        xlab("log population density, 2011")+ylab("average smr, 2009-13")+geom_text(aes(label=jaras))+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

#education and mortality, Bp only
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars))%>%filter(grepl('Bp_|Buda', jaras)),aes(years_edu,smr))+
  geom_point(aes(size=pop,color=income))+labs(title="Education and standardized mortality rate, Budapest districts")+
  xlab("average education, years, 2011")+ylab("average smr, 2009-13")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#education and amneable mortality
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
  select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),aes(years_edu, smr_amenable))+geom_point(aes(size=pop,color=income))+
  labs(title="Education and amenable mortality")+
  xlab("Education, avg. years, 2011")+ylab("Amenable mortality, 2009-13 average")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
  axis.title=element_text(size=16,face="bold"))+geom_smooth()

#distribution of mortality, total
ggplot(data=cbind(healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of mortality rate across counties")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#distribution of mortality, cancer, male
ggplot(data=cbind(healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="male"&cause_name=="Rosszindulatú daganatok"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of cancer mortality rate across counties, males")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#distribution of mortality, cancer, female
ggplot(data=cbind(healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="female"&cause_name=="Rosszindulatú daganatok"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of cancer mortality rate across counties, females")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#distribution of mortality, amenable
ggplot(data=cbind(healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Elkerülhetõ halálozások"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of amenable mortality rate across counties")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#############################END OF PLOTS

########################END OF PAPER

##############################APPENDX_0:REPEATING SOME OF THE ANALYSIS FOR PREMATURE MORTALITY####


#reading in previously parsed data in a csv file, stripping out leading and lagging white space in cause_name
healthdata_pm<-read.csv('https://raw.githubusercontent.com/Zsopi/CEU_final_project/master/smr_premature.csv', strip.white=TRUE)

#filtering out empty rows
healthdata_pm<-healthdata_pm%>%filter(!is.na(year))

#creating smr_difference (males and females)
healthdata_pm<-healthdata_pm%>%mutate(diff_smr=male_smr-female_smr)

#creating narrow versions of dataframe
healthdata_narrow_pm<-healthdata_pm%>%gather(smr_type,smr, male_smr, female_smr, total_smr, diff_smr,-year,-cause_name,-jaras)

#adding new cause aggregations (non-external causes, non-external,
#non smoking causes, not preventable casuses) 
healthdata_wide_pm<-spread(healthdata_narrow_pm,cause_name,smr)
healthdata_wide_pm$belso<-healthdata_wide_pm[,28]-healthdata_wide_pm[,9]
healthdata_wide_pm$belso_nemdohanyzo<-healthdata_wide_pm[,33]-healthdata_wide_pm[,17]
healthdata_wide_pm$nemmegelozheto<-healthdata_wide_pm[,28]-healthdata_wide_pm[,18]

#recreating new narrow version, it was necessary to rename column

hd_name4<-names(healthdata_wide_pm)[[4]]
names(healthdata_wide_pm)[4]<-"tudorak"

healthdata_narrow_pm<-healthdata_wide_pm%>%gather(cause_name, smr, tudorak:nemmegelozheto,-year,-jaras, -smr_type)
healthdata_narrow_pm$cause_name[healthdata_narrow_pm$cause_name=="tudorak"]<-hd_name4

#creating new wide version by years, 
#creating averages by years
#taking differences in averages

healthdata_wide1_pm<-spread(healthdata_narrow_pm,year,smr)

healthdata_wide1_pm$avg2005_2008<-rowMeans(healthdata_wide1_pm[,4:7])
healthdata_wide1_pm$avg2009_2013<-rowMeans(healthdata_wide1_pm[,8:12])
healthdata_wide1_pm$t_diff<-healthdata_wide1_pm$avg2009_2013-healthdata_wide1_pm$avg2005_2008
#healthdata_wide1$t_diff_pct<-as.data.frame(ifelse(healthdata_wide1$avg2005_2008==0,0,healthdata_wide1$avg2009_2013/healthdata_wide1$avg2005_2008))
#healthdata_wide1$t_diff_pct<-as.data.frame(ifelse(healthdata_wide1$t_diff==0,0,healthdata_wide1$t_diff/healthdata_wide1$avg2005_2008))


#creating reduced database with only period averages and time differences, cutting off _smr from smr_type
healthdata_reduced_pm<-healthdata_wide1_pm[,c(1:3,13:15)]
healthdata_reduced_pm<- as.data.frame(lapply(healthdata_reduced_pm,function(x) if(is.character(x)) gsub("_smr","",x) else x))

healthdata_reduced_long_pm<-healthdata_reduced_pm%>%gather(period,smr, avg2005_2008,avg2009_2013,t_diff)

###premature mortality data generation ends here

View(healthdata_wide1_pm%>%filter(smr_type=="total_smr"&cause_name=="Összes halálozás")%>%arrange(avg2009_2013))


#lasso regression for variable order
regr_data<-healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás")
regr_data<-cbind(regr_data, jarasok_regvars)
lasso_reg<-glmnet(as.matrix(regr_data[,c(6,9:10,13:16,18:20)]),as.matrix(regr_data$smr),weights = regr_data$pop)
summary(lasso_reg)
coef.glmnet(lasso_reg)


####premature charts


#demonstarting noisy data
g1 <- subset(healthdata_narrow_pm%>%filter(cause_name=="Összes halálozás")%>%filter(smr_type=="total_smr"), jaras == "Budapest_02._ker.")
g2 <- subset(healthdata_narrow_pm%>%filter(cause_name=="Összes halálozás")%>%filter(smr_type=="total_smr"), jaras == "Belapatfalvi_jaras")

ggplot(data = healthdata_narrow_pm%>%filter(cause_name=="Összes halálozás")%>%filter(smr_type=="total_smr"), aes(year,smr))+
        geom_line(aes(fill=jaras))+ scale_colour_gradientn(colours=rainbow(4))+stat_summary(fun.y = mean, geom="line",color="red",size=1.5)+
        geom_line(data=g1, colour="blue",size=1.5)+geom_line(data=g2, colour="yellow",size=1.5)+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                      axis.title=element_text(size=16,face="bold"))+labs(title="Standardized total mortality rate")

#income and total mortality
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(income,smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and standardized mortality rate")+
        xlab("average taxable income per taxpayer, HUF thousand, 2011")+ylab("average smr, 2009-13")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                                                   axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income and total mortality, no labels
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(income,smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and standardized mortality rate")+
        xlab("average taxable income per taxpayer, HUF thousand, 2011")+ylab("average smr, 2009-13")+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                      axis.title=element_text(size=16,face="bold"))+geom_smooth()

#education and mortality
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and standardized mortality rate")+
        xlab("average education, years, 2011")+ylab("average smr, 2009-13")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

#education and mortality, no labels
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and standardized mortality rate")+
        xlab("average education, years, 2011")+ylab("average smr, 2009-13")+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()


#log pop density and mortality
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(log(pop_dens),smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Population density and standardized mortality rate")+
        xlab("log population density, 2011")+ylab("average smr, 2009-13")+geom_text(aes(label=jaras))+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

#education and mortality, Bp only
ggplot(data=(cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars))%>%filter(grepl('Bp_|Buda', jaras)),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and standardized mortality rate, Budapest districts")+
        xlab("average education, years, 2011")+ylab("average smr, 2009-13")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)


#education and amneable mortality
ggplot(data=(cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                           select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),aes(years_edu, smr_amenable))+geom_point(aes(size=pop,color=income))+
        labs(title="Education and amenable mortality")+
        xlab("Education, avg. years, 2011")+ylab("Amenable mortality, 2009-13 average")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                                      axis.title=element_text(size=16,face="bold"))+geom_smooth()


#distribution of mortality, total
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of mortality rate across counties")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#distribution of mortality, cancer, male
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="male"&cause_name=="Rosszindulatú daganatok"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of cancer mortality rate across counties, males")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#distribution of mortality, cancer, female
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="female"&cause_name=="Rosszindulatú daganatok"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of cancer mortality rate across counties, females")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#distribution of mortality, amenable
ggplot(data=cbind(healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Elkerülhetõ halálozások"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of amenable mortality rate across counties")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))


#xxxx


View(healthdata_wide1_pm%>%filter(smr_type=="total_smr"&cause_name=="Összes halálozás")%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="total_smr"&cause_name=="belso_nemdohanyzo")%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="total_smr"&cause_name=="Keringési rendszer betegségei")%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="female_smr"&cause_name=="Rosszindulatú daganatok")%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="male_smr"&cause_name== "Dohányzásnak tulajdonítható")%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="total_smr"&cause_name== "Krónikus májbetegségek és cirrózis" )%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="total_smr"&cause_name== "Elkerülhetõ halálozások" )%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="male_smr"&cause_name== " A gége, a légcsõ, a hörgõ és tüdõ  rosszindulatú daganata " )%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="female_smr"&cause_name== "Vastagbél és végbél rosszindulatú daganata" )%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="female_smr"&cause_name== "Az emlõ rosszindulatú daganata" )%>%arrange(avg2009_2013))

View(healthdata_wide1_pm%>%filter(smr_type=="male_smr"&cause_name== "A prostata rosszindulatú daganata" )%>%arrange(avg2009_2013))


#description of variables
stargazer(cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),type = "text")

stargazer(cbind(healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),type = "html", out = "var_desc_pm.html")

#selecting data to examine
regr_data_pm<-healthdata_reduced_long_pm%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás")
regr_data_pm<-cbind(regr_data_pm, jarasok_regvars)

#summarizing by the top 10%, middle 80% and bottom 10%
groups=ifelse(regr_data_pm$smr<=quantile(regr_data_pm$smr,0.1),1,ifelse(regr_data_pm$smr>=quantile(regr_datapm$smr,0.9),3,2))
aggregate(regr_data_pm[,c(5:22)],FUN=mean,by=list(groups))
View(as.data.frame(aggregate(regr_data_pm[,c(5:22)],FUN=mean,by=list(groups))))

View(t(as.data.frame(aggregate(regr_data_pm[,c(5:22)],FUN=mean,by=list(groups)))))

write.table(t(as.data.frame(aggregate(regr_data_pm[,c(5:22)],FUN=mean,by=list(groups)))), "C:/Users/Zsopi/Google Drive/R/CEU_final_project/deciles_data_pm.txt", sep=",")


#summarizing by period
healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Összes halálozás")%>%group_by(period) %>%select(c(period,smr))%>%
        summarise_each(funs(mean, median, min, max, sd))

healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Rosszindulatú daganatok")%>%group_by(period) %>%select(c(period,smr))%>%
        summarise_each(funs(mean, median, min, max, sd))

healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="female"&cause_name=="Rosszindulatú daganatok")%>%group_by(period) %>%select(c(period,smr))%>%
        summarise_each(funs(mean, median, min, max, sd))

healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="male"&cause_name=="Rosszindulatú daganatok")%>%group_by(period) %>%select(c(period,smr))%>%
        summarise_each(funs(mean, median, min, max, sd))


healthdata_reduced_long_pm%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%group_by(period) %>%select(c(period,smr))%>%
        summarise_each(funs(mean, median, min, max, sd))


#######################APPENDIX_1: CALCULATIONS AND ADDITIONAL REGRESSIONS##############################

#amenable hypotetical (calculating effect of a hypotetical 1/3 improvement in amenable mortality)
data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás")%>%select(total_smr=smr),
            healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%select(amenable=smr),
            jarasok_regvars))%>%mutate(non_amen=total_smr-amenable)%>%
        mutate(amen_imprvd=amenable-(amenable-min(amenable))*2/3)%>%
        mutate(total_smr_imprvd=amen_imprvd+non_amen)

#calculating percentage decline in hypotetical scenario
(sum(data$pop*data$total_smr_imprvd)/sum(regr_data$pop))/(sum(data$pop*data$total_smr)/sum(regr_data$pop))-1


#additional regressions

#change regressions
reg7w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Összes halálozás"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Összes halálozás")%>%
                              select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),smr_chng~log(smr), weights = pop)
summary(reg7w)

reg8w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Összes halálozás"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Összes halálozás")%>%
                              select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),smr_chng~log(smr)+years_edu, weights = pop)
summary(reg8w)


reg9w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                              select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),smr_chng~log(smr), weights = pop)
summary(reg9w)

reg10w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                               select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),smr_chng~log(smr)+years_edu, weights = pop)
summary(reg10w)

reg11w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Keringési rendszer betegségei"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Keringési rendszer betegségei")%>%
                               select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),smr_chng~log(smr), weights = pop)
summary(reg11w)

reg12w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Keringési rendszer betegségei"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Keringési rendszer betegségei")%>%
                               select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),smr_chng~log(smr)+years_edu, weights = pop)
summary(reg12w)


reg13w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Rosszindulatú daganatok"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Rosszindulatú daganatok")%>%
                               select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),smr_chng~log(smr), weights = pop)
summary(reg13w)

reg14w<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Rosszindulatú daganatok"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Rosszindulatú daganatok")%>%
                               select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),smr_chng~log(smr)+years_edu, weights = pop)
summary(reg14w)



#presenting the regressions 4
stargazer(reg10w, reg12w, reg14w, reg15w,title="Regression Results",
          dep.var.labels.include = FALSE,
          covariate.labels = c("Log of initial mortality rate", "Education (avg. years)", "Mortality rate, excl. cancer, chng%"),
          dep.var.caption  = "Some convergence in mortality rates, cancer and other mortality not related",
          column.labels   = c("Amenable m., chng%","Cardio m., chng%","Cancer m., chng%","Cancer m., chng%"),
          add.lines = list(c("Weights", "population","population","population", "population")),
          type = "html",out = "regr_table4.html")


#diff in diff regressions
reg_dif_in_dif<-lm(data=(cbind(healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="belso")%>%select(jaras,belso_dif=smr),
                               healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso")%>%select(belso=smr),
                               healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%select(amenable_dif=smr),
                               healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%select(amenable=smr),
                               jarasok_regvars))%>%mutate(non_amen=belso-amenable)%>%
                           mutate(non_amen=belso-amenable)%>%
                           mutate(non_amen_dif=belso_dif-amenable_dif)%>%
                           mutate(non_amen_chg=non_amen_dif/non_amen)%>%
                           mutate(amen_chg=amenable_dif/amenable),
                   non_amen_chg~amen_chg, weights = pop)
summary(reg_dif_in_dif)


#####################APPENDIX_2: ADDITIONAL PLOTS#####

#diff in diff chart for amenable and other (internal cause) mortality
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="belso")%>%select(jaras,belso_dif=smr),
                   healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso")%>%select(belso=smr),
                   healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%select(amenable_dif=smr),
                   healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%select(amenable=smr),
                   jarasok_regvars))%>%mutate(non_amen=belso-amenable)%>%
               mutate(non_amen=belso-amenable)%>%
               mutate(non_amen_dif=belso_dif-amenable_dif)%>%
               mutate(non_amen_chg=non_amen_dif/non_amen)%>%
               mutate(amen_chg=amenable_dif/amenable),aes(amen_chg,non_amen_chg))+
        geom_point(aes(size=pop,color=years_edu))+geom_text(aes(label=jaras))+labs(title="Amenable and other mortality, change")+
        scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                         axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#plot comparing residuals
ggplot(data=(cbind(as.data.frame(regr_data),resid(reg2w),resid(reg3w),resid(reg5w),resid(reg6w),resid(reg2w_amen))), aes(resid(reg2w),resid(reg2w_amen)))+geom_point(aes(size=pop,color=income))+
        labs(title="Residuals compared")+
        xlab("Residuals of overall mortality, 2009-13 average")+ylab("Residuals of amenable mortality, 2009-13 average")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                                                                       axis.title=element_text(size=16,face="bold"))+geom_smooth()

#chart of amneable and other mortality
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                           select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),aes(smr_amenable,smr_nr))+geom_point(aes(size=pop,color=years_edu))+
        labs(title="Amenable and other mortality")+
        xlab("Amenable mortality")+ylab("Non-amenable, non-external, non-smoking related mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                                                   axis.title=element_text(size=16,face="bold"))+geom_smooth()


#chart of amneable and other mortality, no label
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                           select(smr_amenable=smr),jarasok_regvars))%>%mutate(smr_nr=smr-smr_amenable),aes(smr_amenable,smr_nr))+geom_point(aes(size=pop,color=years_edu))+
        labs(title="Amenable and other mortality")+
        xlab("Amenable mortality")+ylab("Non-amenable, non-external, non-smoking related mortality")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                       axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#level and change plots
#overall mortality rate and its change
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Összes halálozás"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Összes halálozás")%>%
                           select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),aes(smr,smr_chng))+geom_point(aes(size=pop,color=years_edu))+
        labs(title="Initial 2005-8 average mortality and its change")+
        xlab("2005-8 average mortality")+ylab("change in mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                   axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)+ scale_y_continuous(labels = scales::percent) 

#Amenable mortality and its change, "Elkerülhetõ halálozások"
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Elkerülhetõ halálozások")%>%
                           select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),aes(smr,smr_chng))+geom_point(aes(size=pop,color=years_edu))+
        labs(title="Initial 2005-8 average amenable mortality and its change")+
        xlab("2005-8 average amenable mortality")+ylab("change in amenable mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                                     axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)+ scale_y_continuous(labels = scales::percent) 

#Cardiovascular mortality rates "Keringési rendszer betegségei"
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Keringési rendszer betegségei"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Keringési rendszer betegségei")%>%
                           select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),aes(smr,smr_chng))+geom_point(aes(size=pop,color=years_edu))+
        labs(title="Initial 2005-8 average cardiovascular mortality and its change")+
        xlab("2005-8 average cardiovascular mortality")+ylab("change in cardiovascular mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                                                 axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)+ scale_y_continuous(labels = scales::percent) 

#Cancer mortality rates "Rosszindulatú daganatok"
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Rosszindulatú daganatok"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Rosszindulatú daganatok")%>%
                           select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),aes(smr,smr_chng))+geom_point(aes(size=pop,color=years_edu))+
        labs(title="Initial 2005-8 average cancer mortality and its change")+
        xlab("2005-8 average cancer mortality")+ylab("change in cancer mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                                 axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)+ scale_y_continuous(labels = scales::percent) 

#"Dohányzásnak tulajdonítható"
ggplot(data=(cbind(healthdata_reduced_long%>%filter(period=="avg2005_2008"&smr_type=="total"&cause_name=="Dohányzásnak tulajdonítható"),healthdata_reduced_long%>%filter(period=="t_diff"&smr_type=="total"&cause_name=="Dohányzásnak tulajdonítható")%>%
                           select(smr_chng=smr),jarasok_regvars))%>%mutate(smr_chng=smr_chng/smr),aes(smr,smr_chng))+geom_point(aes(size=pop,color=years_edu))+
        labs(title="2005-8 average smoking related mortality and its change")+
        xlab("2005-8 average smoking related mortality")+ylab("change in smoking related mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                                                                                                   axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)+ scale_y_continuous(labels = scales::percent) 

#distribution of mortality, cardiovascular
ggplot(data=cbind(healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="male"&cause_name=="Keringési rendszer betegségei"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of cardiovascular mortality rate across counties")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#distribution of mortality, smoking related
ggplot(data=cbind(healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Dohányzásnak tulajdonítható"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of smoking related mortality rate across counties")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#distribution of mortality, liver related
ggplot(data=cbind(healthdata_reduced_long%>%filter((period=="avg2009_2013"|period=="avg2005_2008")&smr_type=="total"&cause_name=="Krónikus májbetegségek és cirrózis"),jarasok_regvars),aes(smr, fill=period))+
        geom_density(alpha = 0.1)+labs(title="Distribution of liver related mortality rate across counties")+
        xlab("average smr, 2005-2008 and 2009-13")+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))

#education-mortality scatterplot 2
ggplot(data=regr_data,aes(log(tert_edu),smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+
        xlab("log(tertiary education)")+geom_text(aes(label=jaras))+ 
        scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                         axis.title=element_text(size=16,face="bold"))

#Social aid and smoking-related mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="male"&cause_name=="Dohányzásnak tulajdonítható"),jarasok_regvars),aes(log(social_aid),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Social aid and smoking-related mortality")+
        geom_text(aes(label=jaras))+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                     axis.title=element_text(size=16,face="bold"))

#income-XXX mortality scatterplot


#income and cardio mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Keringési rendszer betegségei"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and cardiovascular mortality")+
        xlab("log income")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                         axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income and cardio mortality, no labels
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Keringési rendszer betegségei"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and cardiovascular mortality")+
        xlab("log income")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                             axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#income and cancer mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Rosszindulatú daganatok"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and cancer mortality")+
        xlab("log income")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                         axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income and cancer mortality, no labels
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Rosszindulatú daganatok"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and cancer mortality")+
        xlab("log income")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                             axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#income and smoking related mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Dohányzásnak tulajdonítható"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and smoking related mortality")+
        xlab("log income")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                         axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income and smoking related mortality, no labels
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Dohányzásnak tulajdonítható"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and smoking related mortality")+
        xlab("log income")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                             axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#income and chronic liver disease/chirrosis mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Krónikus májbetegségek és cirrózis"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and chronic liver disease/chirrosis mortality")+
        xlab("log income")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                         axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income and chronic liver disease/chirrosis mortality, no labels
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Krónikus májbetegségek és cirrózis"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and chronic liver disease/chirrosis mortality")+
        xlab("log income")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                             axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#income and non-smoking related non-external mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and non-smoking related non-external mortality")+
        xlab("log income")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                         axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income and non-smoking related non-external mortality, no labels
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and non-smoking related non-external  mortality")+
        xlab("log income")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                             axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#income and amenable mortality
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="female"&cause_name=="Elkerülhetõ halálozások"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and 'avoidable'mortality")+
        xlab("log income")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                                                         axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income and amenable mortality, no labels
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="male"&cause_name=="Elkerülhetõ halálozások"),jarasok_regvars),aes(log(income),smr))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and 'amenable'mortality")+
        xlab("log income")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                                             axis.title=element_text(size=16,face="bold"))+geom_smooth(method = lm)

#education-XXXX mortality plots

ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()


ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Keringési rendszer betegségei"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and cardio mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Keringési rendszer betegségei"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and cardio mortality")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()


ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="female"&cause_name=="Rosszindulatú daganatok"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()


ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="male"&cause_name=="Rosszindulatú daganatok"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()



ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Krónikus májbetegségek és cirrózis"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Krónikus májbetegségek és cirrózis"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()



ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Dohányzásnak tulajdonítható"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Dohányzásnak tulajdonítható"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="male"&cause_name=="belso_nemdohanyzo"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="male"&cause_name=="belso_nemdohanyzo"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

# A gége, a légcsõ, a hörgõ és tüdõ  rosszindulatú daganata 
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="female"&cause_name==" A gége, a légcsõ, a hörgõ és tüdõ  rosszindulatú daganata "),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()


"Elkerülhetõ halálozások"
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="male"&cause_name=="Elkerülhetõ halálozások"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and 'avoidable' mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()


ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="female"&cause_name=="A légzõrendszer betegségei"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()


ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="male"&cause_name=="Cerebrovasculáris betegségek"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="male"&cause_name=="Vastagbél és végbél rosszindulatú daganata"),jarasok_regvars),aes(years_edu,smr))+
        geom_point(aes(size=pop,color=income))+labs(title="Education and mortality")+geom_text(aes(label=jaras))+ scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income-education scatterplot 1
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(log(income),years_edu))+
        geom_point(aes(size=pop,color=tert_edu))+labs(title="Income and education")+
        xlab("log income")+geom_text(aes(label=jaras))+ 
        scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                         axis.title=element_text(size=16,face="bold"))+geom_smooth()

#income-education scatterplot 2
ggplot(data=cbind(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="Összes halálozás"),jarasok_regvars),aes(income,tert_edu))+
        geom_point(aes(size=pop,color=years_edu))+labs(title="Income and mortality")+
        xlab("log income")+geom_text(aes(label=jaras))+ 
        scale_colour_gradientn(colours=rainbow(4))+theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
                                                         axis.title=element_text(size=16,face="bold"))

#visualizing trends in selected areas

ggplot(data = healthdata_narrow%>%filter(cause_name=="belso_nemdohanyzo")%>%filter(smr_type=="male_smr"), aes(year,smr))+
        geom_line(aes(fill=jaras))+stat_summary(fun.y = median, geom="line",color="red",size=1.5)+
        theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold"))+labs(title="Standardized total mortality rate, nonsmoking, non-accident")

ggplot(data = healthdata_narrow%>%filter(cause_name=="Dohányzásnak tulajdonítható")%>%filter(smr_type=="female_smr"), aes(year,smr))+
        geom_line(aes(fill=jaras))+stat_summary(fun.y = median, geom="line",color="red",size=1.5)+
        theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold"))+labs(title="Standardized total mortality rate")

ggplot(data = healthdata_narrow%>%filter(cause_name=="Keringési rendszer betegségei")%>%filter(smr_type=="male_smr"), aes(year,smr))+
        geom_line(aes(fill=jaras))+stat_summary(fun.y = median, geom="line",color="red",size=1.5)+
        theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold"))+labs(title="Standardized total mortality rate")

ggplot(data = healthdata_narrow%>%filter(cause_name=="Rosszindulatú daganatok")%>%filter(smr_type=="female_smr"), aes(year,smr))+
        geom_line(aes(fill=jaras))+stat_summary(fun.y = median, geom="line",color="red",size=1.5)+
        theme(plot.title=element_text(size=18, face="bold"), axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold"))+labs(title="Standardized total mortality rate")

#####################END OF APPENDIXES


#plotter codes
#BP alone
temp_names_keruletek<-temp_names_keruletek%>%arrange(abc_order)
temp_names_keruletek$variable<-resid(reg2w_amen)[20:42]
temp_names_keruletek<-temp_names_keruletek%>%arrange(orig_order)
keruletek_shp$var1=temp_names_keruletek$variable
spplot(keruletek_shp,z="var1")

#jarasok alone
temp_names_jarasok<-temp_names_jarasok%>%arrange(abc_order)
temp_names_jarasok$variable<-resid(reg2w_amen)[c(1:19,43:198)]
temp_names_jarasok<-temp_names_jarasok%>%arrange(orig_order)
jarasok_shp$var1=temp_names_jarasok$variable
spplot(jarasok_shp,z="var1")

#BP and jarasok combined
temp_names_jar_ker<-temp_names_jar_ker%>%arrange(abc_order)
temp_names_jar_ker$variable<-resid(reg2w_amen)
temp_names_jar_ker<-temp_names_jar_ker%>%arrange(orig_order)
jar_ker_shp$var1=temp_names_jar_ker$variable
spplot(jar_ker_shp,z="var1")


####APPENDIX_3: ALTERNATIVE MAP PLOTS######

#Bp
temp_names_keruletek<-temp_names_keruletek%>%arrange(abc_order)
temp_names_keruletek$variable<-(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name=="belso_nemdohanyzo")%>%select(smr))[20:42,]
temp_names_keruletek<-temp_names_keruletek%>%arrange(orig_order)
keruletek_shp$var1=temp_names_keruletek$variable
spplot(keruletek_shp,z="var1")

#jarasok
temp_data<-(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="female"&cause_name=="Dohányzásnak tulajdonítható")%>%select(smr))
temp_names_jarasok<-temp_names_jarasok%>%arrange(abc_order)
temp_names_jarasok$variable<-(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="female"&cause_name=="Dohányzásnak tulajdonítható")%>%select(smr))[c(1:19,43:198),]
temp_names_jarasok<-temp_names_jarasok%>%arrange(orig_order)
jarasok_shp$var1=temp_names_jarasok$variable
spplot(jarasok_shp,z="var1")

#BP and jarasok combined
temp_names_jar_ker<-temp_names_jar_ker%>%arrange(abc_order)
temp_names_jar_ker$variable<-(healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="female"&cause_name=="belso_nemdohanyzo")%>%select(smr))[1:198,]
temp_names_jar_ker<-temp_names_jar_ker%>%arrange(orig_order)
jar_ker_shp$var1=temp_names_jar_ker$variable
spplot(jar_ker_shp,z="var1")


#selecting data to plot

#cause of death data to plot
plotdata<-healthdata_reduced_long%>%filter(period=="avg2009_2013"&smr_type=="total"&cause_name==    "Összes halálozás" )


#regression residual to plot
plotdata<-as.data.frame(cbind(jaras=hd_jaras_names_shrt,smr=(resid(reg2w))),stringsAsFactors = F)
plotdata$smr<-as.numeric(plotdata$smr)

#variable to plot
plotdata<-as.data.frame(cbind(jaras=hd_jaras_names_shrt,smr=jarasok_regvars$income))
plotdata$smr<-as.numeric(as.character(plotdata$smr))

#whole country plot
ggplot(jar_ker_shp_ff)+geom_map(data = plotdata,aes(map_id=jaras,fill=smr),map=jar_ker_shp_ff)+ expand_limits(x = jar_ker_shp_ff$long, y = jar_ker_shp_ff$lat)+
        scale_fill_gradient2(name="Average standardized \nmortality 2009-13",low = "blue",  mid = "white", midpoint = median(plotdata$smr), high = "red", limits = c(min(plotdata$smr),max(plotdata$smr)))+
        theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank())

#Budapest plot
ggplot(keruletek_shp_ff)+geom_map(data = plotdata,aes(map_id=jaras,fill=smr),map=keruletek_shp_ff)+ expand_limits(x = keruletek_shp_ff$long, y = keruletek_shp_ff$lat)+
        scale_fill_gradient2(name="Average standardized \nmortality 2009-13",low = "blue",  mid = "white", midpoint = median(plotdata$smr), high = "red", limits = c(min(plotdata$smr),max(plotdata$smr)))+
        theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank())

