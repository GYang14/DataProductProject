####Load necessary libraries####
library(plyr)
library(dplyr)
library(caret)
library(woeBinning)
library(sqldf)
library(lubridate)
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations
library(PRROC) # for Precision-Recall curve calculations
library(data.table)# for easy manipulation of data table
library(zoo)
library(lubridate)
library(ggplot2)
library(scales)
library(fiftystater)
library(cdlTools)
library(LendingClub)

####Personal Setup####
setwd("C:/Users/yguan/Desktop/DataScienceApplication/LendingClub")
id<-43238102
API<-"fNNIde5ENcSNxEzmmk8YJdMsxAM="

####Download raw data and select variable based on meaning####
LC_CRED<-MakeCredential(id,API)
Listed<-ListedLoans()
Listed_Loans<-Listed$content$loans
listed_name_map<-read.csv("./Data/listed_name_map.csv",stringsAsFactors = F)
Listed_Loan_Name<-data.frame(orig_name=names(Listed_Loans),stringsAsFactors = F)
Listed_Loan_Name$row<-as.numeric(row.names(Listed_Loan_Name))
Listed_Loan_Name<-sqldf(
  "select distinct a.*,b.MarketNote_name as new_name 
  from Listed_Loan_Name a left join listed_name_map b
  on a.orig_name=b.listed_name
  order by row"
)
names(Listed_Loans)<-Listed_Loan_Name$new_name

MarketNote<-Listed_Loans

Read.LC.Loan.csv <- function(file, nrows = -1) {
  headers <- read.csv(
    file,
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
  headers <-
    gsub("verification_status_joint",
         "verified_status_joint",
         headers,
         fixed = TRUE)
  headers <-
    gsub("verification_status", "is_inc_v", headers, fixed = TRUE)
  tmp <- read.csv(file,
                  skip = 2,
                  header = F,
                  nrows = nrows)
  colnames(tmp) <- headers
  write.csv(tmp, file = "tmp.csv")
  return(tmp)
}

header_2007_2011 <-
  read.csv(
    "./Data/LoanStats3a_securev1.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2012_2013 <-
  read.csv(
    "./Data/LoanStats3b_securev1.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2014 <-
  read.csv(
    "./Data/LoanStats3c_securev1.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2015 <-
  read.csv(
    "./Data/LoanStats3d_securev1.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2016Q1 <-
  read.csv(
    "./Data/LoanStats_securev1_2016Q1.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2016Q2 <-
  read.csv(
    "./Data/LoanStats_securev1_2016Q2.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2016Q3 <-
  read.csv(
    "./Data/LoanStats_securev1_2016Q3.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2016Q4 <-
  read.csv(
    "./Data/LoanStats_securev1_2016Q4.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2017Q1 <-
  read.csv(
    "./Data/LoanStats_securev1_2017Q1.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2017Q2 <-
  read.csv(
    "./Data/LoanStats_securev1_2017Q2.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )
header_2017Q3 <-
  read.csv(
    "./Data/LoanStats_securev1_2017Q3.csv",
    skip = 1,
    header = F,
    nrows = 1,
    as.is = T
  )

identical(header_2015, header_2017Q3)
tmp <-
  Read.LC.Loan.csv(file = "./Data/LoanStats3a_securev1.csv", nrows = 10)
nzv <- nearZeroVar(MarketNote, freqCut = 999 / 1, uniqueCut = 0.1)
Var.MarketNote <- MarketNote[, -nzv]

comm.var <- intersect(names(Var.MarketNote), names(tmp))
MarketNote.only <- setdiff(names(Var.MarketNote), names(tmp))
LoanHist.only <- setdiff(names(tmp), names(Var.MarketNote))
print(MarketNote.only)
print(LoanHist.only)
featurePlot(
  x = Var.MarketNote[, c("effective_int_rate", "int_rate")],
  y = Var.MarketNote$service_fee_rate,
  plot = "scatter",
  type = c("p", "smooth"),
  span = .5,
  layout = c(2, 1)
)
summary(Var.MarketNote[, c("effective_int_rate",
                           "int_rate",
                           "service_fee_rate",
                           "exp_default_rate")])
#Should consider using effective interest rate only to select notes, but the issue is I cannot run back test with available data#
drop.var <- c("member_id", "url")
target.var <- c(
  "loan_status",
  "issue_d",
  "out_prncp",
  "out_prncp_inv",
  "total_pymnt",
  "total_pymnt_inv",
  "total_rec_prncp",
  "total_rec_int",
  "total_rec_late_fee",
  "funded_amnt_inv",
  "recoveries",
  "last_pymnt_d",
  "delinq_amnt",
  "policy_code"
)
model.var <- setdiff(c(comm.var,
                       target.var), drop.var)

####Define function to Load historical data, reformatting variables and then summary####
Load.Loan.Hist.by.Int <- function(file, int_cut, model.var) {
  tmp <- Read.LC.Loan.csv(file)
  tmp$int_rate <- as.numeric(sub("%", "", tmp$int_rate)) / 100
  tmp <- tmp[tmp$int_rate > int_cut, c(model.var)]
  return(tmp)
}

LC.Raw.Smmry <-
  function(file,
           rate_cut,
           model.var,
           grace_factor = 0.31,
           late_16_30_factor = 0.62,
           late_31_120_factor = 0.88,
           default_factor = 0.72,
           severity_factor = 1) {
    tmp <- Load.Loan.Hist.by.Int(file, rate_cut, model.var)
    tmp$monthly_inc <-
      ifelse(tmp$annual_inc / 12 <= 10000, round((tmp$annual_inc / 12) /
                                                               2000) * 2000, 10500)
    tmp$issue_date <-
      as.Date(paste0("01-", tmp$issue_d), format = "%d-%b-%Y")
    tmp$issue_year<-year(tmp$issue_date)
    tmp$issue_month<-month(tmp$issue_date)
    tmp$last_pymnt_date <-
      as.Date(paste0("01-", tmp$last_pymnt_d), format = "%d-%b-%Y")
    tmp$mob <-
      (as.yearmon(tmp$last_pymnt_date) - as.yearmon(tmp$issue_date)) * 12
    tmp$out_prncp_multiplier <-
      ifelse(
        tmp$loan_status == 'In Grace Period',
        1 - grace_factor,
        ifelse(
          tmp$loan_status == 'Late (16-30 days)',
          1 - late_16_30_factor,
          ifelse(
            tmp$loan_status == 'Late (31-120 days)',
            1 - late_31_120_factor,
            ifelse(tmp$loan_status == 'Default', 1 - default_factor, 
                   ifelse(tmp$loan_status == 'Charged Off',0, 1))
          )
        )
      )
    tmp$severity_multiplier <- severity_factor
    tmp1 <- sqldf(
      "select
      *
      ,case when mob is NULL then 12
      when mob<=0 then 1
      else mob end as adj_mob
      ,case when total_pymnt_inv IS NULL or total_pymnt_inv<0 then total_pymnt else total_pymnt_inv end as adj_total_pymnt_inv
      ,case when out_prncp_inv IS NULL or out_prncp_inv<0 then out_prncp else out_prncp_inv end as adj_out_prncp_inv
      ,case when funded_amnt_inv IS NULL or funded_amnt_inv<0 then funded_amnt else funded_amnt_inv end as adj_funded_amnt_inv
      
      
      from tmp
      where id is not null and loan_status not like '%Does not meet the credit policy%' and funded_amnt_inv>0
      
      "
    )
    
    tmp1$total_rec_prncp_inv<-tmp1$total_rec_prncp*tmp1$total_pymnt_inv/tmp1$total_pymnt
    tmp1$adj_out_prncp_inv<-(tmp1$adj_funded_amnt_inv-tmp1$total_rec_prncp_inv)
    tmp1$credit_loss_inv<- tmp1$adj_out_prncp_inv*(1-tmp1$out_prncp_multiplier*tmp1$severity_multiplier)
    
    tmp1$real_SAR <-
      ((
        tmp1$adj_total_pymnt_inv
        -tmp1$total_rec_prncp_inv
        
        - tmp1$credit_loss_inv
      ) / ((tmp1$adj_funded_amnt_inv+tmp1$adj_out_prncp_inv)/2)+1
      ) ^ (1 / (tmp1$adj_mob / 12)) - 1
    
    tmp1$real_SAR<-ifelse(tmp1$adj_total_pymnt_inv==0&tmp1$out_prncp_inv==0,-1,tmp1$real_SAR)
    
    smmry <- sqldf(
      "select
      case when purpose in ('wedding','renewable_energy','house','educational') then 'other'
      else purpose end as purpose
      ,addr_state
      ,issue_date
      ,issue_year
      ,issue_month
      ,case when issue_month in (1,2,3) then 'Q1'
      when issue_month in (4,5,6) then 'Q2'
      when issue_month in (7,8,9) then 'Q3'
      else 'Q4' end as issue_qtr
      ,grade
      ,term
      ,loan_status
      ,home_ownership
      ,case when pub_rec>=1 then 'with public record'
      else 'without public record' end as public_record
      ,case when int_rate<0.07 then 'a.<7.00%'
      when int_rate<0.11 then 'b.7.00%-10.99%'
      when int_rate<0.15 then 'c.11.00%-14.99%'
      when int_rate<0.20 then 'd.15.00%-19.99%'
      else 'e.>20.00%' end as interest_rate
      ,case when inq_last_6mths<4 then inq_last_6mths
      else 4 end as inq_last_6mths
      ,case when fico_range_low<=670 then '660-670'
      when fico_range_low<=690 then '671-690'
      when fico_range_low<=710 then '691-710'
      when fico_range_low<=800 then '711-800'
      else '801+' end as credit_score
      ,case when is_inc_v in ('Source Verified','Verified') then 'Verified'
      else 'Not Verified' end as Income_verified
      ,monthly_inc
      ,case when delinq_2yrs ==0 then '0'
      when delinq_2yrs>=1 and delinq_2yrs<=3 then '1-3'
      when delinq_2yrs>=4 then '4+'
      else 'NA' end as delinq_2yrs_group
      ,count(distinct id) as loan_count
      ,sum(loan_amnt) as loan_amnt
      ,sum(out_prncp) as out_prncp
      ,sum(total_rec_prncp) as total_rec_prncp
      ,sum(total_rec_int) as total_rec_int
      ,sum(total_rec_late_fee) as total_rec_late_fee
      ,sum(recoveries) as recoveries
      ,sum(total_pymnt) as total_pymnt
      ,sum(adj_funded_amnt_inv) as investor_funded_amt
      ,sum(case when loan_status not in ('Current',
      'Does not meet the credit policy. Status:Fully Paid',
      'Fully Paid') then 1 else 0 end) as bad_loan
      ,sum(adj_total_pymnt_inv) as total_pymnt_inv
      ,sum(adj_funded_amnt_inv*int_rate) as amnt_inv_x_int_rate
      ,sum(adj_funded_amnt_inv*real_SAR) as amnt_inv_x_SAR
      from tmp1
      group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17
      "
    )
    return(smmry)
  }
####Loan Data Summary####
smmry_20072011 <-
  LC.Raw.Smmry("./Data/LoanStats3a_securev1.csv", -1, model.var)
smmry_20122013 <-
  LC.Raw.Smmry("./Data/LoanStats3b_securev1.csv", -1, model.var)
smmry_2014 <-
  LC.Raw.Smmry("./Data/LoanStats3c_securev1.csv", -1, model.var)
smmry_2015 <-
  LC.Raw.Smmry("./Data/LoanStats3d_securev1.csv", -1, model.var)
smmry_2016Q1 <-
  LC.Raw.Smmry("./Data/LoanStats_securev1_2016Q1.csv", -1, model.var)
smmry_2016Q2 <-
  LC.Raw.Smmry("./Data/LoanStats_securev1_2016Q2.csv", -1, model.var)
smmry_2016Q3 <-
  LC.Raw.Smmry("./Data/LoanStats_securev1_2016Q3.csv", -1, model.var)
smmry_2016Q4 <-
  LC.Raw.Smmry("./Data/LoanStats_securev1_2016Q4.csv", -1, model.var)
smmry_2017Q1 <-
  LC.Raw.Smmry("./Data/LoanStats_securev1_2017Q1.csv", -1, model.var)
smmry_2017Q2 <-
  LC.Raw.Smmry("./Data/LoanStats_securev1_2017Q2.csv", -1, model.var)
smmry_2017Q3 <-
  LC.Raw.Smmry("./Data/LoanStats_securev1_2017Q3.csv", -1, model.var)
smmry_20072017 <-
  rbind(
    smmry_20072011,
    smmry_20122013,
    smmry_2014,
    smmry_2015
    ,
    smmry_2016Q1,
    smmry_2016Q2,
    smmry_2016Q3,
    smmry_2016Q4
    ,
    smmry_2017Q1,
    smmry_2017Q2,
    smmry_2017Q3
  )
smmry_20072017$loan_amnt<-as.numeric(smmry_20072017$loan_amnt)
smmry_20072017$purpose<-as.factor(as.character(smmry_20072017$purpose))
levels(smmry_20072017$purpose)<-c("Car","Credit Card","Debt Consolidation","Home Improvement",
                        "Major Purchase","Medical","Moving","Other",
                        "Small Business","Vacation")
smmry_20072017$public_record<-as.factor(smmry_20072017$public_record)
smmry_20072017$interest_rate<-as.factor(smmry_20072017$interest_rate)
smmry_20072017$credit_score<-as.factor(smmry_20072017$credit_score)
smmry_20072017$Income_verified<-as.factor(smmry_20072017$Income_verified)
smmry_20072017$delinq_2yrs_group<-as.factor(smmry_20072017$delinq_2yrs_group)

saveRDS(smmry_20072017, './Data/smmry_20072017.rds')
smmry_20072013<-smmry_20072017[smmry_20072017$issue_year<=2013,] 
smmry_2014<-smmry_20072017[smmry_20072017$issue_year==2014,]
smmry_2015<-smmry_20072017[smmry_20072017$issue_year==2015,]
smmry_2016<-smmry_20072017[smmry_20072017$issue_year==2016,]
smmry_2017<-smmry_20072017[smmry_20072017$issue_year==2017,]
save(smmry_20072013,file="./Data/smmry_20072013.Rda")
save(smmry_2014,file="./Data/smmry_2014.Rda")
save(smmry_2015,file="./Data/smmry_2015.Rda")
save(smmry_2016,file="./Data/smmry_2016.Rda")
save(smmry_2017,file="./Data/smmry_2017.Rda")

####Load Loan Summary###
smmry_20072017<-readRDS('./Data/smmry_20072017.rds')
str(smmry_20072017)
####Highlights####

dollar_by_issue<-sqldf("select issue_year,issue_qtr,sum(loan_amnt) as loan_amnt from smmry_20072017 group by 1,2")
facet_bar_plot<-function(data,x,facet,y,title,xlab,ylab,yformat=dollar_format()){
  tmp<-data[,c(x,facet,y)]
  names(tmp)<-c("x","facet","y")
theme_set(theme_bw())
ggplot(tmp, aes(x = x, y=y)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  facet_grid(.~ facet, space="free_x", scales="free_x", switch="x") +
  labs(title=title,x=xlab,y=ylab) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6),strip.placement = "outside",
        panel.spacing=unit(0,"cm"))+
  scale_y_continuous( label=yformat)
}

dollar_by_issue<-sqldf("select issue_year,issue_qtr,sum(loan_amnt/1000000) as loan_in_M from smmry_20072017 group by 1,2")
facet_bar_plot(dollar_by_issue,x="issue_qtr",facet="issue_year",y="loan_in_M",title="Loan Issuance by Quarter",xlab = "Issuance Quarter",ylab="Loan Amount (million)")
dollar_by_issue$cum_loan_dollar<-cumsum(dollar_by_issue$loan_in_M)
facet_bar_plot(dollar_by_issue,x="issue_qtr",facet="issue_year",y="cum_loan",title="Total Loan Issuance",xlab = "Issuance Quarter",ylab="Loan Amount (million)")

count_by_issue<-sqldf("select issue_year,issue_qtr,sum(loan_count) as loan from smmry_20072017 group by 1,2")
count_by_issue$cum_loan<-cumsum(count_by_issue$loan)
facet_bar_plot(count_by_issue,x="issue_qtr",facet="issue_year",y="loan",title="Total Loan Issuance",xlab = "Issuance Quarter",ylab="Loan Count",yformat=comma)

facet_class_bar_plot<-function(data,x,facet,y,class,title,xlab,ylab,yformat=dollar_format()){
  tmp<-data[,c(x,facet,y,class)]
  names(tmp)<-c("x","facet","y","class")
  theme_set(theme_bw())
  ggplot(tmp, aes(x = x, y=y)) + 
    geom_bar(stat="identity", width=.5, aes(fill=class)) + 
    facet_grid(.~ facet, space="free_x", scales="free_x", switch="x") +
    labs(title=title,x=xlab,y=ylab) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6),strip.placement = "outside",
          panel.spacing=unit(0,"cm"))+
    scale_y_continuous( label=yformat)
}
count_by_issue_grade<-sqldf("select issue_year,issue_qtr,grade,sum(loan_count) as loan from smmry_20072017 group by 1,2,3")
facet_class_bar_plot(count_by_issue_grade,x="issue_qtr",facet="issue_year",y="loan",class="grade",title="Loan Issuance by Risk",xlab = "Issuance Quarter",ylab="Loan Count",yformat=comma)

class_pie_plot<-function(data,class,y,title){
  tmp<-data[,c(y,class)]
  names(tmp)<-c("y","class")
  tmp <- tmp %>%
    # factor levels need to be the opposite order of the cumulative sum of the values
    mutate(Group = factor(class, levels=rev(levels(class))),
           cumulative = cumsum(y),
           midpoint = cumulative - y / 2,
           label = paste0(Group, " ", round(y / sum(y) * 100, 1), "%"),
           ypct=y / sum(y))
  theme_set(theme_classic())
  ggplot(tmp, aes(x = "", y=y,fill=Group)) + 
    geom_bar(stat="identity", width=1,colour="black") + 
    labs(title=title,fill="class",x=NULL,y=NULL) + 
    theme(axis.line = element_blank(), 
          plot.title = element_text(hjust=0.5))+
    geom_text(aes(x = 1.3, y = midpoint, label = label))+
    coord_polar(theta = "y", start=0)+
  scale_y_continuous(labels = scales::percent)+
    scale_fill_hue(c=65,l=80)
}
count_by_purpose<-sqldf("select purpose,sum(loan_count) as loan from smmry_20072017 group by 1")
count_by_purpose$loan_pct<-count_by_purpose$loan/sum(count_by_purpose$loan)
count_by_purpose<-
  sqldf("select case when loan_pct<0.02 then 'other' else purpose end as purpose
        ,sum(loan_pct) as loan_pct from count_by_purpose group by 1")

class_pie_plot(data=count_by_purpose,class="purpose",y="loan_pct", title="Reported Loan Purpose")

loan_by_state<-
  sqldf("select addr_state,sum(loan_amnt/1000000.0) as loan_amnt_in_M from smmry_20072017 group by 1")
loan_by_state$state<-tolower(lapply(X = loan_by_state$addr_state,FUN = fips, to='Name'))
head(loan_by_state)

state_heatmap<-function(data,state,y,ylab,yformat,title){
  tmp<-data[,c(y,state)]
  names(tmp)<-c("y","state")
  p<-ggplot(tmp, aes(map_id = state)) + 
    # map points to the fifty_states shape data
    geom_map(aes(fill = y), map = fifty_states) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
    labs(title=title,x = "", y = "") +
    theme(legend.position = "bottom",
          panel.background = element_blank(),legend.text = element_text(size = 8, colour = "black"))
  
  # add border boxes to AK/HI
  p + fifty_states_inset_boxes() +scale_fill_continuous(name = ylab,label=yformat,low ="#56B1F7"  , high = "#132B43")
}
state_heatmap(data=loan_by_state,state="state",y="loan_amnt_in_M",ylab ="Loan Issuance (in Million)" ,yformat=dollar_format(),title="Total Loan Issuance by State")

####Loan Statistics####
##Last Quarter Average Interest Rate##
last_qtr_int_rate_by_term<-sqldf("select term,sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as interest_rate from smmry_20072017 where issue_year=2017 and issue_qtr='Q3' group by 1")
last_qtr_int_rate<-sqldf("select sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as interest_rate from smmry_20072017 where issue_year=2017 and issue_qtr='Q3'")
last_qtr_int_rate$term<-"All"
last_qtr_int_rate_by_term<-rbind(last_qtr_int_rate_by_term,last_qtr_int_rate)
print(last_qtr_int_rate_by_term)
##Historical Returns by Grade##
hist_return_by_grade<-
sqldf("
select 
case when grade in ('A','B','C','D','E') then grade
else 'F+G' end as grade,
sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as int_rate,
sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as SAR
from smmry_20072017
where issue_year*100+issue_month<=201603 
group by 1
      ")
str(hist_return_by_grade)
hist_return_by_grade.m<-melt(hist_return_by_grade,id.vars='grade')
ggplot(hist_return_by_grade.m, aes(grade, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")+scale_y_continuous(label=percent)
##Average Interest Rate##
facet_line_plot<-function(data,x,facet,y,title,xlab,ylab,yformat=percent){
  tmp<-data[,c(x,facet,y)]
  names(tmp)<-c("x","facet","y")
  theme_set(theme_bw())
  ggplot(tmp, aes(x = x, y=y)) + 
    geom_line(aes(y=y)) + 
    facet_grid(.~ facet, space="free_x", scales="free_x", switch="x") +
    labs(title=title,x=xlab,y=ylab) + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6),strip.placement = "outside",
          panel.spacing=unit(0,"cm"))+
    scale_y_continuous( label=yformat,limits = c(0.05,0.2))
}
avg_int_rate<-sqldf("select issue_date,sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as int_rate from smmry_20072017 where issue_year>=2008   group by 1 ")
str(avg_int_rate)
avg_int_rate$facet<-"Issuance Date"
facet_line_plot(data=avg_int_rate,x="issue_date",facet="facet",y="int_rate",title="Average Interest Rate",xlab='',ylab='Interest Rate')
##GRADE MIX OVER TIME##
loan_by_issue_grade<-sqldf("select issue_year,grade,sum(loan_amnt) as loan_amnt from smmry_20072017 group by 1,2")
loan_by_issue_grade$loan_amnt<-as.numeric(loan_by_issue_grade$loan_amnt)
loan_by_issue_grade<-group_by(loan_by_issue_grade,issue_year) %>% mutate(loan_pct = loan_amnt/sum(loan_amnt))
loan_by_issue_grade$x<-""
facet_class_bar_plot(loan_by_issue_grade,x="x",facet="issue_year",y="loan_pct",class="grade",title="Grade Mix over Time",xlab = "Issuance Year",ylab="Loan Amount Percent",yformat=percent)
##Loan Performance Details##
loan_perf<-
  sqldf(
    "select case when grade in ('A','B','C','D','E') then grade
      else 'F+G' end as grade
      ,sum(loan_amnt) as total_issued
      ,sum(case when loan_status  in (
      'Does not meet the credit policy. Status:Fully Paid',
      'Fully Paid') then loan_amnt else 0 end) as fully_paid
      ,sum(case when loan_status  in ('Current','In Grace Period') then out_prncp else 0 end) as current
      ,sum(case when loan_status  not in ('Current','In Grace Period',
      'Does not meet the credit policy. Status:Fully Paid',
      'Fully Paid') then out_prncp else 0 end) as late
      ,sum(case when loan_status  like '%Charged Off%' then loan_amnt-total_rec_prncp-recoveries else 0 end) as NCO
      ,sum(total_rec_prncp) as total_rec_prncp
      ,sum(total_rec_int) as total_rec_int
      ,sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as int_rate
      ,sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as NAR
      ,sum(loan_count) as total_issued_count
      ,sum(case when loan_status like '%Fully%' then loan_count else 0 end) as fully_paid_count
      ,sum(case when loan_status like '%Current%' then loan_count else 0 end) as current_count
      ,sum(case when loan_status not like '%Current%'
           and loan_status not like '%Fully%'
           and loan_status not like '%Charged%' then loan_count else 0 end) as late_count
      ,sum(case when loan_status like '%Charged%' then loan_count else 0 end) as charged_off_count
      from smmry_20072017
      group by 1
    "
  )
loan_perf$total_issued_pct<-loan_perf$total_issued/loan_perf$total_issued
loan_perf$fully_paid_pct<-loan_perf$fully_paid/loan_perf$total_issued
loan_perf$current_pct<-loan_perf$current/loan_perf$total_issued
loan_perf$late_pct<-loan_perf$late/loan_perf$total_issued
loan_perf$NCO_pct<-loan_perf$NCO/loan_perf$total_issued
loan_perf$total_rec_prncp_pct<-loan_perf$total_rec_prncp/loan_perf$total_issued
loan_perf$total_rec_int_pct<-loan_perf$total_rec_int/loan_perf$total_issued
print(loan_perf)
##NET ANNUALIZED RETURN BY VINTAGE##
hist_return_by_vintage<-
  sqldf("
        select 
        issue_year,
        sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as int_rate,
        sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as SAR
        from smmry_20072017
         
        group by 1
        ")
str(hist_return_by_vintage)
hist_return_by_vintage.m<-melt(hist_return_by_vintage,id.vars='issue_year')
ggplot(hist_return_by_vintage.m, aes(issue_year, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")+scale_y_continuous(label=percent)+theme_gray()
