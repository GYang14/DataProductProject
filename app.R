####Load necessary libraries####
library(data.table)# for easy manipulation of data table
library(sqldf)
library(lubridate)
library(ggplot2)
library(scales)
library(fiftystater)
library(cdlTools)
library(LendingClub)
library(shiny)
library(repmis)
library(plotly)

####Personal Setup####



source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/vintage.Rda?raw=true")
source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/state.Rda?raw=true")

# setwd("U:/Documents/Professional EDU/DataScience/RStudio/LC/Competitor Research/LCInvestmentAdvisor")
# load("./smmry_2015.Rda")
# load("./smmry_2016.Rda")
# load("./smmry_2017.Rda")


# Define UI for application that draws a histogram
ui <-  navbarPage(
  "Lending Club Investment Advisor",
  tabPanel(
    "Historical Performance",
    fluidPage(
      titlePanel("Lending Club Historical Performance"),
      sidebarLayout(
        sidebarPanel(
          
          h3("Loan Filter(Blank Means Overall)"),
          splitLayout( submitButton(text = "Refresh Filters"),
                       downloadButton("downloadData", "Download Filtered Data")
          ),
          
          selectInput("vintage", label="Loan Vintage", 
                      choices=levels(vintage$vintage),
                      selected = c("2017Q1","2017Q2","2017Q3"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("term", label="Loan Term", 
                      choices=c(" 36 months"," 60 months"),
                      selected = c(" 36 months"),
                      
                      multiple = T,
                      selectize = TRUE),
          selectInput("purpose", label="Loan Purpose", 
                      choices=c("Car","Credit Card","Debt Consolidation","Home Improvement","Major Purchase",
                                "Medical","Moving","Other","Small Business","Vacation" ),
                      selected = c("Car","Credit Card","Debt Consolidation","Home Improvement"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("addr_state", label="Location State", 
                      choices=levels(state$addr_state),
                      selected = c("CA","TX","NY","PA"),
                      multiple = T,
                      selectize = TRUE),
          sliderInput("monthly_inc","Monthly Income (>=$12,000 are grouped into $12,000)",min=0,max=12000,value=c(0,12000),step=1000),
          selectInput("dlq", label="Delinquencies (Last 2 yrs)", 
                      choices=c( "0", "1-3", "4+" ),
                      multiple = T,
                      selectize = TRUE),
          selectInput("pub_rec", label="Public Record", 
                      choices=c("with public record","without public record"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("grade", label="Risk Grade", 
                      choices=c("A", "B", "C", "D", "E", "F", "G"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("int_rate", label="Interest Rate", 
                      choices=c("a.<7.00%","b.7.00%-10.99%","c.11.00%-14.99%", "d.15.00%-19.99%", "e.>20.00%" ),
                      multiple = T,
                      selectize = TRUE),
          selectInput("homeowner", label="Homer Ownership", 
                      choices=c("MORTGAGE", "NONE","OTHER","OWN","RENT","ANY"),
                      multiple = T,
                      selectize = TRUE),
          sliderInput("inq_last_6mths","Inquiries within 6 Months before Issuance (4+ inquiries are grouped into 4)",min=0,max=4,value=4,step=1),
          selectInput("credit", label="Credit Score", 
                      choices=c("660-670" ,"671-690" ,"691-710" ,"711-800" ,"801+"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("Income_verified", label="Income Verified", 
                      choices=c("Not Verified","Verified"),
                      multiple = T,
                      selectize = TRUE)
          
          
          
        ),
        mainPanel(tabsetPanel(tabPanel(
          "Standard Plot", 
          h4(textOutput("test")),
          plotlyOutput("plot1"),
          plotlyOutput("plot2"),
          plotlyOutput("plot3"),
          plotlyOutput("plot4")
          
        ),
        tabPanel(
          "Backtest Summary", 
          tableOutput("summary1"),
          tableOutput("summary2"),
          tableOutput("summary3"),
          tableOutput("summary4"),
          tableOutput("summary5")
        )
        ))
      )
      
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataInput <- reactive({

    if(length(input$vintage)>0){
      selected_vintage<-data.frame(vintage=input$vintage)
    }else{
      selected_vintage<-data.frame(vintage=levels(vintage$vintage))
    } 
    if(length(input$term)>0){
      selected_term<-data.frame(term=input$term)
    }else{
      selected_term<-data.frame(term=c(" 36 months"," 60 months"))
    } 
    if(length(input$purpose)>0){
      selected_purpose<-data.frame(purpose=input$purpose)
    }else{
      selected_purpose<-data.frame(purpose=c("Car","Credit Card","Debt Consolidation","Home Improvement","Major Purchase",
                                             "Medical","Moving","Other","Small Business","Vacation" ))
    }
    if(length(input$addr_state)>0){
      selected_state<-data.frame(addr_state=input$addr_state)
    }else{
      selected_state<-data.frame(addr_state=levels(state$addr_state))
    }
    
    selected_mth_inc<-data.frame(min=min(input$monthly_inc),max=max(input$monthly_inc))
    if(length(input$dlq)>0){
      selected_dlq<-data.frame(dlq=input$dlq)
    }else{
      selected_dlq<-data.frame(dlq=c( "0", "1-3", "4+" ))
    }
    if(length(input$pub_rec)>0){
      selected_pub_rec<-data.frame(pub_rec=input$pub_rec)
    }else{
      selected_pub_rec<-data.frame(pub_rec=c("with public record","without public record"))
    }
    if(length(input$grade)>0){
      selected_grade<-data.frame(grade=input$grade)
    }else{
      selected_grade<-data.frame(grade=c("A", "B", "C", "D", "E", "F", "G"))
    }
    if(length(input$int_rate)>0){
      selected_int_rate<-data.frame(int_rate=input$int_rate)
    }else{
      selected_int_rate<-data.frame(int_rate=c("a.<7.00%","b.7.00%-10.99%","c.11.00%-14.99%", "d.15.00%-19.99%", "e.>20.00%" ))
    }
    if(length(input$homeowner)>0){
      selected_homeowner<-data.frame(homeowner=input$homeowner)
    }else{
      selected_homeowner<-data.frame(homeowner=c("MORTGAGE", "NONE","OTHER","OWN","RENT","ANY"))
    }
    selected_inq<-data.frame(inq=max(input$inq_last_6mths))
    if(length(input$credit)>0){
      selected_credit<-data.frame(credit=input$credit)
    }else{
      selected_credit<-data.frame(credit=c("660-670" ,"671-690" ,"691-710" ,"711-800" ,"801+"))
    }  
    if(length(input$Income_verified)>0){
      selected_incv<-data.frame(Income_verified=input$Income_verified)
    }else{
      selected_incv<-data.frame(Income_verified=c("Not Verified","Verified"))
    }
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading 2015 Summary")
    source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2015.Rda?raw=true")
    tmp_2015<-sqldf(
      "select 
      a.*
      from smmry_2015 a
      inner join selected_vintage b
      on a.issue_year||a.issue_qtr=b.vintage
      inner join selected_term b1
      on a.term=b1.term
      inner join selected_purpose c
      on a.purpose=c.purpose
      inner join selected_state d
      on a.addr_state=d.addr_state
      inner join  selected_mth_inc e
      on a.monthly_inc>=e.min and a.monthly_inc<=e.max
      inner join selected_dlq f
      on a.delinq_2yrs_group=f.dlq
      inner join selected_pub_rec g
      on a.public_record=g.pub_rec
      inner join selected_int_rate h
      on a.interest_rate=h.int_rate
      inner join selected_grade i
      on a.grade=i.grade
      inner join selected_homeowner j
      on a.home_ownership=j.homeowner
      inner join selected_inq k
      on a.inq_last_6mths <= k.inq
      inner join selected_credit l
      on a.credit_score = l.credit
      inner join selected_incv m
      on a.Income_verified = m.Income_verified
      ")
    rm(smmry_2015)

    progress$set(message = "Loading 2016 Summary")
    source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2016.Rda?raw=true")
    tmp_2016<-sqldf(
      "select 
      a.*
      from smmry_2016 a
      inner join selected_vintage b
      on a.issue_year||a.issue_qtr=b.vintage
      inner join selected_term b1
      on a.term=b1.term
      inner join selected_purpose c
      on a.purpose=c.purpose
      inner join selected_state d
      on a.addr_state=d.addr_state
      inner join  selected_mth_inc e
      on a.monthly_inc>=e.min and a.monthly_inc<=e.max
      inner join selected_dlq f
      on a.delinq_2yrs_group=f.dlq
      inner join selected_pub_rec g
      on a.public_record=g.pub_rec
      inner join selected_int_rate h
      on a.interest_rate=h.int_rate
      inner join selected_grade i
      on a.grade=i.grade
      inner join selected_homeowner j
      on a.home_ownership=j.homeowner
      inner join selected_inq k
      on a.inq_last_6mths <= k.inq
      inner join selected_credit l
      on a.credit_score = l.credit
      inner join selected_incv m
      on a.Income_verified = m.Income_verified
      ")
    rm(smmry_2016)

    progress$set(message = "Loading 2017 Summary")
    source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2017.Rda?raw=true")
    tmp_2017<-sqldf(
      "select 
      a.*
      from smmry_2017 a
      inner join selected_vintage b
      on a.issue_year||a.issue_qtr=b.vintage
      inner join selected_term b1
      on a.term=b1.term
      inner join selected_purpose c
      on a.purpose=c.purpose
      inner join selected_state d
      on a.addr_state=d.addr_state
      inner join  selected_mth_inc e
      on a.monthly_inc>=e.min and a.monthly_inc<=e.max
      inner join selected_dlq f
      on a.delinq_2yrs_group=f.dlq
      inner join selected_pub_rec g
      on a.public_record=g.pub_rec
      inner join selected_int_rate h
      on a.interest_rate=h.int_rate
      inner join selected_grade i
      on a.grade=i.grade
      inner join selected_homeowner j
      on a.home_ownership=j.homeowner
      inner join selected_inq k
      on a.inq_last_6mths <= k.inq
      inner join selected_credit l
      on a.credit_score = l.credit
      inner join selected_incv m
      on a.Income_verified = m.Income_verified
      ")
    rm(smmry_2017)
    tmp<-rbind(tmp_2015,tmp_2016,tmp_2017)
    rm(tmp_2015,tmp_2016,tmp_2017)
    tmp
  })
  
  
  facet_class_bar_plot<-function(data,x,facet,y,class,position="stack",title,xlab="",xsize=8,ylab="",classlab=class,yformat=dollar_format()){
    tmp<-data[,c(x,facet,y,class)]
    names(tmp)<-c("x","facet","y","class")
    theme_set(theme_bw())
    p<-ggplot(tmp, aes(x = x, y=y)) + 
      geom_bar(stat="identity",position=position, width=.5, aes(fill=class)) + 
      facet_grid(.~ facet, space="free_x", scales="free_x", switch="x") +
      labs(title=title,x=xlab,y=ylab,fill = classlab) + 
      theme(axis.text.y = element_text(angle=90),axis.text.x = element_text(angle=0, size =xsize,hjust = 0.5),strip.placement = "outside",
            legend.title = element_text(angle=0, size =xsize*1.5),
            panel.ontop = F,
            panel.spacing=unit(0,"cm"))+
      scale_y_continuous( label=yformat)
    return(p)
  }
  
  
  
  
  output$plot1 <- renderPlotly({
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Plot1")
      data<-dataInput()
      dollar_by_issue<-sqldf("select issue_year,issue_qtr,      
      purpose,sum(loan_amnt/1000000) as loan_amount from data group by 1,2,3")
      p<-facet_class_bar_plot(dollar_by_issue,x="issue_qtr",facet="issue_year",y="loan_amount",class="purpose",title="Loan Issuance (in $Million) by Issuance Date and Purpose",xlab = "",ylab="",classlab="Purpose")
      ggplotly(p)
    }
    else{
      return(NULL)
    }
  })
  
  output$plot2 <- renderPlotly({
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Plot2")
      data<-dataInput()
      loan_by_state<-
        sqldf("select issue_year,addr_state,sum(loan_amnt/1000000.0) as loan_amnt_in_M from data group by 1,2")
      loan_by_state$issue_year<-as.factor(loan_by_state$issue_year)
      loan_by_state$facet<-""
      p<-facet_class_bar_plot(loan_by_state,x="addr_state",facet="facet",y="loan_amnt_in_M",class="issue_year",title="Loan Issuance by State",xsize=6.5,classlab="Issuance Year")
      
      ggplotly(p)
    }
    else{
      return(NULL)
    }
    
  })
  output$plot3 <- renderPlotly({
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Plot3")
      
      data<-dataInput()
      loan_by_issue_grade<-sqldf("select issue_year,
                                       case when grade in ('A','B','C','D','E') then grade
      else 'F+G' end as grade,
                                 loan_status,sum(loan_amnt/1000000) as loan_amnt from data group by 1,2,3")
      loan_by_issue_grade$loan_status<-as.character(loan_by_issue_grade$loan_status)
      loan_by_issue_grade$loan_status<-factor(loan_by_issue_grade$loan_status,levels = c("Current","In Grace Period","Fully Paid","Late (16-30 days)","Late (31-120 days)","Default","Charged Off"))
      p<-facet_class_bar_plot(loan_by_issue_grade,x="grade",facet="issue_year",y="loan_amnt",class="loan_status",title="Loan Origination Amount(in $Million) by Latest Loan Status",classlab="Latest Loan Status")
      ggplotly(p)
    } else{
      return(NULL)
    }
  })
  output$plot4 <- renderPlotly({
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Plot4")
      
      data<-dataInput()
      hist_return_by_grade<-
        sqldf("
              select 
              issue_year,
                    case when grade in ('A','B','C','D','E') then grade
      else 'F+G' end as grade,
              sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
              sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as 'Risk Adjusted Return'
              from data
              group by 1,2
              ")
      hist_return_by_grade.m<-melt(hist_return_by_grade,id.vars=c('issue_year','grade'))
      p<-facet_class_bar_plot(hist_return_by_grade.m,x="grade",facet="issue_year",y="value",class="variable",position="dodge",title="Interest Rate and Risk Adjusted Return",xlab = "",ylab="",classlab="Rate",yformat=percent_format())
      ggplotly(p)
    } else{
      return(NULL)
    }
    
    
  })
  
  
  output$test <- renderText({
    if(nrow(dataInput())==0)
      print("Filters Resulting Empty Set, Please Change Filters")
    
  })

  output$summary1 <- renderTable({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 1")
    data<-dataInput()
    smmry<-sqldf("select 
                 sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(case when loan_status in ('Charged Off') then loan_amnt-total_rec_prncp else 0 end)/
                 sum(loan_amnt) as 'Unannualized Loss Rate',
                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(adj_mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data ")
    smmry$`Risk Adjusted Return`<-percent(smmry$`Risk Adjusted Return`)
    smmry$`Interest Rate`<-percent(smmry$`Interest Rate`)
    smmry$`Unannualized Loss Rate`<-percent(smmry$`Unannualized Loss Rate`)
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry
        
  }, caption = "All Matching Loans",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  output$summary2 <- renderTable({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 2")
    data<-dataInput()
    smmry<-sqldf("select grade as 'GRADE',
                 sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(case when loan_status in ('Charged Off') then loan_amnt-total_rec_prncp else 0 end)/
                 sum(loan_amnt) as 'Unannualized Loss Rate',
                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(adj_mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data
                 group by 1")
    smmry$`Risk Adjusted Return`<-percent(smmry$`Risk Adjusted Return`)
    smmry$`Interest Rate`<-percent(smmry$`Interest Rate`)
    smmry$`Unannualized Loss Rate`<-percent(smmry$`Unannualized Loss Rate`)
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry
    
  }, caption = "GRADE",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  output$summary3 <- renderTable({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 3")
    data<-dataInput()
    smmry<-sqldf("select purpose as 'PURPOSE',
                 sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(case when loan_status in ('Charged Off') then loan_amnt-total_rec_prncp else 0 end)/
                 sum(loan_amnt) as 'Unannualized Loss Rate',
                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(adj_mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data
                 group by 1")
    smmry$`Risk Adjusted Return`<-percent(smmry$`Risk Adjusted Return`)
    smmry$`Interest Rate`<-percent(smmry$`Interest Rate`)
    smmry$`Unannualized Loss Rate`<-percent(smmry$`Unannualized Loss Rate`)
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry
    
  }, caption = "PURPOSE",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  output$summary4 <- renderTable({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 4")
    data<-dataInput()
    smmry<-sqldf("select issue_year as 'ISSUE YEAR',
                 sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(case when loan_status in ('Charged Off') then loan_amnt-total_rec_prncp else 0 end)/
                 sum(loan_amnt) as 'Unannualized Loss Rate',
                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(adj_mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data
                 group by 1")
    smmry$`Risk Adjusted Return`<-percent(smmry$`Risk Adjusted Return`)
    smmry$`Interest Rate`<-percent(smmry$`Interest Rate`)
    smmry$`Unannualized Loss Rate`<-percent(smmry$`Unannualized Loss Rate`)
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry
    
  }, caption = "ISSUE YEAR",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$summary5 <- renderTable({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 5")
    data<-dataInput()
    smmry<-sqldf("select term as 'TERM',
                 sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(case when loan_status in ('Charged Off') then loan_amnt-total_rec_prncp else 0 end)/
                 sum(loan_amnt) as 'Unannualized Loss Rate',
                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(adj_mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data
                 group by 1")
    smmry$`Risk Adjusted Return`<-percent(smmry$`Risk Adjusted Return`)
    smmry$`Interest Rate`<-percent(smmry$`Interest Rate`)
    smmry$`Unannualized Loss Rate`<-percent(smmry$`Unannualized Loss Rate`)
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry
    
  }, caption = "TERM",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      "summary.csv"
    },
    content = function(file) {
      write.csv(dataInput(), file, row.names = FALSE)
    }
  )
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)