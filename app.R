####Load necessary libraries####
library(data.table)# for easy manipulation of data table
library(sqldf)
library(lubridate)
library(lubridate)
library(ggplot2)
library(scales)
library(fiftystater)
library(cdlTools)
library(LendingClub)
library(shiny)
library(repmis)

####Personal Setup####

#setwd("C:/Users/yguan/Desktop/DataScienceApplication/LendingClub")
#source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_20072014.Rda?raw=true")
source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2015.Rda?raw=true")
source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2016.Rda?raw=true")
source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2017.Rda?raw=true")
smmry<-rbind(smmry_2015,smmry_2016,smmry_2017)
rm(smmry_2015,smmry_2016,smmry_2017)
vintage<-sqldf("select distinct issue_year||issue_qtr as vintage from smmry
            ")
vintage$vintage<-as.factor(vintage$vintage)

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
          submitButton(text = "Refresh"),
          selectInput("vintage", label="Loan Vintage", 
                      choices=levels(vintage$vintage),
                      multiple = T,
                      selectize = TRUE),
          selectInput("term", label="Loan Term", 
                      choices=levels(smmry$term),
                      multiple = T,
                      selectize = TRUE),
          selectInput("purpose", label="Loan Purpose", 
                      choices=levels(smmry$purpose),
                      multiple = T,
                      selectize = TRUE),
          selectInput("addr_state", label="Location State", 
                      choices=levels(smmry$addr_state),
                      multiple = T,
                      selectize = TRUE),
          sliderInput("monthly_inc","Monthly Income (>$10,000 are grouped into $10,500)",min=0,max=10500,value=c(0,10500),step=1000),
          selectInput("dlq", label="Delinquencies (Last 2 yrs)", 
                      choices=levels(smmry$delinq_2yrs_group),
                      multiple = T,
                      selectize = TRUE),
          selectInput("pub_rec", label="Public Record", 
                      choices=levels(smmry$public_record),
                      multiple = T,
                      selectize = TRUE),
          selectInput("grade", label="Risk Grade", 
                      choices=levels(smmry$grade),
                      multiple = T,
                      selectize = TRUE),
          selectInput("int_rate", label="Interest Rate", 
                      choices=levels(smmry$interest_rate),
                      multiple = T,
                      selectize = TRUE),
          selectInput("homeowner", label="Homer Ownership", 
                      choices=levels(smmry$home_ownership),
                      multiple = T,
                      selectize = TRUE),
          sliderInput("inq_last_6mths","Inquiries within 6 Months before Issuance (4+ inquiries are grouped into 4)",min=0,max=4,value=4,step=1),
          selectInput("credit", label="Credit Score", 
                      choices=levels(smmry$credit_score),
                      multiple = T,
                      selectize = TRUE),
          selectInput("Income_verified", label="Income Verified", 
                      choices=levels(smmry$Income_verified),
                      multiple = T,
                      selectize = TRUE)
  
          
          
        ),
        mainPanel(tabsetPanel(tabPanel(
          "Standard Plot", 
          plotOutput("plot1"),
          verbatimTextOutput("test")
        )))
      )
      
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  dataInput <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating")
    if(length(input$vintage)>0){
      selected_vintage<-data.frame(vintage=input$vintage)
    }else{
      selected_vintage<-data.frame(vintage=levels(vintage$vintage))
    } 
    if(length(input$term)>0){
      selected_term<-data.frame(term=input$term)
    }else{
      selected_term<-data.frame(term=levels(smmry$term))
    } 
    if(length(input$purpose)>0){
      selected_purpose<-data.frame(purpose=input$purpose)
    }else{
      selected_purpose<-data.frame(purpose=levels(smmry$purpose))
    }
    if(length(input$addr_state)>0){
      selected_state<-data.frame(addr_state=input$addr_state)
    }else{
      selected_state<-data.frame(addr_state=levels(smmry$addr_state))
    }
    
    selected_mth_inc<-data.frame(min=min(input$monthly_inc),max=max(input$monthly_inc))
    if(length(input$dlq)>0){
      selected_dlq<-data.frame(dlq=input$dlq)
    }else{
      selected_dlq<-data.frame(dlq=levels(smmry$delinq_2yrs_group))
    }
    if(length(input$pub_rec)>0){
      selected_pub_rec<-data.frame(pub_rec=input$pub_rec)
    }else{
      selected_pub_rec<-data.frame(pub_rec=levels(smmry$public_record))
    }
    if(length(input$grade)>0){
      selected_grade<-data.frame(grade=input$grade)
    }else{
      selected_grade<-data.frame(grade=levels(smmry$grade))
    }
    if(length(input$int_rate)>0){
      selected_int_rate<-data.frame(int_rate=input$int_rate)
    }else{
      selected_int_rate<-data.frame(int_rate=levels(smmry$interest_rate))
    }
    if(length(input$homeowner)>0){
      selected_homeowner<-data.frame(homeowner=input$homeowner)
    }else{
      selected_homeowner<-data.frame(homeowner=levels(smmry$home_ownership))
    }
    selected_inq<-data.frame(inq=max(input$inq_last_6mths))
    if(length(input$credit)>0){
      selected_credit<-data.frame(credit=input$credit)
    }else{
      selected_credit<-data.frame(credit=levels(smmry$credit_score))
    }  
    if(length(input$Income_verified)>0){
      selected_incv<-data.frame(Income_verified=input$Income_verified)
    }else{
      selected_incv<-data.frame(Income_verified=levels(smmry$Income_verified))
    }   
    
    tmp<-sqldf(
      "select 
      issue_year,
      sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as int_rate,
      sum(amnt_inv_x_SAR)/sum(investor_funded_amt) as SAR
      from smmry a
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
      
      group by 1
      ")
    tmp
  })
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
  
  
  output$plot1 <- renderPlot({
    
    
    data<-melt(dataInput(),id.vars='issue_year')  
    
    
    ggplot(data, aes(issue_year, value)) +   
      geom_bar(aes(fill = variable), position = "dodge", stat="identity")+scale_y_continuous(label=percent)+theme_gray()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

