############## libraries needed
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(openxlsx)
library(tidyverse)

#devtools::install_github("mrdwab/koboloadeR")
library(koboloadeR)

##note that a key function name in koboloader changed at some point from
#"get_host" to "host"
#make sure you update if you have previously loaded an older version


#functions in separate file. Within the folder - check working directory is set to same place
#setwd("C:/Users/sdumb/Dropbox (SSD)/ssd-shiny-server/ODKLink")
source("functions.R")
###########


#user inputs

#location of the form
formfile<-"dummy form.xlsx"

#server used - can be 'kobo', 'ona' or 'kobohr'
servername<-"kobo"

#username and password with data read access
username<-"user"
password<-"pass"

#if you are not sure what the numeric code should be run the following commented
#out code to see the list of forms associated with the account the number needed
#is the id column

#available_data(paste(username,password,sep=":"),api = servername)


#numeric id for form
form_id<-"id"



#key column to split results by. Can allow this to vary, but difficult to set a
#default value that would be sensible for this column so forcing it to be picked
#up front.

#splitcolumn must match column name from R exactly
splitcolumn<-"strata"
splitlabel="Strata"

#if the split column is within a group it should be in form "groupname.varname"
#if the split column is anything other than a select_one then it won't work & I question your decisions
####

#end of user inputs


#non reactive pre-processing of form:
format_form<-ODKForm4R(formfile)

#extract this for some ease later
form<-format_form$form
labels<-format_form$labels

#pull out questions from form
#if changes are made to the form 
quests<-form %>%
        filter(class%in%c("select_one","select_multiple","integer","decimal")&repeatlevel==0) %>%
          mutate(grouplabel=ifelse(grouplabel=="","Main form",grouplabel),
            grouplabel=factor(grouplabel,levels=unique(grouplabel)))%>%
          select(label,grouplabel)

questionnairelist<-split(quests$label,quests$grouplabel)
######



# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",title = "Linking ODK Form",
                    
          dashboardHeader(title="Linking ODK Form",   
                                    titleWidth = 450),
     
#Two tabs just to have the code for multiple tabs shown Data load tab is
#indicative mostly just to show that this is not "fast" - hence why having it
#fully live is not a good idea

dashboardSidebar(sidebarMenu(id="tabs",
                                                 menuItem( tabName ="GetData", text =  "Load Data"),     
                                                 menuItem( tabName ="Q1", text =  "Questionnaire Responses"))
                                     
                    ),
                    
                    dashboardBody(     
                      tabItems(
                        
                        tabItem(
                          tabName="GetData",
                          fluidRow(actionButton("LoadData",label = "Click Here To Load Data")),
                          box(textOutput("Data_Message"))
                        ),
                        
                        
                        tabItem(
                          tabName="Q1",
                          fluidRow( 
                            box(
                              pickerInput("question",label="Question",choices=questionnairelist,options=list(`live-search`=TRUE), 
                                          choicesOpt = list(
                                            style = rep(("color: black"),length(unlist(questionnairelist))),
                                            content = stringr::str_trunc(unlist(questionnairelist), width = 100))
                              )
                            )
                          ),
                          
                          #settings on this box are an attempt to make the box autosize around the table and also turn into a scrollable
                          #box rather than disappearing off the page if it becomes too wide
                          fluidRow(style='display:flex',
                                   box(div(style = 'overflow-x: scroll',
                                           htmlOutput("question"),
                                           tableOutput("QuestionTab")),
                                       width=NULL)
                          ),
                          
                        )
                        

                        
                      )                                
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #if the load data button is pressed then update the message saying how long it takes for the data to load
  observeEvent(input$LoadData,{
    output$Data_Message<-renderText(paste("Data loaded in",round(time$time,2),"seconds"))
  })
  
  #set data and time to be reactive values
  data1<-reactiveValues()
  time<-reactiveValues()
  
  #if load data button is pushed then load the data
  observeEvent(input$LoadData,{
    
    #time check
    t1<-Sys.time()
    #paste user and password together
    p1<-paste(username,password,sep=":")
    
    #conenct to server using koboloader function get_data
    All <-get_data(p1,formid=form_id,api=servername)
    
    #do data formatting based on my formt_odk function
    data1$x1<-formt_odk(data=All,form1=format_form)
    #post stuff time check
    t2<-Sys.time()
    #get time elased 
    time$time<-difftime(t2,t1)
    
  })
  
  
  

  output$QuestionTab <-renderTable({
    
    #make sure the data is a dataframe and not a tibble for compatibility
    out1<-as.data.frame(data1$x1) 
    #quick cheat for splitting out factor column
    out1$split_by<-factor(out1[,splitcolumn])

   out1<- out1%>%
      filter(is.na(split_by)==FALSE)
    
    
    #determine type of question from input
    class1<-na.omit(form$class[form$label==input$question])
  
  #if single select
      if(class1=="select_one"){
        
        #make temporary variable based on input selection
        out1$tmp<-out1[,na.omit(form$varname[form$label==input$question])]
        #create output data by filtering to selected countries and running function
        
        #get sample sizes for each group
        n<-data.frame(table(out1$split_by[is.na(out1[,na.omit(form$varname[form$label==input$question])])==FALSE]))
        colnames(n)<-c("Var1","n")
        
        #get the percentages for each response within each group and make it look a bit nice
        t1<-data.frame(prop.table(table(out1$split_by,
                                        out1$tmp),1)) %>%
          inner_join(n,by="Var1") %>%
          mutate(Freq=scales::percent(Freq,accuracy = 0.1)) %>%
          spread(Var2,Freq)
        
        colnames(t1)[1]<-splitlabel
        
      }
      
      if(class1=="select_multiple"){
        
        #if select multiple take the columns for that question
        key<-paste0(na.omit(form$varname[form$label==input$question]),".")
        multis<-grep(key,colnames(out1),fixed = TRUE)
        
        
        #get the labels for each column
        varlabels<-labels$label[labels$list_name==na.omit(form$list_name[form$label==input$question])]
        
        #get sample sizes
        t1<-data.frame(split=paste0(levels(out1$split_by)," n=",table(out1$split_by[is.na(out1[,multis[1]])==FALSE])))

        
        #probably a more elegant way of summarising these columns. summarise_at probably
        #But looping over them will work for now
        t0<-NULL
        for(i in multis){
          out1$tmp<-out1[,i]
   
          a<-out1 %>%
            group_by(split_by) %>%
            dplyr::summarise(x=scales::percent(mean(tmp,na.rm=TRUE),0.1))
          t0<-cbind(t0,as.vector(a$x))
        }
        
        
        #make it all look somewhat pretty and merge in the labels
        t1<-cbind(t1,t0)
    colnames(t1)<-c("split",varlabels)
    
          t1<- t1 %>%
          gather("Response","value",-(split)) %>%
          mutate(Response=reorder(as.factor(Response),as.numeric(substr(value,1,nchar(value)-1)),mean)) %>%
          spread(split,value) %>%
          arrange(desc(Response))
      }
    
    
      if(class1=="integer"|class1=="decimal"){
        
        out1$tmp<-out1[,na.omit(form$varname[form$label==input$question])]
        #create output data by filtering to selected countries and running function
 
        #summary stats
        #feel free to modify which stats get presented
        
        t1<-out1 %>%
          group_by(split_by) %>%
          dplyr::summarise(n=sum(is.na(tmp)==FALSE),Mean=mean(tmp,na.rm=TRUE),
                           `Min`=min(tmp,na.rm=TRUE),
                           `Median`=median(tmp,na.rm=TRUE),
                           `Max`=max(tmp,na.rm=TRUE))
        
        colnames(t1)[1]<-splitlabel
      }
    
  
    t1
    
  },digits=1,align = "r")
  
  output$question<-renderText({
    paste("<font size=\"5\" color=\"#112446\"><b>&nbsp;&nbsp;&nbsp;",input$question,"</b></font><br>")
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
