require(shiny)
require(lattice)
require(RColorBrewer)


ui<-shinyUI(fluidPage(
  titlePanel("Titanic Data"),
  sidebarLayout(
    sidebarPanel(
      
      
      actionButton("button", "Buy a Ticket"),
      
      br(),
      
      selectInput("select1", label = h4("Sort by..."),
                  choices = list("Class", "Age",
                                 "Sex"), selected = "Class"),
      br(),
      
      
      h4("Would you Survive?"),
      
      selectInput("age", label=h4("Age"),
                  choices=list("0-10","11-20", "21-30", "31-40",
                               "41-50", "51,60", "61-70", "71+"), "0-10")
    ),
    mainPanel(
      
      helpText(   a("Click Here First",     
                    href="https://www.youtube.com/watch?v=mCEfqj9pDAI",
                    target="_blank")
      ),
      helpText(   a("For Caroline",     
                    href="https://www.youtube.com/watch?v=FHG2oizTlpY",
                    target="_blank")
      ),
      
      br(),
      
      h2("Survival Statistics on the Titanic", style = "color:purple", align="center"),
      
      plotOutput("mystat"),
      
      
      
      h4(textOutput("view"),style = "color:red", align="center"),
      h4(textOutput("other"),style = "color:purple", align="center")
      
      
    )
  )
))

server<-shinyServer(function (input, output) {
  
  titanic<- read.table("Titanic.txt", header=T, sep="\t")
  titanic$Name<-as.character(titanic$Name)
  
  zeroten<- subset(titanic, Age>0 & Age<10)
  tentwenty<- subset(titanic, Age>10.01 & Age<20)
  twentythirty<- subset(titanic, Age>20.01 & Age<30)
  thirtyforty<- subset(titanic, Age>30.01 & Age<40)
  fortyfifty<- subset(titanic, Age>40.01 & Age<50)
  fiftysixty<- subset(titanic, Age>50.01 & Age<60)
  sixtyseventy<- subset(titanic, Age>60.01 & Age<70)
  seventyeighty<- subset(titanic, Age>70.01)
  
  view<- eventReactive(input$button, {
    p=(reactiveValues(person = sample(1:1313, 1)))
    button<-input$button
    who<-titanic[p$person,1]
    class<-titanic[p$person,2]
    viab<-if (titanic[p$person, 6]==1) {
      print ("You survived!")
    } else {
      (print ("You died"))
    }
    paste(who,class,viab,sep="   /   ")
  })
  
  output$view <- renderText(view())
  
  
  output$mystat<- renderPlot({
    choice1<- switch(input$select1,
                     "Class"= 1,
                     "Age"= 3,
                     "Sex"= 2)
    
    sortchart<- barchart(apply(Titanic, c(choice1, 4), sum), 
                         data=titanic, col=brewer.pal(4, "PRGn"))
    
    print(sortchart)
    
    
  })
  
  
  output$other<- renderText({
    
    agechoice<- switch(input$age,
                       "0-10" = zeroten,
                       "11-20"= tentwenty,
                       "21-30" = twentythirty,
                       "31-40" = thirtyforty,
                       "41-50" = fortyfifty,
                       "51,60" = fiftysixty,
                       "61-70" = sixtyseventy,
                       "71+" = seventyeighty)
    
    
    
    
    surv<- NULL
    for (i in (1:1313)){
      pls<- subset(agechoice[i,], Survived==1)
      surv<- rbind(surv,pls)
    }
    
    
    dead<- NULL
    for (i in (1:1313)){
      pls<- subset(agechoice[i,], Survived==0)
      dead<- rbind(dead,pls)
    }
    
    comeout<-nrow(surv)/((nrow(surv)+nrow(dead)))
    
    print(comeout)
    
  })
  
})

shinyApp(ui = ui, server = server)