# Contributions
# Grittapard Keeratimongkollert: Demand Generation & Plots
# Tan Yi Shu, Asta: Game Calculations and Computations
# Davina Faith Hadinyoto: Game States and Animation
# Siew Yik Fong: Game Structure and Compilation


# At the beginning of any R session, record your AWS database password:
#source("setAWSPassword.R")
#student100 pw: #WHzKJZ!rwJ#

source("usePackages.R")
pkgnames <- c("tidyverse","shiny","DBI","jsonlite","shinydashboard","shinyjs","ggplot2","plotly")
loadPkgs(pkgnames)

#References
#https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
#https://shiny.rstudio.com/reference/shiny/0.14/passwordInput.html
#https://shiny.rstudio.com/articles/sql-injections.html
#https://shiny.rstudio.com/reference/shiny/0.14/renderUI.html


#Generate, or re-generate, HTML to create modal dialog for Password creation
passwordModal <- function(failed = FALSE) {
  modalDialog(
    title = "Create a new password",
    textInput("newplayername", "Enter a username:"),
    passwordInput("password1", "Enter a new password:"),
    passwordInput("password2", "Confirm by re-entering the new password:"),
    #"If successful, you will be assigned a Player Name to go with this password.",
    if (failed)
      div(tags$b("The passwords do not match. Try again.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("passwordok", "OK")
    )
  )
}

loginModal <- function(failed = FALSE) {
  modalDialog(
    title = "Login",
    textInput("playername", "Enter your assigned Player Name", "FrostyFuzzyPickle"),
    passwordInput("password3", "Enter your password:"),
    if (failed)
      div(tags$b("There is no registered player with that name and password. Try again or re-register.", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("loginok", "OK")
    )
  )
}

finishModal <- function(finish=TRUE) {
  modalDialog(
    title = "You have completed the game!",
    div(tags$b("Congratulations you have reached the end of the game! Check the leaderboard to see how well you did!", style = "color: blue;")),
    
    footer = tagList(
      modalButton("OK")
    )
  )
}

# Function to insert game result data into the leaderboard table
insertLeaderboardData <- function(playername, cashbalance, time) {
  conn <- getAWSConnection()  # Open the database connection
  # Create the INSERT query with placeholders for the values
  querytemplate <- "INSERT INTO Leaderboard (playername, cashbalance, time) VALUES (?id1,?id2, NOW())"
  # Use sqlInterpolate to fill in the placeholders with actual values
  query <- sqlInterpolate(conn, querytemplate, id1=playername, id2=cashbalance)
  # Execute the query to insert the data into the table
  dbExecute(conn, query)
  dbDisconnect(conn)  # Close the database connection
}
getLeaderBoard <- function(){
  conn <- getAWSConnection()
  query <- "SELECT playername AS 'PlayerName', cashbalance AS 'Cash Balance' from Leaderboard ORDER BY cashbalance DESC,time ASC"
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

# ERROR MODALS
error1Modal <- function(failed = FALSE) {
  modalDialog(
    title = "ERROR",
    if (failed) {
      tags$div(tags$b("PLEASE SELECT DELIVERY MODE AND TYPE!", style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel")
    )
  )
}

error2Modal <- function(failed = FALSE) {
  modalDialog(
    title = "ERROR",
    if (failed) {
      tags$div(tags$b("USE A NON-NEGATIVE INTEGER VALUE!", style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel")
    )
  )
}

notloggedinModal <- function(failed = FALSE) {
  modalDialog(
    title = "ERR0R",
    if (failed) {
      tags$div(tags$b("PLEASE REGISTER OR LOGIN TO PLAY THE GAME!", style = "color: red;"))
    },
    footer = tagList(
      modalButton("Cancel")
    )
  )
}

getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student100",
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student100",
    password = getOption("AWSPassword"))
  conn
}

getPlayerID <- function(playername,password){
  
  #open the connection
  conn <- getAWSConnection()
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholders for playername and password
  querytemplate <- "SELECT * FROM LeaderPlayer WHERE playername=?id1 AND password=?id2;"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
  print(query) #for debug
  result <- dbGetQuery(conn,query)
  # If the query is successful, result should be a dataframe with one row
  if (nrow(result)==1){
    playerid <- result$playerid[1]
  } else {
    print(result) #for debugging
    playerid <- 0
  }
  #print(result)
  #print(playerid)
  #Close the connection
  dbDisconnect(conn)
  # return the playerid
  playerid
}

createNewPlayerQuery <- function(conn,playername,password){
  #password could contain an SQL insertion attack
  #Create a template for the query with placeholder for  password
  querytemplate <- "INSERT INTO LeaderPlayer (playername,password) VALUES (?id1,?id2);"
  query<- sqlInterpolate(conn, querytemplate,id1=playername,id2=password)
}

registerPlayer <- function(vals, password){
  #open the connection
  conn <- getAWSConnection()
  playername <- vals$newplayername
  #playername <- getRandomPlayerName(conn) #remove
  query <- createNewPlayerQuery(conn,playername,password)
  #print(query) #for debug
  # This query could fail to run properly so we wrap it in a loop with tryCatch()
  success <- FALSE
  iter <- 0
  MAXITER <- 5
  while(!success & iter < MAXITER){
    iter <- iter+1
    tryCatch(
      
      {  # This is not a SELECT query so we use dbExecute
        result <- dbExecute(conn,query)
        print(result)
        success <- TRUE
      }, error=function(cond){print("registerPlayer: ERROR")
        print(cond)
        # The query failed, likely because of a duplicate playername
        playername <- vals$newplayername
        query <- createNewPlayerQuery(conn,playername,password) }, 
      warning=function(cond){print("registerPlayer: WARNING")
        print(cond)},
      finally = {print(paste0("Iteration ",iter," done."))
      }
    )
  } # end while loop
  # This may not have been successful
  if (!success) playername = NULL
  #Close the connection
  dbDisconnect(conn)
  playername
}

############## UI STARTS HERE ###############

ui <- dashboardPage(
  dashboardHeader(title = "Wine Boutique Game"),
  dashboardSidebar(
    sidebarMenu(
      #https://fontawesome.com/icons?d=gallery
      menuItem("Login", tabName = "welcome", icon = icon("door-open")),
      menuItem("Game", tabName = "game", icon = icon("wine-bottle")),
      menuItem("Info", tabName = "info", icon = icon("circle-info")),
      menuItem("Chart",tabName = "chart",icon = icon("chart-line")),
      menuItem("Leaderboard", tabName = "leaderboard",icon = icon("ranking-star"))
    )
    ),
  dashboardBody(

  tabItems(
    # First tab content
    tabItem(tabName = "welcome",
            fluidPage(
              fluidRow(
                column(2,
                       fluidRow(
                         h2("Login Here", style = "font-weight:bold;")),
                       fluidRow(
                         div(
                           actionButton("register", "Register"),
                           actionButton("login", "Login"))),
                       fluidRow(
                         div(
                           tags$h4("Logged in as:"),
                           htmlOutput("loggedInAs")))),
                column(9,
                       div(
                         style = "position: relative; top: 0px; left: 50px;",
                         fluidRow(
                           tags$h3("Instructions", style = "font-weight: bold;")),
                         fluidRow(
                           div("🍷📅 Welcome to the Wine Boutique Management Challenge! 
                              🎉 Get ready for an exciting 53-week journey as the boss of your very own Wine Boutique. 
                              🏰 In this game, you're the one calling the shots! 💡 You'll decide how many bottles to order, when to reorder, and how to get them delivered. 
                              But, be on your toes! ⚡️ You've got to keep enough stock to fulfill their orders. No backorders! 
                              Watch out for holding costs too – they increase as our stock grows. 💰 Let's be savvy and manage our inventory wisely.
                              🎁 Oh, and guess what? There are special occasions like Christmas and Chinese New Year when demand spikes, so be sure to plan ahead! Have fun! ",
                               "By the way, you still can click 'info' and 'chart' for more help during your exploration!"))
                       ))),
              fluidRow(
                tags$br()
                ),
              div(tags$img(src="login.png", height='600px',width='1063px'),
                  style = "position: relative; top: 0px; left: 45px;"),
              )
    ), #tab 1 ends here
    
    # Second tab content
    tabItem(tabName = "game",
            fluidPage(
              fluidRow(
                # MAIN GAME INTERFACE
                column(6, 
                       tags$div(
                         fluidRow(column(5,
                                         div(
                                           style = "display: inline-block; margin-right: 5px; margin-bottom: 10px; font-size: 23px; font-weight:bold;",
                                           "Week:"
                                         ),
                                         div(
                                           style = "display: inline-block; font-size: 23px; font-weight:bold;",
                                           textOutput("weeknumber")
                                         ))),
                         fluidRow(column(7,
                                         div(
                                           style = "display: inline-block; margin-right: 5px; margin-bottom: 10px; font-size: 23px; font-weight:bold;",
                                           "Cash Balance:"
                                         ),
                                         div(
                                           style = "display: inline-block; font-size: 23px; font-weight:bold;",
                                           textOutput("cashbalance")
                                         ))),
                         fluidRow(column(6,
                                         div(
                                           style = "display: inline-block; margin-right: 5px; margin-bottom: 5px; font-size: 19px; font-weight:bold;",
                                           "Weekly Demand:"
                                         ),
                                         div(
                                           style = "display: inline-block; font-size: 19px; font-weight:bold;",
                                           textOutput("weeklydemand")
                                         )),
                                  column(6,
                                         div(
                                           style = "display: inline-block; margin-right: 5px; font-size: 19px; font-weight:bold;",
                                           "Weekly Revenue:"
                                         ),
                                         div(
                                           style = "display: inline-block; font-size: 19px; font-weight:bold;",
                                           textOutput("revenue")
                                         ))),
                         fluidRow(column(6 ,
                                         div(
                                           style = "display: inline-block; margin-right: 5px; margin-bottom: 5px; font-size: 16px;",
                                           "Warehouse Stock: "
                                         ),
                                         div(
                                           style = "display: inline-block; font-size: 16px;",
                                           textOutput("warehousestock")
                                         )),
                                  
                                  column(6, 
                                         div(
                                           style = "display: inline-block; margin-right: 5px; font-size: 16px;",
                                           "Boutique Stock: "
                                         ),
                                         div(
                                           style = "display: inline-block; font-size: 16px;",
                                           textOutput("boutiquestock")
                                         ))),
                         fluidRow(column(12,
                                         div(
                                           style = "display: inline-block; margin-right: 5px; margin-bottom: 5px; font-size: 16px;",
                                           "Stock In Transit:"
                                         ),
                                         div(
                                           style = "display: inline-block; font-size: 16px;",
                                           textOutput("stockintransit"),
                                         ))),
                         #startbutton gets replaced by nextbutton after being clicked
                         div(id = "button_wrapper",
                             fluidRow(column(12, tags$div(actionButton("startbutton", "START"), uiOutput("notloggedin")))),
                             style = "position: relative; top: 0px; left: 0px;"
                             ),
                         
                         div(
                           imageOutput("gif"),
                           style = "position: relative; top: 0px; left: 69px; width: 5%; height: 5%;")
                         )
                       ), #end column
                
                # USER INPUT PANEL
                column(6,
                       box(
                         width = 12,
                         height = 170,
                         fluidRow(column(12,
                                         div(
                                           style = "display: inline-block; margin-right: 5px; margin-bottom: 5px; font-size: 20px; font-weight:bold;",
                                           "Order Sheet"
                                         ))),
                         fluidRow(column(3,
                                         fluidRow(
                                           column(12,
                                                  div(
                                                    style = "display: inline-block; margin-right: 5px; font-size: 15px; font-weight:bold;",
                                                    "Order Quantity:"
                                                  ))),
                                         fluidRow(
                                           column(12,
                                                  div(
                                                    style = "display: inline-block; margin-right: 5px; font-size: 13px;",
                                                    textInput("orderquantity","Enter Value", "0"),
                                                    tags$head(tags$style(type="text/css", "#orderquantity {width: 70px}")), #adjusts text input box width
                                                    #textOutput("orderquantity"), #for debug
                                                    #textOutput("selectedmode"), #for debug
                                                    #textOutput("selectedtype"), #for debug
                                                    #textOutput("deliverymode"), #for debug
                                                    
                                                    #if errors triggered
                                                    uiOutput("error1msg"),
                                                    uiOutput("error2msg")
                                                  )))),
                                  column(5,
                                         fluidRow(
                                           column(12,
                                                  div(
                                                    style = "display: inline-block; margin-right: 5px; font-size: 15px; font-weight:bold;",
                                                    "Delivery Mode:"
                                                  ))),
                                         fluidRow(
                                           column(12,
                                                  div(
                                                    style = "display: inline-block; margin-right: 5px; font-size: 13px;",
                                                    radioButtons("deliverymode", "Choose one",
                                                                 choices = c("Van (up to 500 bottles)", "Truck (up to 2000 bottles)"),
                                                                 selected = "")
                                                  )))),
                                  column(4,
                                         fluidRow(
                                           column(12,
                                                  div(
                                                    style = "display: inline-block; margin-right: 5px; font-size: 15px; font-weight:bold;",
                                                    "Delivery Type:"
                                                  ))),
                                         fluidRow(
                                           column(12,
                                                  div(
                                                    style = "display: inline-block; margin-right: 5px; font-size: 13px;",
                                                    radioButtons("deliverytype", "Choose one",
                                                                 choices = c("Standard (3 weeks)", "Express (1 week)"),
                                                                 selected = "")
                                                  )))))#end fluid row
                       )#end box
                     )#end column
              ) #end fluid rows
          )#end fluid page
          ), #end 2nd tab
    
    #Third tab content
    tabItem(tabName = "info",
            h2("Information Page", style = "font-weight:bold;"),
            img(src="info.gif", align = "centre", height='594px',width='1188px') #1920X1080 
    ), #end 3rd tab
    
    #Fourth tab content
    tabItem(tabName = "chart",
            h2("Historical Data", style = "font-weight:bold;"),
            box(
              title = tags$p("History Data Charts", style = "font-weight: bold;"),
              width = 12,
              height = 650,
              div(
                class = "scrollable-box",
                tags$p("Weekly Stock, Demand and Cash Flow Plot", style = "font-size: 16px"),
                plotlyOutput("demand_plot"),  # Placeholder for the demand plot
                plotlyOutput("cashflow_plot"),  # Placeholder for the weekly cash flow plot
                plotlyOutput("cumucash_plot")),   # Placeholder for the cumulative cash flow plot
              style = "background-color: #E29CA4;")
            ), #end 4th tab
  
  #Fifth tab content
  tabItem(tabName = "leaderboard",
          h2("Check your rank here!", style = "font-weight:bold;"),
          box(
            title = tags$p("Leaderboard", style = "font-weight: bold;"),
            style = "background-color: #E29CA4;",
            tableOutput("leaderboard_table")
            )
          ) #end 5th tab
  ), #end tabItems
  
  
  #----- HTML& CSS STYLING -----
  tags$style(".scrollable-box { height: 650px; overflow-y: auto; }"),
  tags$style(".box { background-color: #E29CA4; }"),
  
  tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #24305E; #Welcome corner
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #24305E; #hover on welcome corner
                                }
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #24305E;  #rest of header
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #24305E;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #F76C6C;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #24305E;
                                color: #FFFFFF;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #E29CA4;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #516C8D;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #F7E7C3;    #main page
                                }
                                ')))
  )#end dashboardBody
  )#end dashboardPage

############## SERVER STARTS HERE ###############

server <- function(input, output, session) {
  
  # reactiveValues object for storing items like the user password
  vals <- reactiveValues(newplayername=NULL,
                         password = NULL, 
                         playerid=NULL, 
                         playername=NULL, 
                         weeknumber=0,
                         weeklydemand=NULL,
                         demand_con=NULL,
                         demand_act=NULL,
                         orderquantity=NULL,
                         deliverymode = NULL,
                         selectedmode="",
                         selectedtype="",
                         cashbalance=1000000,
                         totalstock=1000,
                         boutiquestock=500,
                         warehousestock=500,
                         holdingcosts=NULL,
                         holding_costs=NULL,
                         shippingcosts=NULL,
                         shipping_costs=NULL,
                         reordercosts=NULL,
                         reorder_costs=NULL,
                         costs_matrix=NULL,
                         totalcosts=NULL,
                         total_costs=NULL,
                         revenue=0,
                         revenues=NULL,
                         revenue_sum=NULL,
                         stocksupply=NULL,
                         stockintransit=NULL,
                         gamestate=0
                         )
  
  # Initiate Game Start and run seeds to generate data
  observeEvent(input$startbutton, {
    
    # Reject game start if not logged in
    if (is.null(vals$playername)) {
      output$notloggedin <- renderUI({
        showModal(notloggedinModal(failed=TRUE))
      })
    }
    
    #Generate data to start game if logged in
    else {
      
      #Define Game State
      vals$gamestate <- 1
      
      #Generate Normal Demand (W1 to W20)
      #set.seed(42)
      normal_demand1 <- sample(50:200, 20, replace = TRUE)
      
      #Generate Peak Demand (W21 to W28)
      #set.seed(42)
      peak_demand1 <- sample(300:450, 8, replace = TRUE)
      
      #Generate Normal Demand (W29 to W32)
      #set.seed(42)
      normal_demand2 <- sample(50:200, 4, replace = TRUE)
      
      #Generate Peak Demand (W33 to W36)
      #set.seed(42)
      peak_demand2 <- sample(300:450, 4, replace = TRUE)
      
      #Generate Normal Demand (W37 to W53)
      #set.seed(42)
      normal_demand3 <- sample(50:200, 17, replace = TRUE)
      
      #Concat Demand (contains the already generated weekly demand)
      vals$demand_con <- c(normal_demand1, peak_demand1, normal_demand2, peak_demand2, normal_demand3)
      
      #Actual Demand (to take values from Concat Demand every week)
      vals$demand_act <- c(0, rep(NA, 53))
      
      # Initialize Revenue 
      vals$revenues <- c(0, rep(NA, 53))
      
      # Initialize Holding Cost 
      vals$holding_costs <- c(0, rep(NA, 53))
      
      # Initialize Shipping Cost 
      vals$shipping_costs <- c(0, rep(NA, 53))     
      
      # Initialize Reorder Cost 
      vals$reorder_costs <- c(0, rep(NA, 53))
      
      # Initialize Total Cost 
      vals$totalcosts <- c(0, rep(NA, 53))
      
      # Initialize Stock Supply  
      vals$stocksupply <- rep(0,53)
      
      # Initialize Stock In Transit  
      vals$stockintransit <- rep(0,53)
      
      # Initialize Delivery Mode 
      vals$deliverymode <- rep(NA,53)
      
      #Replace Start Button with Next Button
      removeUI(selector = "#startbutton") 
      insertUI(
        selector = "#button_wrapper",  # Insert the new button inside the div with id "button_wrapper"
        ui = actionButton("nextbutton", "NEXT")
      )
    }
  })
  
  # THE GOLDEN "NEXT" BUTTON
  observeEvent(input$nextbutton, {
    
    # Codes below will trigger shipping
    if (is.numeric(vals$orderquantity) && vals$orderquantity > 0 && vals$selectedmode != "" && vals$selectedtype != "") {
      
      # Reset order quantity to 0
      updateTextInput(session, "orderquantity", value = "0")
      
      # Update Week Number
      vals$weeknumber <- vals$weeknumber + 1
      
      # Check End Game Condition
      if (vals$weeknumber < 52) {
        
        # Update Weekly Demand
        vals$weeklydemand <- vals$demand_con[vals$weeknumber]
        
        # Update Weekly Shipping Costs and Delivery Mode Vector
        if (vals$selectedmode == "Van (up to 500 bottles)" && vals$selectedtype == "Standard (3 weeks)") {
          vals$shippingcosts <- 2500 *ceiling(vals$orderquantity / 500) #this is total van standard
          vals$deliverymode[vals$weeknumber + 2] <- "V" #not +3 and +1 because I want delivery animations to appear before the end of week!
        } 
        
        else if (vals$selectedmode == "Van (up to 500 bottles)" && vals$selectedtype == "Express (1 week)") {
          vals$shippingcosts <- 3750 *ceiling(vals$orderquantity / 500) #this is total van express
          vals$deliverymode[vals$weeknumber] <- "V"
        }
        
        else if (vals$selectedmode == "Truck (up to 2000 bottles)" && vals$selectedtype == "Standard (3 weeks)") {
          vals$shippingcosts <- 5000 * ceiling(vals$orderquantity / 2000)#this is total truck standard
          vals$deliverymode[vals$weeknumber + 2] <- "T"
        } 
        
        else {
          vals$shippingcosts <- 7500 *ceiling(vals$orderquantity / 2000)#this is total truck express
          vals$deliverymode[vals$weeknumber] <- "T" 
        }
        
        #Update Weekly Revenue
        if (vals$weeklydemand > vals$boutiquestock) {
          vals$revenue <- vals$boutiquestock * 75
        } 
        else {
          vals$revenue <- 75 * vals$weeklydemand
        }
        
        # Update Weekly Holding Cost
        vals$holdingcosts <- ceiling(vals$warehousestock / 50)*50 # $50 for a box of 50
        
        # Update Weekly Reorder Cost
        vals$reordercosts <- vals$orderquantity *50 # each bottle costs $50
        
        # Update Cash Balance
        vals$cashbalance <- vals$cashbalance +vals$revenue - vals$holdingcosts - vals$shippingcosts - vals$reordercosts
        
        # Update Shipping Stock
        if (vals$selectedtype == "Standard (3 weeks)") {
          vals$stockintransit[vals$weeknumber] <- vals$stockintransit[vals$weeknumber] + vals$orderquantity
          vals$stockintransit[vals$weeknumber + 1] <- vals$stockintransit[vals$weeknumber + 1] + vals$orderquantity
          vals$stockintransit[vals$weeknumber + 2] <- vals$stockintransit[vals$weeknumber + 2] + vals$orderquantity
          vals$stocksupply[vals$weeknumber + 3] <- vals$stocksupply[vals$weeknumber + 3] + vals$orderquantity
        } 
        else if (vals$selectedtype == "Express (1 week)") {
          vals$stockintransit[vals$weeknumber] <- vals$stockintransit[vals$weeknumber] + vals$orderquantity
          vals$stocksupply[vals$weeknumber + 1] <- vals$stocksupply[vals$weeknumber + 1] + vals$orderquantity
        }
        
        # Update Weekly Total Stock
        vals$totalstock <- max(vals$boutiquestock + vals$warehousestock + vals$stocksupply[vals$weeknumber] - vals$weeklydemand, 0)
        
        # Set Game States based on Total Stock
        if (vals$totalstock > 0) {
          # 3 HAPPY STATES 
          if (is.na(vals$deliverymode[vals$weeknumber])) {
            vals$gamestate <- 2
          } 
          else if (vals$deliverymode[vals$weeknumber] == "V") {
            vals$gamestate <- 3
          } 
          else if (vals$deliverymode[vals$weeknumber] == "T") {
            vals$gamestate <- 4
          }
        }
        else {
          # 3 SAD STATES 
          if (is.na(vals$deliverymode[vals$weeknumber])) {
            vals$gamestate <- 5
          } 
          else if (vals$deliverymode[vals$weeknumber] == "V") {
            vals$gamestate <- 6
          } 
          else if (vals$deliverymode[vals$weeknumber] == "T") {
            vals$gamestate <- 7
          }
        }

        # Update Weekly Warehouse Stock
        if (vals$totalstock > 500) {
          vals$warehousestock <- vals$totalstock - 500
        }
        else {
          vals$warehousestock <- 0
        }
        
        # Update Weekly Boutique Stock
        if (vals$totalstock > 500) {
          vals$boutiquestock <- 500
        }
        else if (vals$totalstock > 0 && vals$totalstock <= 500) {
          vals$boutiquestock <- vals$totalstock
        }
        else {
          vals$boutiquestock <- 0
        }
        
        #----- UPDATES FOR PLOTS -----
        
        #Update Demand for plot
        vals$demand_act[vals$weeknumber+1] <- vals$weeklydemand #+1 because there is week 0 in demand_act
        
        #Update Revenue for plot
        vals$revenues[vals$weeknumber+1] <- vals$revenue #+1 because there is week 0 in demand_act
        vals$revenue_sum <- cumsum(vals$revenues)
        
        # Update Holding Cost for plot
        vals$holding_costs[vals$weeknumber+1] <- vals$holdingcosts #+1 because there is week 0 in demand_act
        
        # Update Shipping Costs for plot
        vals$shipping_costs[vals$weeknumber+1] <- vals$shippingcosts #+1 because there is week 0 in demand_act
        
        # Update Reorder Costs for plot
        vals$reorder_costs[vals$weeknumber+1] <- vals$reordercosts #+1 because there is week 0 in demand_act
        
        # Update Total Costs for plot
        #convert all the costs into matrix
        vals$costs_matrix <- ifelse(is.na(vals$holding_costs), 0, vals$holding_costs) + 
          ifelse(is.na(vals$shipping_costs), 0, vals$shipping_costs) + 
          ifelse(is.na(vals$reorder_costs), 0, vals$reorder_costs)
        
        vals$totalcosts[vals$weeknumber+1] <- vals$costs_matrix[vals$weeknumber+1]
        vals$total_costs <- cumsum(vals$totalcosts)
        
        
        #------ PLOT RENDERING -----
        
        # Render the Demand plot
        output$demand_plot <- renderPlotly({
          x <- seq(0,53)  # Example x-axis data
          y <- vals$demand_act  # Example y-axis data
          
          # Create a data frame
          data <- data.frame(x = x, y = y)
          
          # Generate demand plot using ggplot2
          demandplot <- ggplot(data, aes(x = x, y = y)) +
            geom_line() +
            labs(x = "Week", y = "Demand", title = "Demand Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5))
        })
        
        # Render the Weekly Cash Flow Plot
        output$cashflow_plot <- renderPlotly({
          x <- seq(0,53)  # Week
          y1 <- vals$revenues  # weekly revenue
          y2 <- vals$holding_costs # weekly holding costs
          y3 <- vals$reorder_costs # weekly inventory purchase costs
          y4 <- vals$shipping_costs # weekly shipping costs
          
          # Create a data frame
          data <- data.frame(x = rep(x, 4), y = c(y1, y2, y3, y4), Legend = rep(c("Revenues", "Holding Costs", "Reorder Costs", "Shipping Costs"), each = length(x)))
          
          # Generate cash flow plot using ggplot2
          cashflowplot <- ggplot(data, aes(x = x, y = y, group = Legend)) +
            geom_line(aes(color = Legend)) +
            labs(x = "Week", y = "Revenue/Costs (SGD)", title = "Weekly Cash Flow Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5))
        })
        
        # Render the Cumulative Cash Flow Plot
        output$cumucash_plot <- renderPlotly({
          x <- seq(0, 53) #Week
          y1 <- vals$revenue_sum # Cumu Revenue
          y2 <- vals$total_costs # Cumu Costs
          
          # Create a data frame
          data <- data.frame(x = rep(x, 2), y = c(y1, y2), Legend = rep(c("Total Revenue", "Total Costs"), each = length(x)))
          
          # Generate cumulative cash flow plot using ggplot2
          cumucashplot <- ggplot(data, aes(x = x, y = y, group = Legend)) +
            geom_line(aes(color=Legend)) + 
            labs(x = "Week", y = "Revenue/Costs (SGD)", title = " Cumulative Cash Flow Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5)) 
        })
      }
      
      else if (vals$weeknumber == 52) {
        
        # Remove option to order more on penultimate week
        removeUI(selector = "#orderquantity") 
        removeUI(selector = "#deliverytype") 
        removeUI(selector = "#deliverymode") 
        
        # Update Weekly Demand
        vals$weeklydemand <- vals$demand_con[vals$weeknumber]
        
        #Update Weekly Revenue
        if (vals$weeklydemand > vals$boutiquestock) {
          vals$revenue <- vals$boutiquestock * 75
        } 
        else {
          vals$revenue <- 75 * vals$weeklydemand
        }
        
        # Update Weekly Holding Cost
        vals$holdingcosts <- ceiling(vals$warehousestock / 50)*50 # $50 for a box of 50
        
        # Update Weekly Reorder Cost
        vals$reordercosts <- vals$orderquantity *50 # each bottle costs $50
        
        # Update Cash Balance
        vals$cashbalance <- vals$cashbalance +vals$revenue - vals$holdingcosts - vals$shippingcosts - vals$reordercosts
        
        
        # Update Weekly Total Stock
        vals$totalstock <- max(vals$boutiquestock + vals$warehousestock + vals$stocksupply[vals$weeknumber] - vals$weeklydemand, 0)
        
        # Set Game States based on Total Stock
        if (vals$totalstock > 0) {
          # 3 HAPPY STATES 
          if (is.na(vals$deliverymode[vals$weeknumber])) {
            vals$gamestate <- 2
          } 
          else if (vals$deliverymode[vals$weeknumber] == "V") {
            vals$gamestate <- 3
          } 
          else if (vals$deliverymode[vals$weeknumber] == "T") {
            vals$gamestate <- 4
          }
        }
        else {
          # 3 SAD STATES 
          if (is.na(vals$deliverymode[vals$weeknumber])) {
            vals$gamestate <- 5
          } 
          else if (vals$deliverymode[vals$weeknumber] == "V") {
            vals$gamestate <- 6
          } 
          else if (vals$deliverymode[vals$weeknumber] == "T") {
            vals$gamestate <- 7
          }
        }
        
        # Update Weekly Warehouse Stock
        if (vals$totalstock > 500) {
          vals$warehousestock <- vals$totalstock - 500
        }
        else {
          vals$warehousestock <- 0
        }
        
        # Update Weekly Boutique Stock
        if (vals$totalstock > 500) {
          vals$boutiquestock <- 500
        }
        else if (vals$totalstock > 0 && vals$totalstock <= 500) {
          vals$boutiquestock <- vals$totalstock
        }
        else {
          vals$boutiquestock <- 0
        }
        
        #----- UPDATES FOR PLOTS -----
        
        #Update Demand for plot
        vals$demand_act[vals$weeknumber+1] <- vals$weeklydemand #+1 because there is week 0 in demand_act
        
        #Update Revenue for plot
        vals$revenues[vals$weeknumber+1] <- vals$revenue #+1 because there is week 0 in demand_act
        vals$revenue_sum <- cumsum(vals$revenues)
        
        # Update Holding Cost for plot
        vals$holding_costs[vals$weeknumber+1] <- vals$holdingcosts #+1 because there is week 0 in demand_act
        
        # Update Shipping Costs for plot
        vals$shipping_costs[vals$weeknumber+1] <- vals$shippingcosts #+1 because there is week 0 in demand_act
        
        # Update Reorder Costs for plot
        vals$reorder_costs[vals$weeknumber+1] <- vals$reordercosts #+1 because there is week 0 in demand_act
        
        # Update Total Costs for plot
        #convert all the costs into matrix
        vals$costs_matrix <- ifelse(is.na(vals$holding_costs), 0, vals$holding_costs) + 
          ifelse(is.na(vals$shipping_costs), 0, vals$shipping_costs) + 
          ifelse(is.na(vals$reorder_costs), 0, vals$reorder_costs)
        
        vals$totalcosts[vals$weeknumber+1] <- vals$costs_matrix[vals$weeknumber+1]
        vals$total_costs <- cumsum(vals$totalcosts)
        
        #------ PLOT RENDERING -----
        
        # Render the Demand plot
        output$demand_plot <- renderPlotly({
          x <- seq(0,53)  # Example x-axis data
          y <- vals$demand_act  # Example y-axis data
          
          # Create a data frame
          data <- data.frame(x = x, y = y)
          
          # Generate demand plot using ggplot2
          demandplot <- ggplot(data, aes(x = x, y = y)) +
            geom_line() +
            labs(x = "Week", y = "Demand", title = "Demand Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5))
        })
        
        # Render the Weekly Cash Flow Plot
        output$cashflow_plot <- renderPlotly({
          x <- seq(0,53)  # Week
          y1 <- vals$revenues  # weekly revenue
          y2 <- vals$holding_costs # weekly holding costs
          y3 <- vals$reorder_costs # weekly inventory purchase costs
          y4 <- vals$shipping_costs # weekly shipping costs
          
          # Create a data frame
          data <- data.frame(x = rep(x, 4), y = c(y1, y2, y3, y4), Legend = rep(c("Revenues", "Holding Costs", "Reorder Costs", "Shipping Costs"), each = length(x)))
          
          # Generate cash flow plot using ggplot2
          cashflowplot <- ggplot(data, aes(x = x, y = y, group = Legend)) +
            geom_line(aes(color = Legend)) +
            labs(x = "Week", y = "Revenue/Costs (SGD)", title = "Weekly Cash Flow Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5))
        })
        
        # Render the Cumulative Cash Flow Plot
        output$cumucash_plot <- renderPlotly({
          x <- seq(0, 53) #Week
          y1 <- vals$revenue_sum # Cumu Revenue
          y2 <- vals$total_costs # Cumu Costs
          
          # Create a data frame
          data <- data.frame(x = rep(x, 2), y = c(y1, y2), Legend = rep(c("Total Revenue", "Total Costs"), each = length(x)))
          
          # Generate cumulative cash flow plot using ggplot2
          cumucashplot <- ggplot(data, aes(x = x, y = y, group = Legend)) +
            geom_line(aes(color=Legend)) + 
            labs(x = "Week", y = "Revenue/Costs (SGD)", title = " Cumulative Cash Flow Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5)) 
        })
      }
      
      # End of Game
      else {
        
        # Get the current player's name, cash balance, and time
        playername <- vals$playername
        cashbalance <- vals$cashbalance
        time <- Sys.time()  # Get the current time
        
        # Insert the game result data into the Leaderboard table
        insertLeaderboardData(playername, cashbalance, time)
        
        #Replace Next Button with Finish Button
        removeUI(selector = "#nextbutton") 
        insertUI(
          selector = "#button_wrapper",  # Insert the new button inside the div with id "button_wrapper"
          ui = actionButton("finishbutton", "FINISH")
        )
      }
    }
    
    # Codes below will NOT trigger shipping
    else if (is.numeric(vals$orderquantity) && vals$orderquantity == 0) {
      
      # Reset order quantity to 0
      updateTextInput(session, "orderquantity", value = "0")
      
      # Reset Shipping and Reorder Costs to 0
      vals$shippingcosts <- 0
      vals$reordercosts <- 0
      
      # Update Week Number
      vals$weeknumber <- vals$weeknumber + 1
      
      # Check End Game Condition
      if (vals$weeknumber < 52) {
      
        # Update Weekly Demand
        vals$weeklydemand <- vals$demand_con[vals$weeknumber]
        
        #Update Weekly Revenue
        if (vals$weeklydemand > vals$boutiquestock) {
          vals$revenue <- vals$boutiquestock * 75
        } 
        else {
          vals$revenue <- 75 * vals$weeklydemand
        }
        
        # Update Weekly Holding Cost
        vals$holdingcosts <- ceiling(vals$warehousestock / 50)*50 # $50 for a box of 50
        
        # Update Cash Balance
        vals$cashbalance <- vals$cashbalance + vals$revenue - vals$holdingcosts
        
        # Update Weekly Total stock
        vals$totalstock <- max(vals$boutiquestock + vals$warehousestock + vals$stocksupply[vals$weeknumber] - vals$weeklydemand, 0)
        
        # Set Game States based on Total Stock
        if (vals$totalstock > 0) {
          # 3 HAPPY STATES 
          if (is.na(vals$deliverymode[vals$weeknumber])) {
            vals$gamestate <- 2
          } 
          else if (vals$deliverymode[vals$weeknumber] == "V") {
            vals$gamestate <- 3
          } 
          else if (vals$deliverymode[vals$weeknumber] == "T") {
            vals$gamestate <- 4
          }
        }
        else {
          # 3 SAD STATES 
          if (is.na(vals$deliverymode[vals$weeknumber])) {
            vals$gamestate <- 5
          } 
          else if (vals$deliverymode[vals$weeknumber] == "V") {
            vals$gamestate <- 6
          } 
          else if (vals$deliverymode[vals$weeknumber] == "T") {
            vals$gamestate <- 7
          }
        }
        
        # Update Weekly Warehouse stock
        if (vals$totalstock > 500) {
          vals$warehousestock <- vals$totalstock - 500
        }
        else {
          vals$warehousestock <- 0
        }
        
        # Update Weekly Boutique stock
        if (vals$totalstock > 500) {
          vals$boutiquestock <- 500
        }
        else if (vals$totalstock > 0 && vals$totalstock <= 500) {
          vals$boutiquestock <- vals$totalstock
        }
        else {
          vals$boutiquestock <- 0
        }
        
        #----- UPDATES FOR PLOTS -----
        
        #Update Demand for plot
        vals$demand_act[vals$weeknumber+1] <- vals$weeklydemand #+1 because there is week 0 in demand_act
        
        #Update Revenue for plot
        vals$revenues[vals$weeknumber+1] <- vals$revenue #+1 because there is week 0 in demand_act
        vals$revenue_sum <- cumsum(vals$revenues)
        
        # Update Holding Cost for plot
        vals$holding_costs[vals$weeknumber+1] <- vals$holdingcosts #+1 because there is week 0 in demand_act
        
        # Update Shipping Costs for plot
        vals$shipping_costs[vals$weeknumber+1] <- vals$shippingcosts #+1 because there is week 0 in demand_act
        
        # Update Reorder Costs for plot
        vals$reorder_costs[vals$weeknumber+1] <- vals$reordercosts #+1 because there is week 0 in demand_act
        
        # Update Total Costs for plot
        # Convert all the costs into matrix
        vals$costs_matrix <- ifelse(is.na(vals$holding_costs), 0, vals$holding_costs) + 
          ifelse(is.na(vals$shipping_costs), 0, vals$shipping_costs) + 
          ifelse(is.na(vals$reorder_costs), 0, vals$reorder_costs)
        
        vals$totalcosts[vals$weeknumber+1] <- vals$costs_matrix[vals$weeknumber+1]
        vals$total_costs <- cumsum(vals$totalcosts)
        
        #------ PLOT RENDERING -----
        
        # Render the Demand Plot
        output$demand_plot <- renderPlotly({
          x <- seq(0,53)  # Example x-axis data
          y <- vals$demand_act  # Example y-axis data
          
          # Create a data frame
          data <- data.frame(x = x, y = y)
          
          # Generate demand plot using ggplot2
          demandplot <- ggplot(data, aes(x = x, y = y)) +
            geom_line() +
            labs(x = "Week", y = "Demand", title = "Demand Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5))
        })
        
        # Render the Weekly Cash Flow Plot
        output$cashflow_plot <- renderPlotly({
          x <- seq(0,53)  # Week
          y1 <- vals$revenues  # weekly revenue
          y2 <- vals$holding_costs # weekly holding costs
          y3 <- vals$reorder_costs # weekly inventory purchase costs
          y4 <- vals$shipping_costs # weekly shipping costs
          
          # Create a data frame
          data <- data.frame(x = rep(x, 4), y = c(y1, y2, y3, y4), Legend = rep(c("Revenues", "Holding Costs", "Reorder Costs", "Shipping Costs"), each = length(x)))
          
          # Generate cash flow plot using ggplot2
          cashflowplot <- ggplot(data, aes(x = x, y = y, group = Legend)) +
            geom_line(aes(color = Legend)) +
            labs(x = "Week", y = "Revenue/Costs (SGD)", title = "Weekly Cash Flow Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5))
        })
        
        # Render the Cumulative Cash Flow Plot
        output$cumucash_plot <- renderPlotly({
          x <- seq(0, 53) #Week
          y1 <- vals$revenue_sum # Cumu Revenue
          y2 <- vals$total_costs # Cumu Costs
          
          # Create a data frame
          data <- data.frame(x = rep(x, 2), y = c(y1, y2), Legend = rep(c("Total Revenue", "Total Costs"), each = length(x)))
          
          # Generate cumulative cash flow plot using ggplot2
          cumucashplot <- ggplot(data, aes(x = x, y = y, group = Legend)) +
            geom_line(aes(color=Legend)) + 
            labs(x = "Week", y = "Revenue/Costs (SGD)", title = " Cumulative Cash Flow Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5)) 
        })
      }
      
      else if (vals$weeknumber == 52) {
        
        # Remove option to order more on penultimate week
        removeUI(selector = "#orderquantity") 
        removeUI(selector = "#deliverytype") 
        removeUI(selector = "#deliverymode") 
        
        # Update Weekly Demand
        vals$weeklydemand <- vals$demand_con[vals$weeknumber]
        
        #Update Weekly Revenue
        if (vals$weeklydemand > vals$boutiquestock) {
          vals$revenue <- vals$boutiquestock * 75
        } 
        else {
          vals$revenue <- 75 * vals$weeklydemand
        }
        
        # Update Weekly Holding Cost
        vals$holdingcosts <- ceiling(vals$warehousestock / 50)*50 # $50 for a box of 50
        
        # Update Weekly Reorder Cost
        vals$reordercosts <- vals$orderquantity *50 # each bottle costs $50
        
        # Update Cash Balance
        vals$cashbalance <- vals$cashbalance +vals$revenue - vals$holdingcosts - vals$shippingcosts - vals$reordercosts
        
        
        # Update Weekly Total Stock
        vals$totalstock <- max(vals$boutiquestock + vals$warehousestock + vals$stocksupply[vals$weeknumber] - vals$weeklydemand, 0)
        
        # Set Game States based on Total Stock
        if (vals$totalstock > 0) {
          # 3 HAPPY STATES 
          if (is.na(vals$deliverymode[vals$weeknumber])) {
            vals$gamestate <- 2
          } 
          else if (vals$deliverymode[vals$weeknumber] == "V") {
            vals$gamestate <- 3
          } 
          else if (vals$deliverymode[vals$weeknumber] == "T") {
            vals$gamestate <- 4
          }
        }
        else {
          # 3 SAD STATES 
          if (is.na(vals$deliverymode[vals$weeknumber])) {
            vals$gamestate <- 5
          } 
          else if (vals$deliverymode[vals$weeknumber] == "V") {
            vals$gamestate <- 6
          } 
          else if (vals$deliverymode[vals$weeknumber] == "T") {
            vals$gamestate <- 7
          }
        }
        
        # Update Weekly Warehouse Stock
        if (vals$totalstock > 500) {
          vals$warehousestock <- vals$totalstock - 500
        }
        else {
          vals$warehousestock <- 0
        }
        
        # Update Weekly Boutique Stock
        if (vals$totalstock > 500) {
          vals$boutiquestock <- 500
        }
        else if (vals$totalstock > 0 && vals$totalstock <= 500) {
          vals$boutiquestock <- vals$totalstock
        }
        else {
          vals$boutiquestock <- 0
        }
        
        #----- UPDATES FOR PLOTS -----
        
        #Update Demand for plot
        vals$demand_act[vals$weeknumber+1] <- vals$weeklydemand #+1 because there is week 0 in demand_act
        
        #Update Revenue for plot
        vals$revenues[vals$weeknumber+1] <- vals$revenue #+1 because there is week 0 in demand_act
        vals$revenue_sum <- cumsum(vals$revenues)
        
        # Update Holding Cost for plot
        vals$holding_costs[vals$weeknumber+1] <- vals$holdingcosts #+1 because there is week 0 in demand_act
        
        # Update Shipping Costs for plot
        vals$shipping_costs[vals$weeknumber+1] <- vals$shippingcosts #+1 because there is week 0 in demand_act
        
        # Update Reorder Costs for plot
        vals$reorder_costs[vals$weeknumber+1] <- vals$reordercosts #+1 because there is week 0 in demand_act
        
        # Update Total Costs for plot
        #convert all the costs into matrix
        vals$costs_matrix <- ifelse(is.na(vals$holding_costs), 0, vals$holding_costs) + 
          ifelse(is.na(vals$shipping_costs), 0, vals$shipping_costs) + 
          ifelse(is.na(vals$reorder_costs), 0, vals$reorder_costs)
        
        vals$totalcosts[vals$weeknumber+1] <- vals$costs_matrix[vals$weeknumber+1]
        vals$total_costs <- cumsum(vals$totalcosts)
        
        #------ PLOT RENDERING -----
        
        # Render the Demand plot
        output$demand_plot <- renderPlotly({
          x <- seq(0,53)  # Example x-axis data
          y <- vals$demand_act  # Example y-axis data
          
          # Create a data frame
          data <- data.frame(x = x, y = y)
          
          # Generate demand plot using ggplot2
          demandplot <- ggplot(data, aes(x = x, y = y)) +
            geom_line() +
            labs(x = "Week", y = "Demand", title = "Demand Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5))
        })
        
        # Render the Weekly Cash Flow Plot
        output$cashflow_plot <- renderPlotly({
          x <- seq(0,53)  # Week
          y1 <- vals$revenues  # weekly revenue
          y2 <- vals$holding_costs # weekly holding costs
          y3 <- vals$reorder_costs # weekly inventory purchase costs
          y4 <- vals$shipping_costs # weekly shipping costs
          
          # Create a data frame
          data <- data.frame(x = rep(x, 4), y = c(y1, y2, y3, y4), Legend = rep(c("Revenues", "Holding Costs", "Reorder Costs", "Shipping Costs"), each = length(x)))
          
          # Generate cash flow plot using ggplot2
          cashflowplot <- ggplot(data, aes(x = x, y = y, group = Legend)) +
            geom_line(aes(color = Legend)) +
            labs(x = "Week", y = "Revenue/Costs (SGD)", title = "Weekly Cash Flow Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5))
        })
        
        # Render the Cumulative Cash Flow Plot
        output$cumucash_plot <- renderPlotly({
          x <- seq(0, 53) #Week
          y1 <- vals$revenue_sum # Cumu Revenue
          y2 <- vals$total_costs # Cumu Costs
          
          # Create a data frame
          data <- data.frame(x = rep(x, 2), y = c(y1, y2), Legend = rep(c("Total Revenue", "Total Costs"), each = length(x)))
          
          # Generate cumulative cash flow plot using ggplot2
          cumucashplot <- ggplot(data, aes(x = x, y = y, group = Legend)) +
            geom_line(aes(color=Legend)) + 
            labs(x = "Week", y = "Revenue/Costs (SGD)", title = " Cumulative Cash Flow Plot") +
            theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5)) +
            scale_x_continuous(limits = c(0,53), breaks = seq(0, 53, by = 5)) 
        })
      }
      
      # End of Game
      else {
        
        # Get the current player's name, cash balance, and time
        playername <- vals$playername
        cashbalance <- vals$cashbalance
        time <- Sys.time()  # Get the current time
        
        # Insert the game result data into the Leaderboard table
        insertLeaderboardData(playername, cashbalance, time)
        
        #Replace Next Button with Finish Button
        removeUI(selector = "#nextbutton") 
        insertUI(
          selector = "#button_wrapper",  # Insert the new button inside the div with id "button_wrapper"
          ui = actionButton("finishbutton", "FINISH")
        )
      }
    }
    
    else if (is.numeric(vals$orderquantity) && vals$orderquantity > 0) {
      output$error1msg <- renderUI({
        showModal(error1Modal(failed=TRUE))
      })
    }
    
    else {
      output$error2msg <- renderUI({
        showModal(error2Modal(failed=TRUE))
      })
    }
    
  })
  
  ########### IF-ELSE IN GAME CONDITIONS ENDS HERE ########
  
  # Observe Game State
  observeEvent(vals$gamestate, {
    if (vals$gamestate == 0){
      output$gif <- renderImage({
        list(src ="www/Background1.png", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
    
    else if (vals$gamestate == 1){
      output$gif <- renderImage({
        list(src ="www/Background2.png", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
    
    else if (vals$gamestate == 2){
      output$gif <- renderImage({
        list(src ="www/Happy.gif", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
    else if (vals$gamestate == 3){
      output$gif <- renderImage({
        list(src ="www/Happy Van.gif", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
    else if (vals$gamestate == 4){
      output$gif <- renderImage({
        list(src ="www/Happy Truck.gif", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
    else if (vals$gamestate == 5){
      output$gif <- renderImage({
        list(src ="www/Sad.gif", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
    else if (vals$gamestate == 6){
      output$gif <- renderImage({
        list(src ="www/Sad Van.gif", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
    else if (vals$gamestate == 7){
      output$gif <- renderImage({
        list(src ="www/Sad Truck.gif", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
    else if (vals$gamestate == 8){
      output$gif <- renderImage({
        list(src ="www/Boom.gif", height = "600px", width = "1066px", alt = "alternate text")
      }, deleteFile = FALSE)
    }
  })
  
  # Observe the changes in the Radio Button Choices
  observeEvent(input$deliverymode, {
    vals$selectedmode <- input$deliverymode
  })
  
  observeEvent(input$deliverytype, {
    vals$selectedtype <- input$deliverytype
  })
  
  # Observe the input for Order Quantity
  observeEvent(input$orderquantity, {
    
    # Check if the input contains any non-numeric characters
    if (grepl("[^0-9]", input$orderquantity)) {
      # If it contains non-numeric characters, classify it as "chr" (character)
      vals$orderquantity <- as.character(input$orderquantity)
    } 
    else if (is.na(input$orderquantity) || is.null(input$orderquantity) || (input$orderquantity == "")){
      
      # If it is empty, set default value 0
      vals$orderquantity <- 0
    }
    else {
      # If it's a full numeric input, classify it as "int" (integer)
      vals$orderquantity <- as.integer(input$orderquantity)
    }
  })
  
  # Observe End Game Finish Button input
  observeEvent(input$finishbutton, {
    
    # Show Leaderboard by clicking Finish Button
    output$leaderboard_table <- renderTable({
      df_leaderboard <- getLeaderBoard()
    }, rownames = TRUE)
    
    showModal(finishModal(finish=TRUE))
    vals$gamestate <- 8
  })
  
  #-------- FOR OUTPUTS ---------
  
  # Show the week number
  output$weeknumber <- renderText({
    as.character(vals$weeknumber)
  })
  # Show weekly demand
  output$weeklydemand <- renderText({
    as.character(vals$weeklydemand)
  })
  # Show revenue
  output$revenue <- renderText({
    sprintf("$%.2f", vals$revenue)
  })
  # Show cash balance
  output$cashbalance <- renderText({
    sprintf("$%.2f", vals$cashbalance)
  })
  # Show the warehouse stock
  output$warehousestock <- renderText({
    as.character(vals$warehousestock)
  })
  # Show the boutique stock
  output$boutiquestock <- renderText({
    as.character(vals$boutiquestock)
  })
  # Show stock in transit
  output$stockintransit <- renderText({
    as.character(vals$stockintransit[vals$weeknumber])
  })
  # Show week of supply
  output$weekofsupply <- renderText({
    as.character(vals$weekofsupply)
  })
  
  # Show leaderboard
  output$leaderboard_table <- renderTable({
    df_leaderboard <- getLeaderBoard()
  }, rownames = TRUE)
  
  #------- FOR DEBUG -------
  output$deliverymode <- renderText({
    vals$deliverymode[vals$weeknumber]
  })
  
  output$orderquantity <- renderText({
    vals$orderquantity
  })
  
  output$selectedmode <- renderText({
    vals$selectedmode
  })
  output$selectedtype <- renderText({
    vals$selectedtype
  })
  output$shippingcosts <- renderText({
    vals$shippingcosts
  })
  
  #------------------ LOGIN STUFFS ------------------
  # Fire some code if the user clicks the Register button
  observeEvent(input$register, {
    showModal(passwordModal(failed=FALSE))
  })
  
  # Fire some code to save new username
  observeEvent(input$newplayername, {
    vals$newplayername <- input$newplayername
  })
  
  # Fire some code if the user clicks the passwordok button
  observeEvent(input$passwordok, {
    # Check that password1 exists and it matches password2
    if (str_length(input$password1) >0 && (input$password1 == input$password2)) {
      #store the password and close the dialog
      vals$password <- input$password1
      print(vals$password) # for debugging
      
      vals$playername <- registerPlayer(vals, vals$password)
      
      if (!is.null(vals$playername)){
        vals$playerid <- getPlayerID(vals$playername,vals$password)
      }
      
      print(vals$playerid) # for debugging
      removeModal()
    } 
    else {
      showModal(passwordModal(failed = TRUE)) 
    }
  })
  
  #Fire some code if the user clicks the Login button
  observeEvent(input$login, {
    showModal(loginModal(failed=FALSE))
  })
  
  # Fire some code if the user clicks the loginok button
  observeEvent(input$loginok, {
    # Get the playerID and check if it is valid
    playerid <- getPlayerID(input$playername,input$password3)
    if (playerid>0) {
      #store the playerid and playername and close the dialog
      vals$playerid <- playerid
      #print(vals$playerid) # for debugging
      vals$playername <- input$playername
      #print(vals$playername) # for debugging
      removeModal()
    } else {
      showModal(loginModal(failed = TRUE))
    }
  })
  
  # React to successful login
  output$loggedInAs <- renderUI({
    if (is.null(vals$playername))
      "Not logged in yet."
    else
      vals$playername
  })

}

shinyApp(ui, server)