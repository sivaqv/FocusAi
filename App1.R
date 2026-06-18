# ==============================================================================
# INTELLIGENT STUDENT DISTRACTION ANALYSIS & ADAPTIVE FOCUS ENHANCEMENT SYSTEM
# Production Build v6.1 - Advanced Strict-Validation Authentication Engine
# ==============================================================================

if (!requireNamespace('shiny', quietly = TRUE)) install.packages('shiny', repos = 'https://cloud.r-project.org')
if (!requireNamespace('shinydashboard', quietly = TRUE)) install.packages('shinydashboard', repos = 'https://cloud.r-project.org')
if (!requireNamespace('shinyjs', quietly = TRUE)) install.packages('shinyjs', repos = 'https://cloud.r-project.org')
if (!requireNamespace('plotly', quietly = TRUE)) install.packages('plotly', repos = 'https://cloud.r-project.org')
if (!requireNamespace('ggplot2', quietly = TRUE)) install.packages('ggplot2', repos = 'https://cloud.r-project.org')
if (!requireNamespace('dplyr', quietly = TRUE)) install.packages('dplyr', repos = 'https://cloud.r-project.org')
if (!requireNamespace('DT', quietly = TRUE)) install.packages('DT', repos = 'https://cloud.r-project.org')

library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)

# ==============================================================================
# USER INTERFACE (UI)
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "FocusAI Platform v6.1"),
  
  dashboardSidebar(
    uiOutput("sidebarMenuUI")
  ),
  
  dashboardBody(
    useShinyjs(), 
    
    tags$head(
      tags$style(HTML("
        /* Custom Styling for the Authentication Gate Workspace */
        .login-box { max-width: 460px; margin: 40px auto; padding: 25px; background: #ffffff; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.1); border-top: 4px solid #3c8dbc; }
        .login-title { font-weight: bold; color: #1e3a8a; text-align: center; margin-bottom: 20px; }
        .role-selector { margin-bottom: 15px; padding: 10px; background-color: #f8fafc; border-radius: 6px; border: 1px solid #e2e8f0; }
        .toggle-link { display: block; text-align: center; margin-top: 15px; cursor: pointer; color: #3c8dbc; font-weight: bold; text-decoration: underline; }
        .about-title { font-weight: bold; color: #1e3a8a; margin-top: 25px; border-bottom: 2px solid #e2e8f0; padding-bottom: 5px; }
        .about-sub-item { margin-bottom: 12px; font-size: 14px; line-height: 1.6; list-style-type: none; padding-left: 0; }
        .tab-tag { background-color: #3b82f6; color: white; padding: 2px 8px; border-radius: 4px; font-family: monospace; font-size: 12px; font-weight: bold; margin-right: 5px; }
        .access-denied { text-align: center; padding: 50px 20px; color: #dc2626; }
      ")),
      tags$script(HTML("
        // 1. Telemetry Capture: Detect when the student switches windows or tabs
        document.addEventListener('visibilitychange', function() {
          if (document.hidden) {
            Shiny.setInputValue('tab_switched', Math.random());
          }
        });
        
        // 2. Main Dashboard Fullscreen Activator
        function launchAppFullscreen() {
          var element = document.documentElement;
          if (element.requestFullscreen) {
            element.requestFullscreen();
          } else if (element.webkitRequestFullscreen) {
            element.webkitRequestFullscreen();
          } else if (element.msRequestFullscreen) {
            element.msRequestFullscreen();
          }
        }
        
        // 3. Isolated Resource Spawning Protocol
        function deploySecureWorkspace(targetUrl) {
          if (targetUrl.includes('youtube.com/watch?v=')) {
            targetUrl = targetUrl.replace('watch?v=', 'embed/');
          } else if (targetUrl.includes('youtu.be/')) {
            targetUrl = targetUrl.replace('youtu.be/', 'youtube.com/embed/');
          }
          
          var w = window.screen.availWidth;
          var h = window.screen.availHeight;
          
          var windowFeatures = 'popup=1,toolbar=no,menubar=no,location=no,status=no,directories=no,titlebar=no,width=' + w + ',height=' + h + ',top=0,left=0';
          
          window.open(targetUrl, 'StudyWorkspaceWindow', windowFeatures);
          launchAppFullscreen();
        }
      "))
    ),
    
    uiOutput("mainBodyUI")
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # Reactive State Trackers
  authSession <- reactiveValues(
    authenticated = FALSE,
    currentRole = NULL,    
    activeUser = "",
    viewMode = "LOGIN" # Valid options: LOGIN or SIGNUP
  )
  
  # Persistent Memory Ledger
  userDirectory <- reactiveValues(
    credentials = data.frame(
      User_Name = c("Alex Mercer"),
      User_Password = c("Alex123!"),
      stringsAsFactors = FALSE
    ),
    ledger = data.frame(
      Registration_Timestamp = c("2026-06-15 09:14:22"),
      Full_Name = c("Alex Mercer"),
      Email_Address = c("alex.mercer1@university.com"),
      Phone_Number = c("9876543210"),
      Academic_Year = c("Third Year / Junior"),
      stringsAsFactors = FALSE
    )
  )
  
  focusData <- reactiveValues(
    distractions = 0,
    activeUrl = "",
    systemState = "Awaiting Session Initialization"
  )
  
  # Toggle Sign In / Sign Up Card Form Panels
  observeEvent(input$go_signup, { authSession$viewMode <- "SIGNUP" })
  observeEvent(input$go_login, { authSession$viewMode <- "LOGIN" })
  
  # --- Authentication / Entry Processing Engine ---
  observeEvent(input$login_btn, {
    if (input$login_role == "ADMIN") {
      if (input$admin_password == "admin123") {
        authSession$authenticated <- TRUE
        authSession$currentRole <- "ADMIN"
        authSession$activeUser <- "System Administrator"
        showNotification("Administrative dashboard verified. Access granted.", type = "message")
      } else {
        showNotification("Verification Failure: Invalid Administrative Key.", type = "error")
      }
    } else {
      # Standard User Password Matching Pipeline
      matched_user <- userDirectory$credentials %>% 
        filter(User_Name == input$login_name & User_Password == input$login_password)
      
      if (nrow(matched_user) > 0) {
        authSession$authenticated <- TRUE
        authSession$currentRole <- "STUDENT"
        authSession$activeUser <- input$login_name
        showNotification(paste("Access Granted. Session initialized for:", input$login_name), type = "message")
      } else {
        showNotification("Access Intercepted: Username or password combination invalid.", type = "error")
      }
    }
  })
  
  # --- Signup Data Constraint Validation Engine ---
  observeEvent(input$signup_btn, {
    
    # Validation Rule 1: Full Name must be strictly greater than 5 characters
    if (nchar(trimws(input$student_name)) <= 5) {
      showNotification("Registration Rejected: Full Name string length must be greater than 5 characters.", type = "error")
      return()
    }
    
    # Validation Rule 2: Email must contain mix of letters & numbers and end explicitly with .com
    email_pattern <- "^(?=.*[A-Za-z])(?=.*[0-9])[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[c][o][m]$"
    if (!grepl(email_pattern, input$student_email, perl = TRUE)) {
      showNotification("Registration Rejected: Email requires letters + numbers and must end with '.com'.", type = "error")
      return()
    }
    
    # Validation Rule 3: Phone number must start with 6,7,8,9 and possess exactly 10 digits
    phone_pattern <- "^[6-9][0-9]{9}$"
    if (!grepl(phone_pattern, input$student_phone)) {
      showNotification("Registration Rejected: Contact number must be 10 digits and start with 6, 7, 8, or 9.", type = "error")
      return()
    }
    
    # Validation Rule 4: Password requires a mix of letters, numbers, and additional special characters
    password_pattern <- "^(?=.*[A-Za-z])(?=.*[0-9])(?=.*[^A-Za-z0-9]).+$"
    if (!grepl(password_pattern, input$student_pass1, perl = TRUE)) {
      showNotification("Registration Rejected: Password must mix alphabetical letters, numbers, and special symbols.", type = "error")
      return()
    }
    
    # Check for Duplicate Account Identity Conflict
    if (input$student_name %in% userDirectory$credentials$User_Name) {
      showNotification("Registration Rejected: Username is already registered on our core file server ledger.", type = "error")
      return()
    }
    
    # Commit Secure Profile Strings to Active Matrix Database Tables
    new_cred_row <- data.frame(User_Name = input$student_name, User_Password = input$student_pass1, stringsAsFactors = FALSE)
    userDirectory$credentials <- rbind(userDirectory$credentials, new_cred_row)
    
    new_profile_row <- data.frame(
      Registration_Timestamp = as.character(Sys.time()),
      Full_Name = input$student_name,
      Email_Address = input$student_email,
      Phone_Number = input$student_phone,
      Academic_Year = switch(input$student_year, "Y1" = "First Year / Freshman", "Y2" = "Second Year / Sophomore", "Y3" = "Third Year / Junior", "Y4" = "Fourth Year / Senior"),
      stringsAsFactors = FALSE
    )
    userDirectory$ledger <- rbind(userDirectory$ledger, new_profile_row)
    
    showNotification("Compilation Success! Profile generated. Proceed to login panel.", type = "message")
    authSession$viewMode <- "LOGIN"
  })
  
  # --- Logout Clear Event Hook ---
  observeEvent(input$logout_btn, {
    authSession$authenticated <- FALSE
    authSession$currentRole <- NULL
    authSession$activeUser <- ""
    authSession$viewMode <- "LOGIN"
    showNotification("Security Protocol Alert: Session closed.", type = "warning")
  })
  
  # Dynamic Sidebar Menu Routing
  output$sidebarMenuUI <- renderUI({
    if (!authSession$authenticated) {
      sidebarMenu(menuItem("Security Gate", tabName = "lockgate", icon = icon("shield-halved")))
    } else if (authSession$currentRole == "ADMIN") {
      sidebarMenu(
        menuItem("Admin Console", tabName = "admin_dashboard", icon = icon("toolbox")),
        menuItem("Log Out System", tabName = "logout_trigger", icon = icon("power-off"))
      )
    } else {
      sidebarMenu(
        menuItem("Home Portal", tabName = "home", icon = icon("house")),
        menuItem("Dashboard Metrics", tabName = "dashboard", icon = icon("chart-line")),
        menuItem("Distraction Analysis", tabName = "distraction", icon = icon("mobile-screen")),
        menuItem("Focus Enhancement", tabName = "focus", icon = icon("brain")),
        menuItem("Study Recommendations", tabName = "recommend", icon = icon("book")),
        menuItem("Reports Engine", tabName = "reports", icon = icon("file-pdf")),
        menuItem("Focus Sandbox Mode", tabName = "focusmode", icon = icon("desktop")),
        menuItem("About Project", tabName = "about", icon = icon("circle-info")),
        menuItem("Log Out Profile", tabName = "logout_trigger", icon = icon("power-off"))
      )
    }
  })
  
  # Dynamic Layout Body Router Nexus
  output$mainBodyUI <- renderUI({
    if (!authSession$authenticated) {
      if (authSession$viewMode == "LOGIN") {
        # --- VIEW: INTERACTIVE GATE LOGIN ---
        fluidRow(
          div(class = "login-box",
              h2(class = "login-title", icon("lock"), "FocusAI Platform Entry"),
              hr(),
              div(class = "role-selector",
                  radioButtons("login_role", "Select System Portal Profile:",
                               choices = c("Student Portal" = "STUDENT", "Administrator Console" = "ADMIN"), selected = "STUDENT")
              ),
              conditionalPanel(
                condition = "input.login_role == 'STUDENT'",
                textInput("login_name", "Registered Username:", value = ""),
                passwordInput("login_password", "Security Account Password:", value = "")
              ),
              conditionalPanel(
                condition = "input.login_role == 'ADMIN'",
                passwordInput("admin_password", "Admin System Cryptographic Key:", value = "")
              ),
              br(),
              actionButton("login_btn", "Verify and Authenticate Identity", class = "btn-primary btn-block", style = "font-weight: bold; padding: 11px; font-size: 15px;"),
              actionLink("go_signup", "Create new student profile? Sign Up Here", class = "toggle-link")
          )
        )
      } else {
        # --- VIEW: STACK FORM VALIDATION SIGNUP PANEL ---
        fluidRow(
          div(class = "login-box",
              h2(class = "login-title", icon("user-plus"), "Account Creation Hub"),
              hr(),
              textInput("student_name", "Full Name (Must be > 5 characters):", value = ""),
              textInput("student_email", "Academic Email (Requires letters, numbers, ends in '.com'):", value = ""),
              textInput("student_phone", "Mobile Registry Number (10 digits, starts with 6-9):", value = ""),
              selectInput("student_year", "Profile Domain Academic Year Grouping:", 
                          choices = c("First Year / Freshman" = "Y1", "Second Year / Sophomore" = "Y2", "Third Year / Junior" = "Y3", "Fourth Year / Senior" = "Y4")),
              passwordInput("student_pass1", "Set Password (Letters + Numbers + Special Characters):", value = ""),
              br(),
              actionButton("signup_btn", "Confirm & Register Identity Core", class = "btn-success btn-block", style = "font-weight: bold; padding: 11px; font-size: 15px;"),
              actionLink("go_login", "Return to login credentials card screen", class = "toggle-link")
          )
        )
      }
    } else {
      # --- AUTHENTICATED MODULE CORE CONSOLE VIEWS ---
      tabItems(
        tabItem(tabName = "admin_dashboard",
                if (authSession$currentRole == "ADMIN") {
                  fluidRow(
                    box(width = 12, status = "danger", solidHeader = TRUE, title = "Master User Enrolment Log Files Directory",
                        p("Decrypted database directory matrix updating live as students register profiles:"),
                        hr(),
                        DTOutput("adminUserTable")
                    ),
                    box(width = 4, status = "warning", solidHeader = TRUE, title = "Terminal Commands",
                        actionButton("logout_btn", "Securely Terminate Admin Session", class = "btn-warning btn-block")
                    )
                  )
                } else {
                  div(class = "access-denied", icon("ban", class = "fa-4x"), h3("CRITICAL INTERCEPT: Administrative Authorization Level Required."))
                }
        ),
        
        tabItem(tabName = "home",
          fluidRow(
            box(width = 12, status = "primary", solidHeader = TRUE, title = "Adaptive Focus Optimization Workspace Nexus",
              h2(paste("Active System Operator:", authSession$activeUser)),
              br(),
              h4("Core Structural Pillars Installed:"),
              tags$ul(
                tags$li("Chrome-Trusted Sequence Handshake Window Spawning Engine"),
                tags$li("Dynamic 3-Slot Link Custom Resource Repository Management"),
                tags$li("Automatic Focus Deviation Intercept Telemetry via Window Listeners"),
                tags$li("Longitudinal Reports Aggregation Matrix Engine")
              )
            )
          )
        ),
        
        tabItem(tabName = "dashboard",
          fluidRow(
            valueBoxOutput("focusBox", width = 3), valueBoxOutput("distractionBox", width = 3),
            valueBoxOutput("studyBox", width = 3), valueBoxOutput("productivityBox", width = 3)
          ),
          fluidRow(
            box(width = 6, title = "Daily Focus Distribution Trend", status = "primary", plotlyOutput("focusTrend")),
            box(width = 6, title = "Weekly Recurrent Distraction Frequency", status = "warning", plotlyOutput("distractionTrend"))
          )
        ),
        
        tabItem(tabName = "distraction",
          fluidRow(
            box(width = 4, title = "Input Behavioral Variables Metrics", status = "danger", solidHeader = TRUE,
              sliderInput("mobile", "Mobile Device Usage (Daily Hours):", min = 0, max = 10, value = 3),
              sliderInput("social", "Social Media Platform Engagements (Hours):", min = 0, max = 10, value = 2),
              sliderInput("notification", "Push Notifications Dispatched Per Hour:", min = 0, max = 50, value = 12),
              sliderInput("idle", "System Ambient Idle Time (Minutes):", min = 0, max = 120, value = 15),
              actionButton("analyze", "Execute ML Simulation Analysis", class = "btn-danger btn-block")
            ),
            box(width = 8, title = "Classification Model Metrics Output", status = "success", solidHeader = TRUE,
              h3(textOutput("distractionLevel")), hr(), plotlyOutput("pieChart")
            )
          )
        ),
        
        tabItem(tabName = "focus",
          fluidRow(
            box(width = 6, status = "warning", solidHeader = TRUE, title = "Attention Calibration Inputs",
              sliderInput("focusscore", "Self-Reported Focus Index Baseline:", min = 0, max = 100, value = 65),
              sliderInput("studytime", "Target Continuous Work Window (Hours):", min = 1, max = 8, value = 2)
            ),
            box(width = 6, status = "success", title = "Adaptive Feedback Diagnostics",
              h3("Focus Target Matrix Output"), textOutput("focusImprove"), hr(),
              h4("Prescriptive Behavioral Tweaks:"),
              tags$ul(tags$li("Activate Device Focus Profiles immediately."), tags$li("Deploy Pomodoro Interval Breaks (25 min on / 5 min off)."))
            )
          )
        ),
        
        tabItem(tabName = "recommend",
          fluidRow(
            box(width = 4, status = "primary", solidHeader = TRUE, title = "Curricular Selection Core",
              selectInput("subject", "Target Academic Domain Subject:", 
                          choices = c("Python Programming" = "python", "Java Programming" = "java", "C++ Programming" = "cpp", "C Systems Programming" = "c", "R for Data Science" = "r", "Data Structures & Algorithms (DSA)" = "dsa", "Computer Networks" = "networks"))
            ),
            box(width = 8, status = "success", solidHeader = TRUE, title = "Automated High-Performance Schedule Optimization Matrix",
              h3(textOutput("recommendedSubjectHeader")), p("The curriculum map below isolates precise conceptual elements ordered strictly by operational importance:"), hr(), tableOutput("studyPlan")
            )
          )
        ),
        
        tabItem(tabName = "reports",
          fluidRow(box(width = 12, status = "primary", title = "Aggregated Longitudinal Logs Engine", DTOutput("reportTable")))
        ),
        
        tabItem(tabName = "focusmode",
          fluidRow(
            box(width = 5, title = "Secure Study Resource Management", status = "danger", solidHeader = TRUE,
              p(tags$b("Project Demonstration Protocol:")), hr(),
              textInput("link1", "Study Link Slot 1 URL:", "https://www.youtube.com/watch?v=dQw4w9WgXcQ"),
              textInput("link2", "Study Link Slot 2 URL:", "https://www.geeksforgeeks.org"),
              textInput("link3", "Study Link Slot 3 URL:", "https://scholar.google.com"),
              selectInput("selectedLink", "Active Study Path Selection:", choices = c("Link 1", "Link 2", "Link 3")), br(), uiOutput("dynamicActionBtnContainer")
            ),
            box(width = 7, title = "Secure Monitor Display Canvas", status = "success", solidHeader = TRUE,
              div(style = "background-color: #f0fdf4; padding: 15px; border-left: 5px solid #16a34a; border-radius: 4px;",
                  h3(style = "margin: 0 0 5px 0; font-weight: bold; color: #16a34a;", textOutput("focusStatus")), p(style = "margin: 0; font-size: 13px; color: #1e293b;", htmlOutput("currentModeText"))
              ), br(),
              div(style = "border: 2px dashed #cbd5e1; border-radius: 6px; padding: 30px; text-align: center; color: #475569; background-color: #f8fafc;",
                  icon("shield-halved", class = "fa-3x", style = "color: #94a3b8; margin-bottom: 10px;"), h4("Security Automation Engine Listening..."), p("Your targeted workspace window has been separated into an isolated window.")
              )
            )
          )
        ),
        
        tabItem(tabName = "about",
          fluidRow(
            box(width = 12, title = "Platform Sidebar & Functional Module Blueprint", status = "primary", solidHeader = TRUE,
              h2(style = "margin-top:0; font-weight: bold;", "Sidebar Architecture & Functional Directory"), hr(),
              tags$ul(style = "padding-left: 0;",
                tags$li(class = "about-sub-item", span(class = "tab-tag", "1. Home Portal"), tags$strong("Primary Landing Interface")),
                tags$li(class = "about-sub-item", span(class = "tab-tag", "2. Dashboard Metrics"), tags$strong("Real-Time Analytics Core")),
                tags$li(class = "about-sub-item", span(class = "tab-tag", "3. Distraction Analysis"), tags$strong("Machine Learning Simulator Matrix")),
                tags$li(class = "about-sub-item", span(class = "tab-tag", "4. Focus Enhancement"), tags$strong("Adaptive Feedback & Behavioral Engine")),
                tags$li(class = "about-sub-item", span(class = "tab-tag", "5. Study Recommendations"), tags$strong("Curricular Priority Routing Matrix")),
                tags$li(class = "about-sub-item", span(class = "tab-tag", "6. Reports Engine"), tags$strong("Longitudinal Log Auditing Matrix")),
                tags$li(class = "about-sub-item", span(class = "tab-tag", "7. Focus Sandbox Mode"), tags$strong("Secure Environment Encapsulation Field")),
                tags$li(class = "about-sub-item", span(class = "tab-tag", "8. About Project"), tags$strong("System Architectural Blueprint"))
              )
            )
          )
        ),
        
        tabItem(tabName = "logout_trigger",
                fluidRow(
                  box(width = 4, status = "danger", solidHeader = TRUE, title = "Session Termination Lock",
                      p("Are you sure you want to exit the system and close terminal locks?"), hr(),
                      actionButton("logout_btn", "Confirm Profile Sign-Out", class = "btn-danger btn-block")
                  )
                )
        )
      )
    }
  })
  
  # --- Admin User Directory Render Pipeline ---
  output$adminUserTable <- renderDT({ datatable(userDirectory$ledger, options = list(pageLength = 10, scrollX = TRUE), class = "stripe hover cell-border") })
  
  # Tab-Switching Intercept Hook
  observeEvent(input$tab_switched, {
    if (focusData$activeUrl != "") { 
      focusData$distractions <- focusData$distractions + 1
      focusData$systemState <- "Distraction Analysis Mode Active (Tab minimized/switched)"
    }
  })
  
  # Dynamic Button Generator Pipeline
  output$dynamicActionBtnContainer <- renderUI({
    chosen_path <- switch(input$selectedLink, "Link 1" = input$link1, "Link 2" = input$link2, "Link 3" = input$link3)
    js_call <- sprintf("deploySecureWorkspace('%s'); Shiny.setInputValue('register_session_start', '%s', {priority: 'event'}); return false;", chosen_path, chosen_path)
    tags$button(id = "openLink", type = "button", class = "btn btn-success btn-block", style = "font-weight: bold; padding: 12px; font-size: 15px;", onclick = js_call, icon("lock"), " Deploy Link & Lock Workspace")
  })
  
  # Catch session start
  observeEvent(input$register_session_start, {
    focusData$activeUrl <- input$register_session_start
    focusData$systemState = "Study Analysis Mode Active (Enforced Monitoring Workspace Launched)"
    showNotification("Security Protocol Loaded! Monitored target active.", type = "message")
  })
  
  output$focusStatus <- renderText({ paste0("Focus Tracking Counter: ", focusData$distractions, " Distractions Captured") })
  output$currentModeText <- renderText({ paste0("System Engine State: <b>", focusData$systemState, "</b>") })
  
  output$focusBox <- renderValueBox({ 
    base_score <- isolate(input$focusscore); if(is.null(base_score)) base_score <- 65
    calculated_index <- max(10, base_score - (focusData$distractions * 3.5))
    valueBox(paste0(round(calculated_index, 1), "%"), "Focus Index Profile", icon = icon("brain"), color = "green") 
  })
  output$distractionBox <- renderValueBox({ valueBox(focusData$distractions, "Session Exceptions Intercepts", icon = icon("triangle-exclamation"), color = "red") })
  output$studyBox <- renderValueBox({ valueBox("4.0 Hrs", "Accumulated Analytics Window", icon = icon("book"), color = "blue") })
  output$productivityBox <- renderValueBox({ 
    base_prod <- 95 - (focusData$distractions * 4); calculated_productivity <- max(5, base_prod)
    valueBox(paste0(round(calculated_productivity, 1), "%"), "Performance Target Matching", icon = icon("chart-line"), color = "yellow") 
  })
  
  output$focusTrend <- renderPlotly({
    base_val <- input$focusscore; if(is.null(base_val)) base_val <- 65
    current_live <- max(10, base_val - (focusData$distractions * 3.5))
    df <- data.frame(Day = factor(c("Mon","Tue","Wed","Thu","Fri"), levels=c("Mon","Tue","Wed","Thu","Fri")), Score = c(62, 68, 74, 71, current_live))
    plot_ly(df, x = ~Day, y = ~Score, type = "scatter", mode = "lines+markers", line = list(color = '#2563EB', width = 3)) %>% layout(yaxis = list(range = c(0, 100)))
  })
  
  output$distractionTrend <- renderPlotly({
    df <- data.frame(Day = factor(c("Mon","Tue","Wed","Thu","Fri"), levels=c("Mon","Tue","Wed","Thu","Fri")), AnomalyCount = c(18, 14, 11, 8, focusData$distractions))
    plot_ly(df, x = ~Day, y = ~AnomalyCount, type = "bar", marker = list(color = '#F59E0B'))
  })
  
  observeEvent(input$analyze, {
    aggregate_risk_score <- input$mobile + input$social + (input$notification / 12) + (input$idle / 15)
    classification_label <- if (aggregate_risk_score < 6) "LOW DISTRACTION LEVEL" else if (aggregate_risk_score < 11) "MEDIUM DISTRACTION COEFFICIENT" else "HIGH DISTRACTION INTERRUPT DETECTED"
    output$distractionLevel <- renderText({ classification_label })
    output$pieChart <- renderPlotly({ plot_ly(labels = c("Compliant Focus Window", "Interference Anomalies"), values = c(max(100 - (aggregate_risk_score * 7.5), 15), aggregate_risk_score * 7.5), type = "pie") })
  })
  
  output$focusImprove <- renderText({ paste("Targeted Optimization Potential:", input$focusscore + 12, "%") })
  
  output$recommendedSubjectHeader <- renderText({
    subj_names <- c("python" = "Python Development Matrix", "java" = "Java Enterprise Architecture", "cpp" = "C++ Object-Oriented Engineering", "c" = "C Low-Level Systems Programming", "r" = "R Statistical Data Analytics", "dsa" = "Data Structures & Algorithms (DSA) Blueprint", "networks" = "Computer Networks & Layered Architectures")
    subj_names[input$subject]
  })
  
  output$studyPlan <- renderTable({
    req_hours <- 4 
    master_curriculum <- list(
      "python" = data.frame(Topic_Context = c("Variables, Data Types & Basic Operators", "Conditional Statements & Execution Loops", "Functions, Scope & Argument Passing (*args, **kwargs)", "Lists, Tuples, Strings & Dictionary Mappings", "Lambda Functions, Map, Filter & Reduce closures", "Object-Oriented Blueprinting (Classes, Inheritance)", "File IO Handling & Exception Handlers", "Decorators & Advanced Inner Functions"), Priority_Rank = c("CRITICAL (Fundamental)", "CRITICAL (Fundamental)", "HIGH (Core Engine)", "HIGH (Core Engine)", "MEDIUM (Functional)", "MEDIUM (OOP Architecture)", "LOWER (Implementation)", "LOWER (Advanced)")),
      "java" = data.frame(Topic_Context = c("JVM Runtime Architecture, Compilation & Syntax", "Pure Object-Oriented Blueprinting (Encapsulation, Inheritance)", "Interfaces & Abstract Structural Formations", "Java Collections Framework (ArrayList, HashMap Engine)", "Generics Validation & Data Iterators", "Multi-Threading, Concurrency & Synchronization", "Exception Handlers & Try-Catch Blocks", "Stream IO APIs & File Management"), Priority_Rank = c("CRITICAL (Fundamental)", "CRITICAL (Fundamental)", "HIGH (Core Engine)", "HIGH (Core Engine)", "MEDIUM (Generics)", "MEDIUM (Concurrency)", "LOWER (Implementation)", "LOWER (Advanced)")),
      "cpp" = data.frame(Topic_Context = c("Base Compilation Pipeline, Syntax & Scope Resolution", "Pointer Engineering, References & Heap Memory Layout", "Dynamic Allocation Matrices (new / delete operators)", "Object-Oriented Design (Classes & Encapsulation)", "Polymorphism, Virtual Functions & Abstract Interfaces", "Standard Template Library (STL Vectors, Maps)", "Function & Operator Overloading Engines", "Memory De-allocation Audits & Advanced Iterators"), Priority_Rank = c("CRITICAL (Fundamental)", "CRITICAL (Fundamental)", "HIGH (Core Engine)", "HIGH (Core Engine)", "MEDIUM (OOP Architecture)", "MEDIUM (STL Framework)", "LOWER (Implementation)", "LOWER (Advanced)")),
      "c" = data.frame(Topic_Context = c("Procedural Constructs, Primitive Datatypes & Base Operators", "Explicit Pointer Arithmetic & Address Referencing", "Array Mapping Matrices & Direct Dereferencing", "Manual Memory Allocation (malloc, calloc, free)", "User-Defined Structure Engines (struct, union)", "Bitwise Operation Controllers & Custom Enums", "Header System Compiles & Preprocessor Directives", "File System Descriptors & Persistent Stream IO"), Priority_Rank = c("CRITICAL (Fundamental)", "CRITICAL (Fundamental)", "HIGH (Core Engine)", "HIGH (Core Engine)", "MEDIUM (Data Structuring)", "MEDIUM (Bitwise Mechanics)", "LOWER (Compilation)", "LOWER (Advanced)")),
      "r" = data.frame(Topic_Context = c("Vectorized Data Structures (Vectors, Matrices, Lists)", "Dataframe Manipulation, Slicing & Subsetting", "Tidyverse Pipelines (dplyr Data Filtering & Mutation)", "tidyr Restructuring & Structural Reshaping", "Descriptive Statistics & Exploratory Data Analysis", "Hypothesis Testing Matrices (T-Tests, ANOVA)", "ggplot2 Geometry Layers & Aesthetic Mapping", "Dynamic Linear & Logistic Regression Engines"), Priority_Rank = c("CRITICAL (Fundamental)", "CRITICAL (Fundamental)", "HIGH (Core Engine)", "HIGH (Core Engine)", "MEDIUM (Analytics)", "MEDIUM (Statistical Inference)", "LOWER (Visualization)", "LOWER (Modeling)")),
      "dsa" = data.frame(Topic_Context = c("Asymptotic Notation Metrics (Time & Space Big-O Complexity)", "Linear Array Structures & Singly/Doubly Linked Lists", "Stack & Queue Implementations (LIFO/FIFO execution)", "Searching Operations (Linear vs Optimized Binary Search)", "Sorting Algorithms (MergeSort, QuickSort Upper Bounds)", "Hierarchical Hash Tables & Collision Protocols", "Non-Linear Binary Trees & Heap Tree Formations", "Advanced Graphs, BFS/DFS Traversals & Dynamic Programming"), Priority_Rank = c("CRITICAL (Analysis Base)", "CRITICAL (Fundamental)", "HIGH (Core Engine)", "HIGH (Core Engine)", "MEDIUM (Sorting Bounds)", "MEDIUM (Hashing Mechanics)", "LOWER (Tree Structures)", "LOWER (Advanced DP)")),
      "networks" = data.frame(Topic_Context = c("Layered Architecture Models (OSI 7-Layer vs TCP/IP Suite)", "Transport Layer Mechanisms (TCP 3-Way Handshake, UDP)", "Network Addressing Schemes (IPv4 Subnetting & IPv6 Structure)", "Routing Protocol Algorithms (RIP, OSPF Link-State)", "Data Link Framing Protocols, Error Controls & MAC Layer", "Application Layer Infrastructure (DNS, HTTP Engines)", "Network Address Translation (NAT) & Port Porting Matrices", "Network Security Architectures (SSL/TLS Handshaking)"), Priority_Rank = c("CRITICAL (Architecture)", "CRITICAL (Data Integrity)", "HIGH (Core Routing)", "HIGH (Core Routing)", "MEDIUM (Link Layer)", "MEDIUM (Application)", "LOWER (Translation)", "LOWER (Security Engine)"))
    )
    df_selected <- master_curriculum[[input$subject]]
    proportional_weights <- c(0.22, 0.18, 0.15, 0.13, 0.11, 0.09, 0.07, 0.05)
    calculated_hours <- round(req_hours * proportional_weights, 2)
    data.frame(Topic_Execution_Focus = df_selected$Topic_Context, Pedagogical_Importance = df_selected$Priority_Rank, Temporal_Allocation = paste(calculated_hours, "Hrs"))
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'l')
  
  output$reportTable <- renderDT({
    datatable(data.frame(Session_Reference_Token = c("SESS-10901", "SESS-10902", "SESS-10903"), Baseline_Focus_Score = c(72, 79, 86), System_Matched_Productivity = c(76, 82, 91), Observed_Environmental_Faults = c(22, 14, focusData$distractions)), options = list(pageLength = 5, scroller = TRUE, dom = 't'))
  })
}

shinyApp(ui = ui, server = server)