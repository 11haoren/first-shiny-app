library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(dplyr)
library(plotly)
library(tidyverse)

#link function
createLink <- function(val) {
    paste0('<a href="',val,'" target="_blank" class="btn btn-primary">Info</a>')
}
#INput data 
df <- tibble(file = list.files(pattern = ".csv")) %>% # Define list of file names that contain csv
  mutate(data = map(file, read_csv), # Read in data using file names
         data = map(data, ~mutate_all(.x, as.character))) %>% # Convert all features to character for unnesting 
  unnest() %>% # Unnest the data to create a dataset containing all files 
  mutate(url_link = createLink(Link)) # Create links 

# Filter the data frame to select each file 
lux <- df %>% filter(str_detect(file, "lux")) %>% select(-file)
makeups <- df %>% filter(str_detect(file, "makeup")) %>% select(-file)
hou <- df %>% filter(str_detect(file, "hou")) %>% select(-file)
jew <- df %>% filter(str_detect(file, "jew")) %>% select(-file)
ap <- df %>% filter(str_detect(file, "ap")) %>% select(-file)

{ filenames <- list.files(pattern = ".csv", path = "data")
  for (i in 1:length(filenames)) {
    filename <- stringr::str_replace_all(filenames[i], ".csv", "")
    assign(filename, data.frame(read.csv(paste0("data/",filenames[i]),header = TRUE,
                                         fileEncoding = "UTF-8-BOM",
                                         stringsAsFactors = TRUE)))
  }
  lux$url_link <- createLink(lux$Link)
  lux <- lux[,-which(names(lux) %in% c("Link"))]
  
  makeups$url_link <- createLink(makeups$Link)
  makeups <- makeups[,-which(names(makeups) %in% c("Link"))]
  
  hou$url_link <- createLink(hou$Link)
  hou <- lux[,-which(names(hou) %in% c("Link"))]
  
  jew$url_link <- createLink(jew$Link)
  jew <- makeups[,-which(names(jew) %in% c("Link"))]
  
  ap$url_link <- createLink(ap$Link)
  ap <- makeups[,-which(names(ap) %in% c("Link"))]
}




### creat a function to develop the same table
##############
createtable <- function(id ) {
    DT::renderDataTable(server = FALSE,
                        {datatable({id},
                                   options = list(
                                       pagelength = 5,
                                       selection = "none"))
                            return(id)},
                        escape = FALSE,
                        rownames = FALSE,
                        extensions = c("SearchPanes", "Select", "Buttons", "Scroller", "FixedColumns"),
                        options = list(
                            processing = FALSE,
                            dom = ("Bfrtip"),
                            scrollY = 450,
                            scroller = TRUE,
                            scrollX = TRUE,
                            buttons = list("searchPanes", "copy","print",list(
                                extend ="collection",
                                buttons = c("csv","excel","pdf"),
                                text = "Downlod")),
                            language = list(searchPanes = list(collapse = "Filter Rows")),
                            columnDefs = list(
                                list(searchPanes = list(show = FALSE), targets = 1:13),
                                list(searchPanes = list(controls = FALSE), targets = 0)),
                            selection = "none"
                        )
                        
                        
    )
    
}

createtable2 <- function(id2 ) {
    DT::renderDataTable(server = FALSE,
                        {datatable({id2},
                                   options = list(
                                       pagelength = 5,
                                       selection = "none"))
                            return(id2)},
                        escape = FALSE,
                        rownames = FALSE,
                        extensions = c("SearchPanes", "Select", "Buttons", "Scroller", "FixedColumns"),
                        options = list(
                            processing = FALSE,
                            dom = ("Bfrtip"),
                            scrollY = 450,
                            scroller = TRUE,
                            scrollX = TRUE,
                            buttons = list("searchPanes", "copy","print",list(
                                extend ="collection",
                                buttons = c("csv","excel","pdf"),
                                text = "Downlod")),
                            language = list(searchPanes = list(collapse = "Filter Rows")),
                            columnDefs = list(
                                list(searchPanes = list(show = FALSE), targets = 1:11),
                                list(searchPanes = list(controls = FALSE), targets = 0)),
                            selection = "none"
                        )
                        
                        
    )
    
}
## sprinf iS difficult to use ,and always has bugs.

###########################

header <- dashboardHeader(title = span(tagList(icon("table"), "Search.In")),
                          userOutput("user"))

###########################

sidebar <- dashboardSidebar(
    sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Luxury",  tabName = "Luxury",icon = icon("chess-queen"),
                     badgeLabel = "new", badgeColor = "green"),
            menuItem("Makeups", tabName = "Makeups", icon = icon("wine-bottle")),
            menuItem("Houseware", tabName = "Houseware", icon = icon("couch")),
            menuItem("Jewelry", tabName = "Jewelry", icon = icon("gem")),
            menuItem("Apparel", tabName = "Apparel", icon = icon("tshirt"))
    )
    # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                   label = "search...")
        
)

#######################

body <- dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
        tabItem( tabName = "home",
                 fluidRow(
                     valueBox(nlevels(lux$Company), "company", icon = icon("chess-queen")),
                     valueBoxOutput("makeupbox"),
                     valueBoxOutput("houbox"),
                     valueBoxOutput("jewbox"),
                     valueBoxOutput("apbox")
                         ),
                 fluidRow(
                     selectizeInput(
                         inputId = "Category", 
                         label = "Select a Category", 
                         choices = list("luxury", "makeups", "houseware","jewelry","apparel"), 
                         selected = "luxury",
                         multiple = FALSE
                     ),
                     plotlyOutput( outputId = "p")
                     )
                 
                 ),
        tabItem(tabName = "Luxury",
                DTOutput('lux')),
        tabItem(tabName = "Makeups",
                DTOutput('mak')),
        tabItem(tabName = "Houseware",
                DTOutput('hou')),
        tabItem(tabName = "Jewelry",
                DTOutput('jew')),
        tabItem(tabName = "Apparel",
                DTOutput('ap'))
        
        
    )
)

###########################

ui <- function(req){dashboardPage(header, 
                    sidebar,
                    body)
}
## req be faster

###########################

server <- function(input, output) {
 
    
  
    output$makeupbox <- renderValueBox({
        valueBox(
        nlevels(makeups$Company),
        "company", icon = icon("wine-bottle"),
        color=("purple")
        )
    })
    
    output$houbox <- renderValueBox({
        valueBox(
            nlevels(hou$Company),
            "company", icon = icon("couch"),
            color=("yellow")
        )
    })
    

    output$jewbox <- renderValueBox({
        valueBox(
            nlevels(jew$Company),
            "company", icon = icon("gem"),
            color=("olive")
        )
    })
    
    output$apbox <- renderValueBox({
        valueBox(
            nlevels(ap$Company),
            "company", icon = icon("tshirt"),
            color=("red")
        )
    })
    
    
    dataSource <- reactive({
        switch(input$Category,"luxury" = lux,"makeups" = makeups, "houseware" = hou ,
               "jewelry" = jew,"apparel" = ap
               
               )
    })
    output$p <- renderPlotly({
        
        dataSource() %>% select(Company,Revenue,Year) %>%
            mutate( Revenue = as.numeric(gsub(",", "", Revenue))) %>%
            ggplot( aes(x=Company, y = Revenue,  fill=factor(Year)))+
            geom_col(position = "dodge") + 
            scale_fill_brewer(palette = "Blues")
    }
        
    )

    output$lux <- createtable(lux)
    
    output$mak <- createtable(makeups)
                           
    output$hou <- createtable2(hou)
    
    output$jew <- createtable2(jew)
    
    output$ap <- createtable2(ap)

    

        output$user <- renderUser({
            dashboardUser(
                name = "Mary Zeng", 
                image = "https://riyugo.com/i/2021/04/12/1oebk8.jpg", 
                title = "Business Analyst",
                subtitle = "JD.COM", 
                footer = p("Oh ho ~ gotcha", class = "text-center")
                # fluidRow(
                #     dashboardUserItem(
                #         width = 6,
                #         socialButton(
                #             href = "https://dropbox.com",
                #             icon = icon("dropbox")
                #         )
                #     ),
                #     dashboardUserItem(
                #         width = 6,
                #         socialButton(
                #             href = "https://github.com",
                #             icon = icon("github")
                #         )
                #     )
                # )
            )
            
    }
    )
}
###########################
shinyApp(ui, server)