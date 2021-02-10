# Jonathon Repta
# CS 424 Project 1:
# Using R to visualize data on electrical power generation in the US

# libraries

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)

# Read in data
energy <- read.table(file = "https://www.evl.uic.edu/aej/424/annual_generation_state.csv", sep = ",", header = TRUE)

# convert year to "numeric"
energy$YEAR <- as.numeric(energy$YEAR)

# convert "Megawatthours" to numeric
energy$GENERATION..Megawatthours. <- as.numeric(gsub(pattern=",","", ignore.case = TRUE, energy$GENERATION..Megawatthours))

# Filter negative megawatthour values
energy <- subset(energy, energy$GENERATION..Megawatthours >= 0)

# Unify state totals by giving them the same case
energy$STATE<-toupper(energy$STATE)

# Filter missing identifiers
energy <- subset(energy, energy$STATE != "  ")

# Convert STATE, TYPE OF PRODUCER, and ENERGY SOURCE to categorical values
energy$STATE <- as.factor(energy$STATE)
energy$TYPE.OF.PRODUCER <- as.factor(energy$TYPE.OF.PRODUCER)
energy$ENERGY.SOURCE <- as.factor(energy$ENERGY.SOURCE)

# Remove irrelevant energy sources
energy <- subset(energy, energy$ENERGY.SOURCE != "Other")
energy <- subset(energy, energy$ENERGY.SOURCE != "Other Gases")
energy <- subset(energy, energy$ENERGY.SOURCE != "Other Biomass")
energy <- subset(energy, energy$ENERGY.SOURCE != "Pumped Storage")

# Give columns legible names
names(energy) <- c("year", "state", "producerType", "energySource", "megaWattHours")

# Create a subset of the data that excludes the "total" category
energyWithoutTotal <- subset(energy, energySource != "Total")
energyWithoutTotal <- subset(energyWithoutTotal, state != "US-TOTAL")

# stacked bar chart showing the amount of each energy source per year from 1990 - 2019
ggplot(data=energyWithoutTotal, aes(x = year, y = megaWattHours, fill = energySource))+
geom_bar(stat="identity")+
labs(title="Energy Contribution", subtitle="in Billions of Megawatt Hours", x = "Year", y = "Energy Generated (in billion mWh)")+
scale_y_continuous(labels = function(x) format(x/1000000000, scientific = FALSE))

# stacked bar chart showing PERCENT of the total production for each energy source per year from 1990 - 2019
ggplot(data=energyWithoutTotal, aes(x = year, y = megaWattHours, fill = energySource))+
geom_bar(stat="identity", position="fill")+
labs(title="Energy Contribution", subtitle="as Percentage of Total", x = "Year", y = "Percent contributed")+
scale_y_continuous(labels=scales::percent)

# line chart showing the amount of each energy source per year from 1990 - 2019
ggplot(data=energyWithoutTotal, aes(x = year, y = megaWattHours, fill = energySource, color=energySource))+
stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
labs(title="Energy Contribution Over Time", x = "Year", y = "Energy Generated\nin Billions of Megawatt Hours")+
scale_y_continuous(labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))

# line chart showing the PERCENT of the total production for each energy source per year from 1990 - 2019
(percentContribution <- ddply(energyWithoutTotal, .(year, energySource), summarize, yearly_usage=sum(megaWattHours)))
percentContribution <- ddply(percentContribution, .(year), mutate, yearly_percentage = yearly_usage / sum(yearly_usage))

ggplot(data=percentContribution, aes(x = year, y = yearly_percentage, fill = energySource, color=energySource))+
stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
labs(title="Energy Contribution Over Time", x = "Year", y = "Energy Generated\nin Billions of Megawatt Hours")+
scale_y_continuous(labels=scales::percent)

# Raw numbers for the amount of each energy source per year from 1990 - 2019
rawTotalsPerYear <- rawTotalsPerYearegate(x = energyWithoutTotal$megaWattHours,by = list(energyWithoutTotal$year,energyWithoutTotal$energySource), FUN = sum)
names(rawTotalsPerYear) <- c("year","energyType", "totalEnergyProduced")

# Raw numbers for the PERCENT of the total production for each energy source per year from 1990 - 2019
(percentContributionPerYear <- ddply(energyWithoutTotal, .(year, energySource), summarize, yearly_usage=sum(megaWattHours)))
percentContributionPerYear <- ddply(percentContributionPerYear, .(year), mutate, yearly_percentage = yearly_usage / sum(yearly_usage))


# Create the shiny dashboard
ui <- fluidPage(
  title = "CS 424: Project 1",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "energyWithoutTotal"',
        checkboxGroupInput("show_vars", "Columns of energy sources to show:",
                           names(energyWithoutTotal), selected = names(energyWithoutTotal))
      ),
      conditionalPanel(
        'input.dataset === "rawTotalsPerYear"',
         checkboxGroupInput("show_vars", "Columns of energy sources to show:",
          names(rawTotalsPerYear))
      ),
      conditionalPanel(
        'input.dataset === "iris"',
        helpText("Display 5 records by default.")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("energyWithoutTotal", DT::dataTableOutput("mytable1")),
        tabPanel("rawTotalsPerYear", DT::dataTableOutput("rawTotalsPerYearegation"))
        # tabPanel("iris", DT::dataTableOutput("mytable3"))
      )
    )
  )
)

server <- function(input, output) {

  # choose columns to display
  energyWithoutTotal2 = energyWithoutTotal[sample(nrow(energyWithoutTotal), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(energyWithoutTotal2[, input$show_vars, drop = FALSE])
  })

  # sorted columns are colored now because CSS are attached to them
  rawTotalsPerYear2 = rawTotalsPerYear[sample(nrow(rawTotalsPerYear)), ]
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(rawTotalsPerYear2[, input$show_vars, drop = FALSE])
  })

  # customize the length drop-down menu; display 5 rows per page by default
  # output$mytable3 <- DT::renderDataTable({
  #   DT::datatable(iris, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  # })

}

shinyApp(ui, server)

# TODO
'''
# Create the shiny dashboard
ui <- dashboardPage(
    dashboardHeader(title = "CS 424: Project 1"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,

                     sidebarMenu(
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
                       menuItem("", tabName = "cheapBlankSpace", icon = NULL),

                     selectInput("Year", "Select the year to visualize", years, selected = 2020),
                     selectInput("Room", "Select the room to visualize", listNamesGood, selected = "Meeting Room")
                     ),
    dashboardBody(
  fluidRow(
    column(2,
        fluidRow(
          box(title = "Plotting on a jpeg", solidHeader = TRUE, status = "primary", width = 12,
              plotOutput("jpeg", height = 300)
          )
          ),


        fluidRow(
          box(title = "Leaflet Map", solidHeader = TRUE, status = "primary", width = 12,
              leafletOutput("leaf", height = 200)
          )
        )
    ),

    column(8,
       fluidRow(
            box( title = "Room Temperature Range over a Year", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("hist1", height = 200)
            )
        ),
       fluidRow(
            box( title = "Room Temperature at Noon over a Year", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("hist2", height = 200)
            )
        ),
       fluidRow(
         box( title = "Heatmap of Room Temperature over a Year", solidHeader = TRUE, status = "primary", width = 12,
              plotOutput("hist0", height = 250)
         )
       )
   ),
   column(2,
        fluidRow(
            box(title = "Noon Temps as Box Plot", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("hist4", height = 200)
                )
            ),
        fluidRow(
             box(title = "Noon Temps as Bar Chart", solidHeader = TRUE, status = "primary", width = 12,
                plotOutput("hist3", height = 200)
                )
            ),
        fluidRow(
             box(title = "Noon Temps as Table", solidHeader = TRUE, status = "primary", width = 12,
                dataTableOutput("tab1", height = 200)
                )
             )
        )
    )
))

server <- function(input, output) {

# increase the default font size
theme_set(theme_grey(base_size = 18))

# calculate the values one time and re-use them in multiple charts to speed things up
justOneYearReactive <- reactive({subset(allData, year(allData$newDate) == input$Year)})
newNoonsReactive <- reactive({subset(allData, year(allData$newDate) == input$Year & Hour == 12)})
oneRoomNoonReactive <- reactive({subset(allData$input$Room, year(allData$newDate) == input$Year & Hour == 12)})

# in 2017 it was y=justOneYear["Hour"] - needed to make a change for 2018

# create heat map for the given year and room and play with the colors to make it more readable
output$hist0 <- renderPlot({
    justOneYear <- justOneYearReactive()
    ggplot(justOneYear, aes(x=newDate, color=justOneYear[,input$Room], y=Hour)) +
        labs(x=paste("Day in", input$Year), y = "Hour of the Day")
          geom_point(alpha = 1/2, size = 4, shape=15) + scale_y_continuous() +

        theme_dark(18) + theme(plot.background = element_rect(fill = "gray50")) + theme(axis.title = element_text(colour = "white")) +
            theme(axis.text = element_text(colour = "white")) + theme(panel.grid.major = element_line(colour = "white")) +
            theme(panel.grid.minor = element_line(colour = "white")) +
            theme(legend.background = element_rect(fill="gray50")) +
            theme(legend.text = element_text(colour = "white"))  +
            theme(legend.title = element_text(colour = "white"))  +

        scale_colour_gradient2(low = "green", high = "red", limits=c(60, 90), midpoint=70, mid="yellow") +
            theme(legend.position="top") + labs(colour = "Temperature (F) ") +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
})


# show all of the temperatures for a given room for a given year
output$hist1 <- renderPlot({
    justOneYear <- justOneYearReactive()
    ggplot(justOneYear, aes(x=newDate, y=justOneYear[,input$Room])) +
        labs(x=paste("Day in", input$Year), y = "Temperature (F)") + geom_point(alpha = 1/12) + ylim(55,90) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
    })


# show a line graph of the temperatures at noon for a given room for a given year
output$hist2 <- renderPlot({
    newNoons <- newNoonsReactive()
    ggplot(newNoons, aes(x=newDate, y=newNoons[,input$Room])) +
        labs(x=paste("Day in", input$Year), y = "Temperature (F)") + geom_point() + geom_line() + ylim(55,90) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
})


# show a bar chart of the temperatures at noon for a given room for a given year
output$hist3 <- renderPlot({
    newNoons <-  newNoonsReactive()
    temperatures <- as.data.frame(table(newNoons[,input$Room]))
    temperatures$Var1 <- as.numeric(as.character(temperatures$Var1))

    ggplot(temperatures, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", fill="steelblue") +
        labs(x="Temperature (F)", y = "Count") + xlim(60,90)
})


# show box plot of the temperatures at noon for a given room for a given year
output$hist4 <- renderPlot({
    newTemps2 <- justOneYearReactive()
    ggplot(newTemps2, aes(x = "", y = newTemps2[,input$Room])) + geom_boxplot() + labs(y="Temperature (F)", x="") + ylim(55,90)
})


# use DT to help out with the tables - https://datatables.net/reference/option/
output$tab1 <- DT::renderDataTable(
    DT::datatable({
    newNoons <-  newNoonsReactive()
    temperatures <- as.data.frame(table(newNoons[,input$Room], dnn = list("Temperature")), responseName = "Count")
  },
  options = list(searching = FALSE, pageLength = 5, lengthChange = FALSE, order = list(list(1, 'desc'))
  ), rownames = FALSE
    )
)

# read in a jpeg map of the lab to show the room layout and plot some text on it
output$jpeg <- renderPlot({
    # read in a jpg image
    jp <- jpeg::readJPEG('evl_2nd_floor.jpg')

    df <- data.frame(x = 1:10, y = 1:10) # set the range to be 1 to 10 in x and y for the image

    markerX = 0
    markerY = 0

    if (input$Room == "Classroom")
    {
      markerX = 3
      markerY = 4.5
    }
    else if (input$Room == "Ph.D. Room")
    {
      markerX = 5.25
      markerY = 4.5
    }
    else if (input$Room == "Thin Rooms")
    {
      markerX = 6.7
      markerY = 4.5
    }
    else if (input$Room == "Machine Room")
    {
      markerX = 8.75
      markerY = 4.5
    }
    else if (input$Room == "Work Room")
    {
      markerX = 3
      markerY = 7.1
    }
    else if (input$Room == "Meeting Room")
    {
      markerX = 6
      markerY = 7.1
    }
    else if (input$Room == "Main Lab")
    {
      markerX = 8.75
      markerY = 7.1
    }
    else
    {
      markerX = 0
      markerY = 0
    }

    ggplot(df, aes(x,y)) + geom_blank() + labs(x="", y = "") +
        annotation_custom(rasterGrob(jp, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) +

        annotate("text", x = 3, y = 4.5, label = "Classroom") +
        annotate("text", x = 5.25, y = 4.5, label = "Ph.D \n Room") +
        annotate("text", x = 6.7, y = 4.5, label = "Thin Rooms") +
        annotate("text", x = 8.75, y = 4.5, label = "Machine Room") +
        annotate("text", x = 3, y = 7.1, label = "Work \n Room") +
        annotate("text", x = 6, y = 7.1, label = "Meeting \n Room") +
        annotate("text", x = 8.75, y = 7.1, label = "Main Lab") +

      # show the selected room with a colored overlay based on input$Room
      annotate("rect", xmin = markerX-0.9, xmax = markerX+0.9, ymin = markerY-0.5, ymax = markerY+0.5, fill="green", alpha=0.3) +

        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
        theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
})

# add a leaflet map and put a marker on it at the location of the lab
# while not overly useful this can ceratinly be expnded upon
output$leaf <- renderLeaflet({
    map <- leaflet()
    map <- addTiles(map)
    map <- setView(map, lng = -87.647998, lat = 41.870, zoom = 18)
    map <- addMarkers(map, lng = -87.6477, lat = 41.8698, popup = "evl")
    map
})


}

shinyApp(ui = ui, server = server)

'''