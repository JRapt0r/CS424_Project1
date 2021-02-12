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

# Raw numbers for the amount of each energy source per year from 1990 - 2019
rawTotalsPerYear <- aggregate(x = energyWithoutTotal$megaWattHours,by = list(energyWithoutTotal$year,energyWithoutTotal$energySource), FUN = sum)
names(rawTotalsPerYear) <- c("year","energyType", "totalEnergyProduced")

# Raw numbers for the PERCENT of the total production for each energy source per year from 1990 - 2019
(percentContributionPerYear <- ddply(energyWithoutTotal, .(year, energySource), summarize, yearly_usage=sum(megaWattHours)))
percentContributionPerYear <- ddply(percentContributionPerYear, .(year), mutate, yearly_percentage = yearly_usage / sum(yearly_usage))

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

# Create the shiny dashboard
ui <- fluidPage(
  title = "CS 424: Project 1",
  navbarPage("Project 1",
    tabPanel("Plot",
      sidebarLayout(
          sidebarPanel(
              checkboxInput("check1", "All",TRUE),
              checkboxInput("check2", "Coal",FALSE),
              checkboxInput("check3", "Geothermal",FALSE),
              checkboxInput("check4", "Hydroelectric Conventional",FALSE),
              checkboxInput("check5", "Natural Gas",FALSE),
              checkboxInput("check6", "Nuclear",FALSE),
              checkboxInput("check7", "Petroleum",FALSE),
              checkboxInput("check8", "Solar Thermal and Photovoltaic",FALSE),
              checkboxInput("check9", "Wind",FALSE),
              checkboxInput("check10",  "Wood and Wood Derived Fuels",FALSE),
              selectInput("stateSelect1", "First state:",
                c("Alabama" = "AL",
                "Alaska" = "AK",
                "Arizona" = "AZ",
                "Arkansas" = "AR",
                "California" = "CA",
                "Colorado" = "CO",
                "Connecticut" = "CT",
                "Delaware" = "DE",
                "Florida" = "FL",
                "Georgia" = "GA",
                "Hawaii" = "HI",
                "Idaho" = "ID",
                "Illinois" = "IL",
                "Indiana" = "IN",
                "Iowa" = "IA",
                "Kansas" = "KS",
                "Kentucky" = "KY",
                "Louisiana" = "LA",
                "Maine" = "ME",
                "Maryland" = "MD",
                "Massachusetts" = "MA",
                "Michigan" = "MI",
                "Minnesota" = "MN",
                "Mississippi" = "MS",
                "Missouri" = "MO",
                "Montana" = "MT",
                "Nebraska" = "NE",
                "Nevada" = "NV",
                "New Hampshire" = "NH",
                "New Jersey" = "NJ",
                "New Mexico" = "NM",
                "New York" = "NY",
                "North Carolina" = "NC",
                "North Dakota" = "ND",
                "Ohio" = "OH",
                "Oklahoma" = "OK",
                "Oregon" = "OR",
                "Pennsylvania" = "PA",
                "Rhode Island" = "RI",
                "South Carolina" = "SC",
                "South Dakota" = "SD",
                "Tennessee" = "TN",
                "Texas" = "TX",
                "Utah" = "UT",
                "Vermont" = "VT",
                "Virginia" = "VA",
                "Washington" = "WA",
                "West Virginia" = "WV",
                "Wisconsin" = "WI",
                "Wyoming" = "WY",
                "Washington DC" = "DC",
                "Total" = "ALL"),
                selected="IL"
            ),
            selectInput("stateSelect2", "Second state:",
                c("Alabama" = "AL",
                "Alaska" = "AK",
                "Arizona" = "AZ",
                "Arkansas" = "AR",
                "California" = "CA",
                "Colorado" = "CO",
                "Connecticut" = "CT",
                "Delaware" = "DE",
                "Florida" = "FL",
                "Georgia" = "GA",
                "Hawaii" = "HI",
                "Idaho" = "ID",
                "Illinois" = "IL",
                "Indiana" = "IN",
                "Iowa" = "IA",
                "Kansas" = "KS",
                "Kentucky" = "KY",
                "Louisiana" = "LA",
                "Maine" = "ME",
                "Maryland" = "MD",
                "Massachusetts" = "MA",
                "Michigan" = "MI",
                "Minnesota" = "MN",
                "Mississippi" = "MS",
                "Missouri" = "MO",
                "Montana" = "MT",
                "Nebraska" = "NE",
                "Nevada" = "NV",
                "New Hampshire" = "NH",
                "New Jersey" = "NJ",
                "New Mexico" = "NM",
                "New York" = "NY",
                "North Carolina" = "NC",
                "North Dakota" = "ND",
                "Ohio" = "OH",
                "Oklahoma" = "OK",
                "Oregon" = "OR",
                "Pennsylvania" = "PA",
                "Rhode Island" = "RI",
                "South Carolina" = "SC",
                "South Dakota" = "SD",
                "Tennessee" = "TN",
                "Texas" = "TX",
                "Utah" = "UT",
                "Vermont" = "VT",
                "Virginia" = "VA",
                "Washington" = "WA",
                "West Virginia" = "WV",
                "Wisconsin" = "WI",
                "Wyoming" = "WY",
                "Washington DC" = "DC",
                "Total" = "ALL"),
                selected="ALL"
            ),
            selectInput("yearSelect1", "First year:",
                c("1990" = 1990,
                "1991" = 1991,
                "1992" = 1992,
                "1993" = 1993,
                "1994" = 1994,
                "1995" = 1995,
                "1996" = 1996,
                "1997" = 1997,
                "1998" = 1998,
                "1999" = 1999,
                "2000" = 2000,
                "2001" = 2001,
                "2002" = 2002,
                "2003" = 2003,
                "2004" = 2004,
                "2005" = 2005,
                "2006" = 2006,
                "2007" = 2007,
                "2008" = 2008,
                "2009" = 2009,
                "2010" = 2010,
                "2011" = 2011,
                "2012" = 2012,
                "2013" = 2013,
                "2014" = 2014,
                "2015" = 2015,
                "2016" = 2016,
                "2017" = 2017,
                "2018" = 2018,
                "2019" = 2019,
                "ALL" = 0),
                selected=0
            ),
            selectInput("yearSelect2", "Second year:",
                c("1990" = 1990,
                "1991" = 1991,
                "1992" = 1992,
                "1993" = 1993,
                "1994" = 1994,
                "1995" = 1995,
                "1996" = 1996,
                "1997" = 1997,
                "1998" = 1998,
                "1999" = 1999,
                "2000" = 2000,
                "2001" = 2001,
                "2002" = 2002,
                "2003" = 2003,
                "2004" = 2004,
                "2005" = 2005,
                "2006" = 2006,
                "2007" = 2007,
                "2008" = 2008,
                "2009" = 2009,
                "2010" = 2010,
                "2011" = 2011,
                "2012" = 2012,
                "2013" = 2013,
                "2014" = 2014,
                "2015" = 2015,
                "2016" = 2016,
                "2017" = 2017,
                "2018" = 2018,
                "2019" = 2019,
                "ALL" = 0),
                selected=0
            )
          ),
          mainPanel(
            tabsetPanel(
                id = 'dataset',
                tabPanel("Energy Usage Bar", plotOutput("line0"), plotOutput("line4") ),
                tabPanel("Energy Usage Bar (percentage)", plotOutput("line1"), plotOutput("line5") ),
                tabPanel("Energy Usage Line", plotOutput("line2"), plotOutput("line6") ),
                tabPanel("Energy Usage Line (percentage)", plotOutput("line3"))
            )
          )
      )
    ),
    tabPanel("Raw Data",
        fluidPage(
          mainPanel(
            tabsetPanel(
              id = 'rawdata',
              tabPanel("rawTotalsPerYear", DT::dataTableOutput("mytable0")),
              tabPanel("rawTotalsPerYearPercent", DT::dataTableOutput("mytable1"))
            )
          )
        )
    ),
    tabPanel("About",
      verbatimTextOutput("about")
    )
  )
)


server <- function(input, output) {

  theme_set(theme_grey(base_size = 18))

  # Handle reactive checkboxes
  justOneEnergySourceReactive <- reactive({
    dataSet <- NULL
    toReturn <- NULL

    # All years
    if (input$yearSelect1 == 0) {
      dataSet <- energyWithoutTotal
    }
    else {
      dataSet <- subset(energyWithoutTotal, energyWithoutTotal$year == input$yearSelect1)
    }

    # State select
    if (input$stateSelect1 == "ALL") {
      dataSet <- dataSet
    }
    else {
      dataSet <- subset(dataSet, dataSet$state == input$stateSelect1)
    }

    # Energy source toggles
    if (input$check2) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Coal"))
    }
    if (input$check3) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Geothermal"))
    }
    if (input$check4) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Hydroelectric Conventional"))
    }
    if (input$check5) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Natural Gas"))
    }
    if (input$check6) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Nuclear"))
    }
    if (input$check7) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Petroleum"))
    }
    if (input$check8) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Solar Thermal and Photovoltaic"))
    }
    if (input$check9) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Wind"))
    }
    if (input$check10) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Wood and Wood Derived Fuels"))
    }

    # All energy sources
    if (input$check1) {
      toReturn <- dataSet
    }

    # All years
    if (input$yearSelect1 == 0) {
      toReturn <- dataSet
    }

    toReturn
  })

  justOneEnergySourceReactive2 <- reactive({
    dataSet <- NULL
    toReturn <- NULL

    # All years
    if (input$yearSelect2 == 0) {
      dataSet <- energyWithoutTotal
    }
    else {
      dataSet <- subset(energyWithoutTotal, energyWithoutTotal$year == input$yearSelect2)
    }

    # State select
    if (input$stateSelect2 == "ALL") {
      dataSet <- dataSet
    }
    else {
      dataSet <- subset(dataSet, dataSet$state == input$stateSelect2)
    }

    # Energy source toggles
    if (input$check2) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Coal"))
    }
    if (input$check3) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Geothermal"))
    }
    if (input$check4) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Hydroelectric Conventional"))
    }
    if (input$check5) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Natural Gas"))
    }
    if (input$check6) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Nuclear"))
    }
    if (input$check7) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Petroleum"))
    }
    if (input$check8) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Solar Thermal and Photovoltaic"))
    }
    if (input$check9) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Wind"))
    }
    if (input$check10) {
       toReturn <- rbind(toReturn, subset(dataSet, dataSet$energySource == "Wood and Wood Derived Fuels"))
    }

    # All energy sources
    if (input$check1) {
      toReturn <- dataSet
    }

    toReturn
  })

  justOneEnergySourcePercentageReactive <- reactive({
    toReturn <- NULL

    if (input$check2) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Coal"))
    }
    if (input$check3) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Geothermal"))
    }
    if (input$check4) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Hydroelectric Conventional"))
    }
    if (input$check5) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Natural Gas"))
    }
    if (input$check6) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Nuclear"))
    }
    if (input$check7) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Petroleum"))
    }
    if (input$check8) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Solar Thermal and Photovoltaic"))
    }
    if (input$check9) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Wind"))
    }
    if (input$check10) {
       toReturn <- rbind(toReturn, subset(percentContributionPerYear, percentContributionPerYear$energySource == "Wood and Wood Derived Fuels"))
    }

    # All
    if (input$check1) {
      toReturn <- percentContributionPerYear
    }

    toReturn
  })

  # Graph reactive graphs
  output$line0 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive(), aes(x = year, y = megaWattHours, fill = energySource))+
    geom_bar(stat="identity")+
    labs(title="Energy Contribution", subtitle="in Billions of Megawatt Hours", x = "Year", y = "Energy Generated (in billion mWh)")+
    scale_y_continuous(labels = function(x) format(x/1000000000, scientific = FALSE))
  })
  output$line4 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive2(), aes(x = year, y = megaWattHours, fill = energySource))+
    geom_bar(stat="identity")+
    labs(title="Energy Contribution", subtitle="in Billions of Megawatt Hours", x = "Year", y = "Energy Generated (in billion mWh)")+
    scale_y_continuous(labels = function(x) format(x/1000000000, scientific = FALSE))
  })

  output$line1 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive(), aes(x = year, y = megaWattHours, fill = energySource))+
    geom_bar(stat="identity", position="fill")+
    labs(title="Energy Contribution", subtitle="as Percentage of Total", x = "Year", y = "Percent contributed")+
    scale_y_continuous(labels=scales::percent)
  })
  output$line5 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive2(), aes(x = year, y = megaWattHours, fill = energySource))+
    geom_bar(stat="identity", position="fill")+
    labs(title="Energy Contribution", subtitle="as Percentage of Total", x = "Year", y = "Percent contributed")+
    scale_y_continuous(labels=scales::percent)
  })


  output$line2 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive(), aes(x = year, y = megaWattHours, fill = energySource, color=energySource))+
    stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
    labs(title="Energy Contribution Over Time", x = "Year", y = "Energy Generated\nin Billions of Megawatt Hours")+
    scale_y_continuous(labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))
  })
  output$line6 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive2(), aes(x = year, y = megaWattHours, fill = energySource, color=energySource))+
    stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
    labs(title="Energy Contribution Over Time", x = "Year", y = "Energy Generated\nin Billions of Megawatt Hours")+
    scale_y_continuous(labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))
  })


  output$line3 <- renderPlot({
    ggplot(data=justOneEnergySourcePercentageReactive(), aes(x = year, y = yearly_percentage, fill = energySource, color=energySource))+
    stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
    labs(title="Energy Contribution Over Time", x = "Year", y = "Energy Generated\nin Billions of Megawatt Hours")+
    scale_y_continuous(labels=scales::percent)
  })


  output$mytable0 <- DT::renderDataTable({
    DT::datatable(rawTotalsPerYear, options = list(orderClasses = TRUE))
  })

  output$mytable1 <- DT::renderDataTable({
    DT::datatable(percentContributionPerYear, options = list(orderClasses = TRUE))
  })

  # About page
  output$about <- renderPrint({
    "Created by: Jonathon Repta\n Data from: https://www.eia.gov/electricity/data/state/ "
  })
}

shinyApp(ui, server)