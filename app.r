# Jonathon Repta
# CS 424 Project 1:
# Using R to visualize data on electrical power generation in the US

# libraries

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(RColorBrewer)
library(usmap)
library(stringr)

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
energy <- subset(energy, energy$ENERGY.SOURCE != "Total")

# Give columns legible names
names(energy) <- c("year", "state", "producerType", "energySource", "megaWattHours")

# Create a subset of the data that excludes the "total" category
energyWithoutTotal <- subset(energy, state != "US-TOTAL")

# Create consistent color palette
myColors <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#111111","#A65628","#F781BF","#999999")
names(myColors) <- c("Coal","Geothermal","Hydroelectric Conventional","Natural Gas","Nuclear","Petroleum","Solar Thermal and Photovoltaic","Wind","Wood and Wood Derived Fuels")

# Raw numbers for the amount of each energy source per year from 1990 - 2019
rawTotalsPerYear <- aggregate(x = energyWithoutTotal$megaWattHours,by = list(energyWithoutTotal$year,energyWithoutTotal$energySource), FUN = sum)
names(rawTotalsPerYear) <- c("year","energyType", "totalEnergyProduced")

# Raw numbers for the PERCENT of the total production for each energy source per year from 1990 - 2019
(percentContributionPerYear <- ddply(energyWithoutTotal, .(year, energySource), summarize, yearly_usage=sum(megaWattHours)))
percentContributionPerYear <- ddply(percentContributionPerYear, .(year), mutate, yearly_percentage = yearly_usage / sum(yearly_usage))

# stacked bar chart showing the amount of each energy source per year from 1990 - 2019
ggplot(data=energyWithoutTotal, aes(x = year, y = megaWattHours, fill=energySource))+
geom_bar(stat="identity")+
labs(title="Energy Contribution", subtitle="(in billion Mwh)", x = "Year", y = "Energy Generated\n(in billion Mwh)")+
scale_y_continuous(labels = function(x) format(x/1000000000, scientific = FALSE))+
scale_fill_manual(name = "Energy Sources", values = myColors)

# stacked bar chart showing PERCENT of the total production for each energy source per year from 1990 - 2019
ggplot(data=energyWithoutTotal, aes(x = year, y = megaWattHours, fill=energySource))+
geom_bar(stat="identity", position="fill")+
labs(title="Energy Contribution", subtitle="as Percentage of Total", x = "Year", y = "Percent contributed")+
scale_y_continuous(labels=scales::percent)+
scale_fill_manual(name = "Energy Sources", values = myColors)

# line chart showing the amount of each energy source per year from 1990 - 2019
ggplot(data=energyWithoutTotal, aes(x = year, y = megaWattHours, color=energySource))+
stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
labs(title="Energy Contribution", subtitle="Over Time", x = "Year", y = "Energy Generated\n(in billion Mwh)")+
scale_y_continuous(labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))+
scale_colour_manual(name = "Energy Sources", values = myColors)

# line chart showing the PERCENT of the total production for each energy source per year from 1990 - 2019
(percentContribution <- ddply(energyWithoutTotal, .(year, energySource), summarize, yearly_usage=sum(megaWattHours)))
percentContribution <- ddply(percentContribution, .(year), mutate, yearly_percentage = yearly_usage / sum(yearly_usage))

ggplot(data=percentContribution, aes(x = year, y = yearly_percentage, color=energySource))+
stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
geom_point()+
labs(title="Energy Contribution", subtitle="Over Time", x = "Year", y = "Energy Generated\n(in billion Mwh)")+
scale_y_continuous(labels=scales::percent)+
scale_colour_manual(name = "Energy Sources", values = myColors)


# State contribution for all years (raw totals)
(stateContribution <- ddply(energyWithoutTotal, .(state), summarize, state_usage=sum(megaWattHours)))

# State contribution for all years (percentage)
(stateContributionPercentage <- ddply(energyWithoutTotal, .(state), summarize, state_usage=sum(megaWattHours)))
total_usage = sum(stateContributionPercentage$state_usage)
stateContributionPercentage <- ddply(stateContributionPercentage, .(state), mutate, state_percentage = (state_usage / total_usage)*100)

# Plot regional output
plot_usmap(regions="states", data=stateContribution, values="state_usage", labels=TRUE,label_color = "#ffffff")+
scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Energy Generated\n(in billion Mwh)", labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))+
labs(title="Energy Contribution", subtitle="by Region")

# Plot regional output (by percentage)
plot_usmap(regions="states", data=stateContributionPercentage, values="state_percentage", labels=TRUE, label_color = "#ffffff")+
scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Percent contributed", labels = function(x) format(x, big.mark=",", scientific = FALSE))+
labs(title="Energy Contribution (percentage)", subtitle="by Region")


# Create the shiny dashboard
ui <- fluidPage(
  title = "CS 424: Project 1",
  navbarPage("Project 1",
    tabPanel("Plot",
      fluidRow(
          column(2,
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
                "All states" = "ALL"),
                selected="IL"
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
                  "All years" = 0),
                  selected=0),
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
                "All states" = "ALL"),
                selected="ALL"
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
                "All years" = 0),
                selected=0
            )
          ),
          column(10,
            tabsetPanel(
                id = 'dataset',
                tabPanel("Energy Usage Bar",
                  fluidRow(
                    column(6,
                      plotOutput("line0"),
                      plotOutput("line4")
                    ),
                    column(6,
                      plotOutput("map0"),
                      plotOutput("map4")
                    )
                  )
                ),
                tabPanel("Energy Usage Bar (percentage)",
                  fluidRow(
                    column(6,
                      plotOutput("line1"),
                      plotOutput("line5")
                    ),
                    column(6,
                      plotOutput("map1"),
                      plotOutput("map5")
                    )
                  )
                ),
                tabPanel("Energy Usage Line",
                  fluidRow(
                    column(6,
                      plotOutput("line2"),
                      plotOutput("line6")
                    ),
                    column(6,
                      plotOutput("map2"),
                      plotOutput("map6")
                    )
                  )
                ),
                tabPanel("Energy Usage Line (percentage)",
                  fluidRow(
                    column(6,
                      plotOutput("line3"),
                      plotOutput("line7")
                    ),
                    column(6,
                      plotOutput("map3"),
                      plotOutput("map7")
                    )
                  )
                )
            )
          )
      )
    ),
    tabPanel("Raw Data",
        fluidPage(
          mainPanel(
            tabsetPanel(
              id = 'rawdata',
              tabPanel("Raw Energy Produced", DT::dataTableOutput("mytable0")),
              tabPanel("Raw Energy Produced (percentage)", DT::dataTableOutput("mytable1"))
            )
          )
        )
    ),
    tabPanel("About",
      verbatimTextOutput("name"),
      verbatimTextOutput("date"),
      verbatimTextOutput("dataset")
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

    (percentContributionPerYear <- ddply(dataSet, .(year, energySource), summarize, yearly_usage=sum(megaWattHours)))
    percentContributionPerYear <- ddply(percentContributionPerYear, .(year), mutate, yearly_percentage = yearly_usage / sum(yearly_usage))

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

  justOneEnergySourcePercentageReactive2 <- reactive({
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

    (percentContributionPerYear <- ddply(dataSet, .(year, energySource), summarize, yearly_usage=sum(megaWattHours)))
    percentContributionPerYear <- ddply(percentContributionPerYear, .(year), mutate, yearly_percentage = yearly_usage / sum(yearly_usage))

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

  mapProductionReactive1 <- reactive({
    dataSet <- NULL
    toReturn <- NULL

    # All years
    if (input$yearSelect1 == 0) {
      dataSet <- energyWithoutTotal
    }
    else {
      dataSet <- subset(energyWithoutTotal, energyWithoutTotal$year == input$yearSelect1)
    }

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

    # All
    if (input$check1) {
      toReturn <- dataSet
    }

    # State contribution for all years (raw totals)
    (toReturn <- ddply(toReturn, .(state), summarize, state_usage=sum(megaWattHours)))

    toReturn
  })

  mapProductionReactive2 <- reactive({
    dataSet <- NULL
    toReturn <- NULL

    # All years
    if (input$yearSelect2 == 0) {
      dataSet <- energyWithoutTotal
    }
    else {
      dataSet <- subset(energyWithoutTotal, energyWithoutTotal$year == input$yearSelect2)
    }

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

    # All
    if (input$check1) {
      toReturn <- dataSet
    }

    # State contribution for all years (raw totals)
    (toReturn <- ddply(toReturn, .(state), summarize, state_usage=sum(megaWattHours)))

    toReturn
  })

  mapProductionReactivePercentage1 <- reactive({
    dataSet <- NULL
    toReturn <- NULL

    # All years
    if (input$yearSelect1 == 0) {
      dataSet <- energyWithoutTotal
    }
    else {
      dataSet <- subset(energyWithoutTotal, energyWithoutTotal$year == input$yearSelect1)
    }

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

    # All
    if (input$check1) {
      toReturn <- dataSet
    }

    (toReturn <- ddply(toReturn, .(state), summarize, state_usage=sum(megaWattHours)))
    total_usage = sum(toReturn$state_usage)
    toReturn <- ddply(toReturn, .(state), mutate, state_percentage = (state_usage / total_usage)*100)

    toReturn
  })

  mapProductionReactivePercentage2 <- reactive({
    dataSet <- NULL
    toReturn <- NULL

    # All years
    if (input$yearSelect2 == 0) {
      dataSet <- energyWithoutTotal
    }
    else {
      dataSet <- subset(energyWithoutTotal, energyWithoutTotal$year == input$yearSelect2)
    }

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

    # All
    if (input$check1) {
      toReturn <- dataSet
    }

    (toReturn <- ddply(toReturn, .(state), summarize, state_usage=sum(megaWattHours)))
    total_usage = sum(toReturn$state_usage)
    toReturn <- ddply(toReturn, .(state), mutate, state_percentage = (state_usage / total_usage)*100)

    toReturn
  })


  # Graph reactive graphs
  output$line0 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive(), aes(x = year, y = megaWattHours, fill = energySource))+
    geom_bar(stat="identity")+
    labs(title=paste(input$stateSelect1,"Energy Contribution", sep=" "), subtitle="(in billion Mwh)", x = "Year", y = "Energy Generated\n(in billion Mwh)")+
    scale_y_continuous(labels = function(x) format(x/1000000000, scientific = FALSE))+
    scale_fill_manual(name = "Energy Sources", values = myColors)
  })
  output$map0 <- renderPlot({
    plot_usmap(regions="states", data=mapProductionReactive1(), values="state_usage", labels=TRUE,label_color = "#ffffff")+
    scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Energy Generated\n(in billion Mwh)", labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))+
    labs(title="Energy Contribution", subtitle="by Region")
  })

  output$line4 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive2(), aes(x = year, y = megaWattHours, fill = energySource))+
    geom_bar(stat="identity")+
    labs(title=paste(input$stateSelect2,"Energy Contribution", sep=" "), subtitle="(in billion Mwh)", x = "Year", y = "Energy Generated\n(in billion Mwh)")+
    scale_y_continuous(labels = function(x) format(x/1000000000, scientific = FALSE))+
    scale_fill_manual(name = "Energy Sources", values = myColors)
  })
  output$map4 <- renderPlot({
    plot_usmap(regions="states", data=mapProductionReactive2(), values="state_usage", labels=TRUE,label_color = "#ffffff")+
    scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Energy Generated\n(in billion Mwh)", labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))+
    labs(title="Energy Contribution", subtitle="by Region")
  })


  output$line1 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive(), aes(x = year, y = megaWattHours, fill = energySource))+
    geom_bar(stat="identity", position="fill")+
    labs(title=paste(input$stateSelect1,"Energy Contribution", sep=" "), subtitle="as Percentage of Total", x = "Year", y = "Percent contributed")+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(name = "Energy Sources", values = myColors)
  })
  output$map1 <- renderPlot({
    plot_usmap(regions="states", data=mapProductionReactivePercentage1(), values="state_percentage", labels=TRUE, label_color = "#ffffff")+
    scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Percent contributed", labels = function(x) format(x, big.mark=",", scientific = FALSE))+
    labs(title="Energy Contribution (percentage)", subtitle="by Region")
  })
  output$line5 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive2(), aes(x = year, y = megaWattHours, fill = energySource))+
    geom_bar(stat="identity", position="fill")+
    labs(title=paste(input$stateSelect2,"Energy Contribution", sep=" "), subtitle="as Percentage of Total", x = "Year", y = "Percent contributed")+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(name = "Energy Sources", values = myColors)
  })
  output$map5 <- renderPlot({
    plot_usmap(regions="states", data=mapProductionReactivePercentage2(), values="state_percentage", labels=TRUE, label_color = "#ffffff")+
    scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Percent contributed", labels = function(x) format(x, big.mark=",", scientific = FALSE))+
    labs(title="Energy Contribution (percentage)", subtitle="by Region")
  })


  output$line2 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive(), aes(x = year, y = megaWattHours, color=energySource))+
    stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
    labs(title=paste(input$stateSelect1,"Energy Contribution", sep=" "), subtitle="Over Time", x = "Year", y = "Energy Generated\n(in billion Mwh)")+
    scale_y_continuous(labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))+
    scale_colour_manual(name = "Energy Sources", values = myColors)
  })
  output$map2 <- renderPlot({
    plot_usmap(regions="states", data=mapProductionReactive1(), values="state_usage", labels=TRUE,label_color = "#ffffff")+
    scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Energy Generated\n(in billion Mwh)", labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))+
    labs(title="Energy Contribution", subtitle="by Region")
  })
  output$line6 <- renderPlot({
    ggplot(data=justOneEnergySourceReactive2(), aes(x = year, y = megaWattHours, color=energySource))+
    stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
    labs(title=paste(input$stateSelect2,"Energy Contribution", sep=" "), subtitle="Over Time", x = "Year", y = "Energy Generated\n(in billion Mwh)")+
    scale_y_continuous(labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))+
    scale_colour_manual(name = "Energy Sources", values = myColors)
  })
  output$map6 <- renderPlot({
    plot_usmap(regions="states", data=mapProductionReactive2(), values="state_usage", labels=TRUE,label_color = "#ffffff")+
    scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Energy Generated\n(in billion Mwh)", labels = function(x) format(x/1000000000, big.mark=",", scientific = FALSE))+
    labs(title="Energy Contribution", subtitle="by Region")
  })

  output$line3 <- renderPlot({
    ggplot(data=justOneEnergySourcePercentageReactive(), aes(x = year, y = yearly_percentage, color=energySource))+
    stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
    geom_point()+
    labs(title=paste(input$stateSelect1,"Energy Contribution", sep=" "), subtitle="Over Time (as percentage of total)", x = "Year", y = "Percent contributed")+
    scale_y_continuous(labels=scales::percent)+
    scale_colour_manual(name = "Energy Sources", values = myColors)
  })
  output$map3 <- renderPlot({
    plot_usmap(regions="states", data=mapProductionReactivePercentage1(), values="state_percentage", labels=TRUE, label_color = "#ffffff")+
    scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Percent contributed", labels = function(x) format(x, big.mark=",", scientific = FALSE))+
    labs(title="Energy Contribution (percentage)", subtitle="by Region")
  })

  output$line7 <- renderPlot({
    ggplot(data=justOneEnergySourcePercentageReactive2(), aes(x = year, y = yearly_percentage, color=energySource))+
    stat_summary(fun="sum", geom="line", size=1.0, show.legend=TRUE)+
    geom_point()+
    labs(title=paste(input$stateSelect2,"Energy Contribution", sep=" "), subtitle="Over Time (as percentage of total)", x = "Year", y = "Percent contributed")+
    scale_y_continuous(labels=scales::percent)+
    scale_colour_manual(name = "Energy Sources", values = myColors)
  })
  output$map7 <- renderPlot({
    plot_usmap(regions="states", data=mapProductionReactivePercentage2(), values="state_percentage", labels=TRUE, label_color = "#ffffff")+
    scale_fill_continuous(low = "#1e90ff", high = "#092b4c", name="Percent contributed", labels = function(x) format(x, big.mark=",", scientific = FALSE))+
    labs(title="Energy Contribution (percentage)", subtitle="by Region")
  })


  output$mytable0 <- DT::renderDataTable({
    rt <- rawTotalsPerYear
    names(rt) <- c("Year","Energy Source","Energy Produced (Mwh)")
    DT::datatable(rt, options = list(orderClasses = TRUE))
  })

  output$mytable1 <- DT::renderDataTable({
    (pc <- ddply(energyWithoutTotal, .(year, energySource), summarize, yearly_usage=sum(megaWattHours)))
    pc <- ddply(pc, .(year), mutate, yearly_percentage = (yearly_usage / sum(yearly_usage)) * 100)
    names(pc) <- c("Year", "Energy Source", "Energy Produced (mwH)", "Percent of Yearly Total")
    DT::datatable(pc, options = list(orderClasses = TRUE))
  })

  # About page
  output$name <- renderPrint({
    "Created by: Jonathon Repta"
  })
  output$date <- renderPrint({
    "Created on: February 13, 2021"
  })
  output$dataset <- renderPrint({
    "Data from: https://www.eia.gov/electricity/data/state/"
  })
}

shinyApp(ui, server)