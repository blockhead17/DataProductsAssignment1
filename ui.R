shinyUI(
      pageWithSidebar(
            headerPanel("Welcome to the Pick Nine! app"),
            sidebarPanel(
                  h3("Play Ball!"),
                  p("Instructions: You need to pick both an historic team and build another team 
                    (comprised of Hall of Famers) to compare against."),
                  selectizeInput("topteam", label = h5("Choose an historical team"), 
                        choices = list(
                              "#1 : 1927 New York Yankees" = 1,
                              "#2 : 1939 New York Yankees" = 2,
                              "#3 : 1975 Cincinnati Reds" = 3,
                              "#4 : 1998 New York Yankees" = 4,
                              "#5 : 1929 Philadelphia Athletics" = 5,
                              "#6 : 1961 New York Yankees" = 6,
                              "#7 : 1902 Pittsburgh Pirates" = 7,
                              "#8 : 1970 Baltimore Orioles" = 8,
                              "#9 : 1984 Detroit Tigers" = 9,
                              "#10: 1907 Chicago Cubs" = 10),
                        options = list( #Don't give a default team
                              placeholder = 'Please select an option below',
                              onInitialize = I('function() { this.setValue(""); }')
                        )),
                  hr(),
                  h5("Click the button to put together a team of hall of famers!"),
                  actionButton("goButton", label = "Pick Nine!"),
                  hr(),
                  h5("Choose two offensive statistics to compare the starting rosters of 
                     both teams on a plot."),
                  selectizeInput("plotx", label = h5("Choose X-Axis Statistic"), 
                                 choices = list(
                                       "At Bats (AB)" = "AB",
                                       "Runs (R)" = "R",
                                       "Hits (H)" = "H",
                                       "Home Runs (HR)" = "HR",
                                       "Walks (BB)" = "BB",
                                       "Strikouts (SO)" = "SO",
                                       "Batting Average (AVG)" = "AVG",
                                       "Slugging Percentage (SLG)" = "SLG",
                                       "On Base Percentage (OBP)" = "OBP"),
                                 options = list( #Don't give a default team
                                       placeholder = 'Please select X statistic',
                                       onInitialize = I('function() { this.setValue(""); }')
                                 )),
                  selectizeInput("ploty", label = h5("Choose Y-Axis Statistic"), 
                                 choices = list(
                                       "At Bats (AB)" = "AB",
                                       "Runs (R)" = "R",
                                       "Hits (H)" = "H",
                                       "Home Runs (HR)" = "HR",
                                       "Walks (BB)" = "BB",
                                       "Strikouts (SO)" = "SO",
                                       "Batting Average (AVG)" = "AVG",
                                       "Slugging Percentage (SLG)" = "SLG",
                                       "On Base Percentage (OBP)" = "OBP"),
                                 options = list( #Don't give a default team
                                       placeholder = 'Please select Y statistic',
                                       onInitialize = I('function() { this.setValue(""); }')
                                 ))
            ),
            mainPanel(
                  tags$head(
                        tags$style(HTML("
                                        .shiny-output-error-validation {
                                        color: green;
                                        }
                                        "))
                        ),
                  tabsetPanel(
                        tabPanel("Rosters",
                                 hr(),
                                 textOutput("text1"),
                                 tags$br(),
                                 tags$b("Here is the roster of the top team you picked:"),
                                 tableOutput("histRoster"),
                                 hr(),
                                 tags$b(p("Here is the roster for the randomly generated team:")),
                                 tags$span(style="color:green", textOutput("noRandTeam")),
                                 tableOutput("userRoster")), 
                        tabPanel("Head to Head", 
                                 hr(),
                                 tags$b(p("If these two teams were to go head to head in a season, 
                                    the team offensive statistics might look like the table below.  
                                    Because we are comparing single season totals from an
                                    historical team and career totals from a randomly generated team, 
                                    the randomly picked team's statisitics have been standardized 
                                    to the same number of 'at bats' of the historical team for
                                    easier comparison.")),
                                 tableOutput("tbl1")), 
                        tabPanel("Plot", 
                                 hr(),
                                 h5("How do the players in each starting line up compare?"),
                                 tags$span(style="color:green", textOutput("plotError")),
                                 plotOutput("plot1",click = "plot_click")),
                        tabPanel("About", 
                                    hr(),
                                    h5("About this app"),
                                 p("This application was inspired by an online article, 'Bleacher Report's
                                    Official Rankings of the 50 Greatest Teams in MLB History' by
                                   Joel Reuter (http://bleacherreport.com/articles/1995178-bleacher-
                                   reports-official-rankings-of-the-50-greatest-teams-in-mlb-history/).
                                   As a fan of baseball - a game that surely inspires number crunching 
                                   in its fans - I wanted to compare some of the greatest teams to 
                                   have played the game in real life versus a team of Hall of Famers."),
                                 tags$b("I fully recognize that I've taken a big chance on this particular 
                                        project given the short time frame provided by the Coursera 
                                        class I'm taking, but I'm also hoping that any fellow students 
                                        could appreciate wanting to make use of some data that had more 
                                        personal meaning than one of the very standard datasets available 
                                        to the casual R user."),
                                 tags$br(),
                                 tags$br(),
                                 p("Using readily available data in the Lahman R package, I gathered 
                                   batting data on the members of the Top 10 all time teams.  Additionally, 
                                   I gathered batting data on all players inducted into the Hall of Fame 
                                   (except those that played most of their games as a designated hitter).  
                                   The starting rosters for the historical teams include 1 player at 
                                   each position, including pitcher, selected as those players with the 
                                   most games played at that position during the season.  In order to 
                                   not get too carried away with the selection of players and despite 
                                   the notion that pitchers generally have lower quality batting 
                                   totals, I thought it appropriate to have the pitchers in the batting 
                                   line up in the spirit of the game as it used to be before the 
                                   designated hitter rule came to be."),
                                 p("Once picking an historical team and selecting a random squad of 
                                   Hall of Famers, the starting rosters are presented (though these are 
                                   not batting orders!), the team offensive totals are explored for a 
                                   hypothetical season of games, and the starting rosters of players 
                                   from both teams can be compared graphically on an interactive scatter 
                                   plot of offensive statistics available.  Because batting data from the  
                                   Lahman package are for players by season, all career totals and 
                                    all averages are calculated by the application."),
                                 h4("Thanks, and enjoy!"),
                                 hr(),
                                 p("For application users not as familiar with baseball statistics, 
                                    below is a listing of abbreviations used in this application."),
                                    p("AB = At Bats"),
                                    p("AVG = Batting Average"),
                                    p("BB = Walks"),
                                    p("H = Hits"),
                                    p("HOF = Hall of Fame (year of induction on Rosters)"),
                                    p("HR = Home Runs"),
                                    p("OBP = On Base Percentage"),
                                    p("POS = Position Played"),
                                    p("R = Runs"),
                                    p("SLG = Slugging Percentage"),
                                    p("SO = Strikouts"),
                                    hr(),
                                    p("Additionally, for those less familiar with the game, below is a 
                                      basic description of the calculated averages and percentages 
                                      presented here, all of which were excerpted from Wikipedia entries 
                                      (accessed September 13, 2015)."),
                                    h6("Batting Average"),
                                    p("In baseball, the batting average is defined by the number of hits 
                                    divided by at bats. It is usually reported to three decimal places and 
                                    pronounced as if it were multiplied by 1,000: a player with a batting 
                                    average of .300 is 'batting three-hundred.' A point (or percentage point) 
                                    is understood to be .001 . If necessary to break ties, batting averages 
                                    could be taken to more than three decimal places."),
                                    h6("Slugging Percentage"),
                                    p("In baseball statistics, slugging percentage is a popular measure of 
                                    the power of a hitter. It is calculated as total bases divided by at bats: 
                                    SLG = (1*1B + 2*2B + 3*3B + 4*HR) / AB where AB is the number of at-bats 
                                    for a given player, and 1B, 2B, 3B, and HR are the number of singles, 
                                    doubles, triples, and home runs, respectively. Walks are specifically 
                                    excluded from this calculation. The name is a misnomer, as the statistic 
                                    is not a percentage but a scale of measure whose computed value is a 
                                    rational number in the interval 0 to 4."),
                                    h6("On Base Percentage"),
                                    p("On Base Percentage is a measure of how often a batter reaches base. 
                                    It is approximately equal to Times on Base/Plate appearances. The full 
                                    formula is OBP = (Hits + Walks + Hit by Pitch) / (At Bats + Walks + Hit 
                                    by Pitch + Sacrifice Flies)."),
                                    p("NOTE: Sacrifice flies were not counted as an official statistic until 
                                      1954. Before that time, all sacrifices were counted as sacrifice hits 
                                      (SH), which included both sacrifice flies and bunts. Sacrifice bunts 
                                      (sacrifice hits since 1954), which would lower a batter's on-base 
                                      percentage, are not included in the calculation for on-base percentage, 
                                      as bunting is an offensive strategy - often dictated by the manager - 
                                      the use of which does not necessarily reflect on the batter's ability 
                                      and should not be used to penalize him. For calculations of OBP 
                                      before 1954, or where sacrifice flies are not explicitly listed, the 
                                      number of sacrifice flies should be assumed to be zero."))
                        
))))
