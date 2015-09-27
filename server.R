shinyServer(
      function(input, output) {
            ############### SIDEBAR ITEMS ###############
            #Choose a HOF team at random -- 1 player at each position (3 outfielders total)
            #---though outfielders can be coded by field (POS=RF|CF|LF) stats are available 
            #---outfielders will be grouped together
            getRandom <- eventReactive( input$goButton, {
                  validate( 
                        need(input$goButton != 0, "Please generate a HOF team")
                  )
                  df <- rbind(
                        hofBattingCareerMaster %>%
                              filter(POS!="OF") %>%
                              group_by(POS) %>%
                              sample_n(size=1),
                        hofBattingCareerMaster %>%
                              filter(POS=="OF") %>%
                              group_by(POS) %>%
                              sample_n(size=3))
                  df <- ungroup(df)
                  return(df)
            })
            getHist <- reactive( {
                  validate( #If the user hasn't selected a team yet, display text
                        need(input$topteam != "", "Please select an historical team")
                  )
                  #Team choices (in rank order)
                  if (input$topteam==1) {teamChoice <- getTeam(1927,"NYA")}
                  else if (input$topteam==2) {teamChoice <- getTeam(1939,"NYA")}
                  else if (input$topteam==3) {teamChoice <- getTeam(1975,"CIN")}
                  else if (input$topteam==4) {teamChoice <- getTeam(1998,"NYA")}
                  else if (input$topteam==5) {teamChoice <- getTeam(1929,"PHA")}
                  else if (input$topteam==6) {teamChoice <- getTeam(1961,"NYA")}
                  else if (input$topteam==7) {teamChoice <- getTeam(1902,"PIT")}
                  else if (input$topteam==8) {teamChoice <- getTeam(1970,"BAL")}
                  else if (input$topteam==9) {teamChoice <- getTeam(1984,"DET")}
                  else if (input$topteam==10) {teamChoice <- getTeam(1907,"CHN")}
                  #To present a roster, trim team to 9 players based on position
                  df <- rbind(
                        teamChoice %>%
                              filter(POS!="OF") %>%
                              group_by(POS) %>%
                              filter(AB==max(AB)),
                        teamChoice %>%
                              filter(POS=="OF") %>%
                              group_by(POS) %>%
                              arrange(desc(AB)) %>%
                              head(3)
                  )
                  df <- ungroup(df)
                  return(df)
            })
            ############### ROSTER TAB IN MAIN WINDOW ###############
            output$text1 <- renderText({ 
                  paste("You have selected historical team ranked #", input$topteam)
            })
            output$histRoster <- renderTable({ 
                  histTeam <- getHist()
                  colnames(histTeam)[19] <- "Player (HOF)"
                  histTeam[,c(19,16:18,26)]
            })
            output$userRoster <- renderTable({ 
                  rndTeam <- getRandom()
                  colnames(rndTeam)[17] <- "Player (HOF)"
                  rndTeam <- rndTeam[,c(17,14:16,24)]
            })
            ############### HEAD TO HEAD TAB IN MAIN WINDOW ###############
            output$tbl1 <- renderTable({ 
                  if (input$topteam==1) {histChoice <- getTeam(1927,"NYA")}
                  else if (input$topteam==2) {histChoice <- getTeam(1939,"NYA")}
                  else if (input$topteam==3) {histChoice <- getTeam(1975,"CIN")}
                  else if (input$topteam==4) {histChoice <- getTeam(1998,"NYA")}
                  else if (input$topteam==5) {histChoice <- getTeam(1929,"PHA")}
                  else if (input$topteam==6) {histChoice <- getTeam(1961,"NYA")}
                  else if (input$topteam==7) {histChoice <- getTeam(1902,"PIT")}
                  else if (input$topteam==8) {histChoice <- getTeam(1970,"BAL")}
                  else if (input$topteam==9) {histChoice <- getTeam(1984,"DET")}
                  else if (input$topteam==10) {histChoice <- getTeam(1907,"CHN")}

                  randChoice <- getRandom()
                  histChoice<-histChoice[,-c(2:3)] #drop the year and team ID for rbind below
                  
                  #Sum up the stats for each team
                  histSummed <- sumStats(x=histChoice)
                  randSummed <- sumStats(x=randChoice)
 
                  #Create a data set that combines the user's team and the historical (comparison) team
                  head2head<-rbind(histSummed,randSummed)
                  head2head <- head2head[c(1:3,12,4:11,13:15)] #Order the columns
                  
                  #Since the historical team stats are already for a single and the user's team stats
                  #reflect the career totals of the players chosen, we need to even the playing field.
                  #Counted totals will be divided by the head to head ratio of AB.  The calculated stats
                  #will remain as calculated by career totals (they are already comparable). This will
                  #produce comparable numbers for a real season of an historical team vs. a fictional 
                  #season using the average statisitics of a team created by the user.
                  hhRatio <- as.numeric(head2head[2,1]/head2head[1,1])
                  head2head[2,1:12] <- lapply(head2head[2,1:12]/hhRatio, round)
                  head2head[2,1:12] <- lapply(head2head[2,1:12], as.character)
                  head2head$Team <- c("Historical","Random HOF")
                  head2head[,c(16,1:3,7:9,13:15)]#Present only certain columns
            })
            ############### PLOT TAB IN MAIN WINDOW ###############
            output$plot1 <- renderPlot({ 
                  validate( #If the user hasn't selected their stats yet, display text
                        need(input$plotx != "", "Please select your X-axis statistic"),
                        need(input$ploty != "", "Please select your Y-axis statistic")
                  )
                  histChoice <- getHist()
                  randChoice <- getRandom()
                  histChoice2<-semi_join(histBattingCareerMaster,histChoice,by="playerID")
                  histChoice2$Team<-"Historical"
                  randChoice$Team<-"Random HOF"
                  allChoice<-rbind(histChoice2,randChoice)
                  allChoice$x<-allChoice[input$plotx]
                  allChoice$y<-allChoice[input$ploty]

                  if (input$plotx==input$ploty) {
                        return("Choose a different statistic")
                  }
                  ##Create a base plot; build up in layers
                  g <- ggplot(allChoice,aes(x=unlist(x),y=unlist(y),col=Team)) +
                        geom_point(data=allChoice,alpha=.8,size=8) +
                        #make_text(data=allChoice,x=unlist(x),y=unlist(y),
                        #           showSelected=playerID,showSelected2=HR) +
                        theme_bw(base_family = "Helvetica", base_size = 12) +
                        theme(legend.position = "right") +
                        labs(x = getLabel(input$plotx)) +
                        labs(y = getLabel(input$ploty)) +
                        labs(title = paste("Career Offensive Statistics for Each Starting Nine")) 
                  ##Print the plot
                  print(g)
            })
            output$plotError <- renderText({ 
                  if (input$plotx==input$ploty & input$plotx != "") {
                        paste("Please select a different statistic for 
                              X or Y (you are trying to plot", getLabel(input$plotx),
                              "against itself)!")
                  }
            })
            #If the user hasn't selected a team yet, display text
            output$noRandTeam <- renderText({ 
                  if (input$goButton == 0) {
                        paste("Please randomly assign a team using the action button.")
                  }
            })
      }
)
