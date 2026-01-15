library(shiny)
library(bslib)
library(magick)
library(stringr)

staticDeck = c("AC","KC","QC","JC","10C","9C","8C","7C","6C","5C","4C","3C","2C","AD","KD",
    "QD","JD","10D","9D","8D","7D","6D","5D","4D","3D",
    "2D","AS","KS","QS","JS","10S","9S","8S","7S","6S",
    "5S","4S","3S","2S","AH","KH","QH","JH","10H","9H","8H","7H","6H","5H","4H","3H","2H")


ui = fluidPage(
    title = "RGamble",
    tags$style(HTML("
        body {
            background-color: green;
            flex-direction: column;
            display: flex;
            align-items: center;
            justify-content: center;
        }

        .blackjackbutton {
            background-color: #0a5555;
            width: 300px;
            height: 72.036px;
            border: 1px;
            border-radius: 500px;
            font-size: 50px;
            color: #ffffff;
            margin: 50px;
            margin-bottom: 10px; margin-top: 10px;
        }
        
        .blackjackbutton:hover {
            background-color: #3d8462;
            color: #ffffff
        }

        #betAmt {
            background-color: #3d8462;
            width: 300px;
            height: 72.036px;
            border: 1px;
            border-radius: 500px;
            font-size: 50px;
            color: #ffffff;
            margin: 0px;
            text-align: center;
            margin-bottom: 0px; margin-top: 0px;
        }

        button:hover {
            background-color: #3d8462;
            cursor: pointer;
        }


        div {
            font-size: 50px;
            color: white;
            align-items: center;
            justify-content: center;
            margin: 50px;
            
        }

        img {
            margin: 30px
        }

        .cardRow {
            display: flex;
            flex-wrap: wrap;
            width: 555px
        }
        

        h1 {
            color: #FFFFFF;
            text-align: center;
            font-size: 100px;
        }

        h2 {
            width: 99%;
            text-align: center;
            font-size: 50px;
            color: #FFFFFF;
        }
        
        p {
            text-align: center;
        }

        .shiny-output-error { visibility: hidden; }
        .shiny-output-error-validation { visibility: hidden; }
        .form-group.shiny-input-container {margin: 50px;  margin-bottom: 0px; margin-top: 0px;}
        .shiny-text-output.shiny-bound-output {margin-bottom: 0px; margin-top: 10px;}
        "
    )),
    div(style="position:absolute; display: flex; align-items:right; 
        width: 95%; left: 0px; top: -50px; justify-content: right",
        textOutput("credits")   
    ),
    div(title = "title",p("Bioinfo Blackjack")),
    div(style = "display: flex; justify-content: center; margin-bottom: 0px; margin-top: 0px;",
        numericInput("betAmt",label=NULL,value=0,width="300"),
        actionButton("bet",label="Place Bet",width="300",class="blackjackbutton"),
    ),
    div(style = "display: flex; justify-content: center;  margin-bottom: 0px; margin-top: 0px;",
        actionButton("hit",label="hit",width="300",class="blackjackbutton"),
        actionButton("stand",label="stand",width="300",class="blackjackbutton"),
    ),
    div(style = "display: flex; justify-content: space-between; margin-top: 0px; margin-bottom: 0px",
        textOutput("playerTitle"),
        textOutput("dealerTitle")
    ),
    div(style = "display: flex; flex-direction: row;  margin-bottom: 0px; margin-top: 0px;",
        div(class = "cardRow",
        imageOutput("p1",inline=TRUE),
        imageOutput("p2",inline=TRUE),
        imageOutput("p3",inline=TRUE),
        imageOutput("p4",inline=TRUE),
        imageOutput("p5",inline=TRUE),
        imageOutput("p6",inline=TRUE),
        ),
        div(class = "cardRow",
        imageOutput("d1",inline=TRUE),
        imageOutput("d2",inline=TRUE),
        imageOutput("d3",inline=TRUE),
        imageOutput("d4",inline=TRUE),
        imageOutput("d5",inline=TRUE),
        imageOutput("d6",inline=TRUE)
        )
    )
)

server = function(input,output) {

    dealerCards = c("d1","d2","d3","d4","d5","d6")
    playerCards = c("p1","p2","p3","p4","p5","p6")
    allCards = c(dealerCards,playerCards)

    rvs = reactiveValues(
        deck = staticDeck,
        player = vector(),
        dealer = vector(),
        credits = 1000,
        playing = FALSE,
        hitting = FALSE,
        dealerTurn = FALSE,
        gameOverWait = FALSE,
        betAmt = 0
    )

    #HELPER FUNCTIONSSS

    updateCard = function(cardElement,cardVal) {
        switch(
            EXPR = cardElement,
            d1 = (output$d1 = renderCard(cardVal)),
            d2 = (output$d2 = renderCard(cardVal)),
            d3 = (output$d3 = renderCard(cardVal)),
            d4 = (output$d4 = renderCard(cardVal)),
            d5 = (output$d5 = renderCard(cardVal)),
            d6 = (output$d6 = renderCard(cardVal)),
            p1 = (output$p1 = renderCard(cardVal)),
            p2 = (output$p2 = renderCard(cardVal)),
            p3 = (output$p3 = renderCard(cardVal)),
            p4 = (output$p4 = renderCard(cardVal)),
            p5 = (output$p5 = renderCard(cardVal)),
            p6 = (output$p6 = renderCard(cardVal)),
        )
    }

    renderCard = function(cardVal) {
        if (cardVal == "") return(renderImage({NULL},deleteFile=FALSE))
        filePath = paste("./assets/",cardVal,".png",sep="")
        tempList = list(src = filePath,width=125,height=181,contentType="image/png")
        return(renderImage({tempList},deleteFile=FALSE))
    }

    updatePlayerText = function(playerVal = 0) {
        output$playerTitle = renderText(paste("Player:",ifelse(playerVal == 0,"",playerVal)))
    }

    updateDealerText = function(dealerVal = 0) {
        output$dealerTitle = renderText(paste("Dealer:",ifelse(dealerVal==0,"",dealerVal)))
    }
    
    getHandVal = function(hand) {
        total = 0
        aceNum = 0
        for (i in hand) {
            card = str_remove(i,"[CSHD]")
            if (card %in% c("K","Q","J")) total = total + 10
            else if (card == "A") {
                total = total + 11
                aceNum = aceNum + 1
            }
            else total = total + as.numeric(card)
        }
        if (aceNum > 0 ) {
            for (i in 1:aceNum) {
                if (total > 21) total = total - 10
            }
        }
        return(total)
    }

    drawCard = function(hand,n) {
        randInds = sample(1:length(rvs$deck),n)
        if (hand == "player") rvs$player = c(rvs$player,rvs$deck[randInds])
        else rvs$dealer = c(rvs$dealer,rvs$deck[randInds])
        rvs$deck = rvs$deck[-randInds]
    }

    updatePlayerUI = function() {
        for (i in 1:length(rvs$player)) {
            updateCard(playerCards[i],rvs$player[i])
        }
        updatePlayerText(getHandVal(rvs$player))
    }

    updateDealerUI = function() {
        if (rvs$hitting == TRUE) {
            updateCard(dealerCards[1],rvs$dealer[1])
            updateDealerText(getHandVal(rvs$dealer[1]))
        }
        else {
            for (i in 1:length(rvs$dealer)) {
                updateCard(dealerCards[i],rvs$dealer[i])
            }
            updateDealerText(getHandVal(rvs$dealer))   
        }
    }

    updateCredits = function() {
        output$credits = renderText(paste("Credits:",rvs$credits))
    }

    checkBust = function(hand) {
        return (getHandVal(hand) > 21)
    }
    
    checkWin = function() {
        return(sign(getHandVal(rvs$player)-getHandVal(rvs$dealer)))
    }

    playerStand = function() {
            if (rvs$hitting) {
                rvs$hitting = FALSE
                updateDealerUI() 
                rvs$dealerTurn = TRUE
            }
    }

    #ngl i wrote ts function without chat originally but the wait logic was
    # too confusing at it was 3 am so i chatted it :skull: i get how it work tho

    observe({
        req(rvs$dealerTurn)
        invalidateLater(1000)
        isolate({
            currVal = getHandVal(rvs$dealer)
            if (currVal < 17) {
                drawCard("dealer", 1)
                updateDealerUI()
            } 
            else {
                if (rvs$gameOverWait == FALSE) {
                    rvs$gameOverWait = TRUE
                } 
                else {
                    rvs$dealerTurn = FALSE 
                    rvs$gameOverWait = FALSE
                    if (checkBust(rvs$dealer)) {
                        rvs$credits = rvs$credits + rvs$betAmt
                    } else {
                        rvs$credits = rvs$credits + (checkWin() * rvs$betAmt)
                    }
                    
                    resetGame()
                    rvs$playing = FALSE
                }
            }
        })
    })

    playerHit = function() {
        if (rvs$hitting) {
            drawCard("player",1)
            updatePlayerUI()
            if (checkBust(rvs$player)) {
                rvs$credits = rvs$credits - rvs$betAmt
                rvs$playing = FALSE
                rvs$betting = FALSE
                resetGame()
            }
        }
    }

    gameStart = function() {
        if (!rvs$playing && input$betAmt > 0 && input$betAmt <= rvs$credits){
            rvs$betAmt = input$betAmt
            rvs$playing = TRUE
            rvs$hitting = TRUE
            rvs$betAmt = input$betAmt
            drawCard("player",2)
            updatePlayerUI()
            drawCard("dealer",2)
            updateDealerUI()
            if(getHandVal(rvs$player) == 21) {
                rvs$credits = rvs$credits + rvs$betAmt*1.5
                rvs$playing = FALSE
                rvs$betting = FALSE
                resetGame()
            }
            else if (getHandVal(rvs$dealer) == 21) {
                rvs$credits = rvs$credits - rvs$betAmt
                rvs$playing = FALSE
                rvs$betting = FALSE
                resetGame()
            }
        }
    }

    resetGame = function() {
        updateCredits()
        rvs$dealer = vector()
        rvs$player = vector()
        rvs$deck = staticDeck
        for (i in allCards) {
            updateCard(i,"")
        }
        updateDealerText()
        updatePlayerText()
        for (i in c(playerCards[1:2],dealerCards[1:2])) {
            updateCard(i,"card back")
        }
    }


    
    # Game Loop
    observeEvent(input$bet,{
        gameStart()
    })
    
    observeEvent(input$hit,{
        playerHit()
    })

    observeEvent(input$stand,{
        playerStand()
    })
    resetGame()
}

shinyApp(ui = ui, server = server)

