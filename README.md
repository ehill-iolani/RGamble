# Bioinfo Blackjack!

It's blackjack, duh. Top left button is where you put your bet amount, top right is 
to start game. If you run outta credits refresh your browser (or do something
better with your time).

Merry Chroistmas!

# Installation

pull the repo

```
git clone https://github.com/bobwaft/RGamble
cd RGamble
R
library(shiny)
library(bslib)
runApp("app.R")
```

Docker image

```
git clone https://github.com/ehill-iolani/RGamble.git
cd RGamble
docker build -t blackjack:ghetto .
docker run -dt --rm -p 3838:3838 blackjack:ghetto .
```