library(shiny)
library(ggplot2)
library(ggthemes)
library(deSolve)
library(gridExtra)


shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    rm(list=ls())     #load packages ggplot2,ggthemes,desolve,gridExtra
    SIR <- function (times,x,parms) {
      SL = x[1]
      IL = x[2]
      RL = x[3]
      NL = x[4]
      SS = x[5]
      IS = x[6]
      RS = x[7]
      NS = x[8]
      if (IL<1) IL=0
      if (IS<1) IS=0
      #BetaL large predator intraspecific transmition Rate
      with(as.list(c(x,parms)), {
        dSL <- -(betaL*SL*IL/NL+betaSL*SL*IS/NL) - (b*SL)
        dIL <- +(betaL*SL*IL/NL+betaSL*SL*IS/NL) - (((gamma + b)/(1-rhoL))*IL) #- gamma*IL - (0.07)*IL   #(b+) 0.015 is the additional mortality due to the infection
        dRL <- +gamma*IL - b*RL
        dNL <- +dSL + dIL + dRL
        dSS <- -(betaS*SS*IS/NS) - ((b+(igp*NL/kl))*SS) #25Aug16 igp decreases as NL decreases; max IGP ~carrying capacity
        dIS <- +(betaS*SS*IS/NS) - (((gamma + b)/(1-rhoS))*IS) #- gamma*IS - (0.07)*IS   #(add igp) 0.015 is the additional mortality due to the infection
        dRS <- +gamma*IS - ((b+(igp*NL/kl))*RS)
        dNS <- +dSS + dIS + dRS
        out <- c(dSL,dIL,dRL,dNL,dSS,dIS,dRS,dNS)
        list(out)
      })
    }
    eventfun <- function(t, y, parms){
      with (as.list(c(y,parms)),{
        if ((SL+RL) > 2){
          SL <- SL + (((a)*365)*(1-(NL/kl))*((SL+RL)/2)*round(cos(2%%((t%%365)+2))))#((((a*12)*round(cos(2%%((t%%12)+2))))-((.5*((a*12)-(b*12))*NL/kl)*round(cos(2%%((t%%12)+2)))))*(SL+RL))   # birth pulse
        }
        NL <- SL + IL + RL
        if ((SS+RS) > 1){
          SS <- SS + (((a)*365)*(1-(NS/ks))*((SS+RS)/2)*round(cos(2%%((t%%365)+2))))#((((a*12)*round(cos(2%%((t%%12)+2))))-((.5*((a*12)-(b*12))*NS/ks)*round(cos(2%%((t%%12)+2)))))*(SS+RS))   # birth pulse
        }
        NS <- SS + IS + RS
        #if (IL<1) IL=0
        #if (IS<1) IS=0
        return(c(SL,IL,RL,NL,SS,IS,RS,NS))
      })
    }
    times <- seq(1,20000, by = 1) # daily time step
    parameters <- c(betaL = input$betaL, betaS = input$betaS, betaSL = input$betaSL, gamma = input$gamma, a= input$a, b = input$b, igp = input$igp, kl = input$kl,ks = input$ks, rhoL = input$rhoL, rhoS = input$rhoS)#beta SL 0.02; igp0011041; igp 0.00543//.000362 to .000452;5Jun16a = .00531435 betas .56 to .14, gamma .3 to .07#a=.0765, b= 0.0543, igp=0.00125, betaSL 0.05
    init <- c(SL=49,IL=1,RL=0,NL=50,SS=499,IS=1,RS=0,NS=500)
    out <- as.data.frame(ode(y = init, times = times, func = SIR, parms = parameters, events=list(func=eventfun,time=times)))
    mydata1 <- data.frame(Period=rep((1:length(out$SL)),4),Population =  c(round(out$SL),round(out$IL),round(out$RL),round(out$NL)),
                          Indicator=rep(c("SusceptibleL","InfectedL","RecoveredL","TotalL"),each=length(out$SL)))
    mydata2 <- data.frame(Period=rep((1:length(out$SS)),4),Population =  c(round(out$SS),round(out$IS),round(out$RS),round(out$NS)),#format(round(out$IS,2),nsmall = 2)
                          Indicator=rep(c("SusceptibleS","InfectedS","RecoveredS","TotalS"),each=length(out$SS)))
    mydata3 <- data.frame(PopulationS =  round(out$NS),PopulationL =  round(out$NL))
              
    
    p1 = ggplot(mydata1,aes(x=Period,y=Population, group=Indicator))
    f1 = p1+geom_line(aes(colour = Indicator)) + xlab("Time (days)")+ylab("Intraguild Predator \n population") + theme_hc(base_size=10)+ theme(legend.position = "right",legend.title=element_blank())+ggtitle("Disease dynamics \n\n Spillover, vhigh intraguild predation \n") + theme(plot.title=element_text(face="bold",size=10)) #+ggtitle("Population dynamics:\n Disease cannot be sustained in large predator species alone")#+theme(plot.title = element_text(size=10, vjust=2))# "high intraguild predation,with infection spillover\n"
    p2 = ggplot(mydata2,aes(x=Period,y=Population, group=Indicator)) 
    f2 = p2+geom_line(aes(colour = Indicator)) + xlab("Time (days)")+ylab ("Intraguild Prey \n population")  + theme_hc(base_size=10) + theme(legend.position = "right",legend.title=element_blank()) #+ ggtitle("Disease dynamics in small predator population:\n No intraguild predation")
    f3 = ggplot(mydata3, aes(x = PopulationS, y = PopulationL)) + geom_line()+ theme_hc(base_size=10) + theme(legend.position = "right",legend.title=element_blank())
    grid.arrange(f1,f2,f3,ncol=1)
  })
})