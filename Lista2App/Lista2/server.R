library(ggplot2)
library(shiny)
source("funcoes.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
        
  v <- reactiveValues(data = NULL)              
 
  #Eventos  
   observeEvent(input$calcProp,{
           v$data <- sprintf("p^ = %.3f", input$k/input$nProp)
   })

   observeEvent(input$calcMed,{
           v$data <- sprintf("μ^ = %.3f", input$mu)
   })
  
  observeEvent(input$limpProp, {
          v$data <- NULL
   })
  
  observeEvent(input$limpMed, {
          v$data <- NULL
  })
  
  observeEvent(input$calcVar, {
          v$data <- sprintf("sigma^ = %.3f",
                            ((input$nVar-1)/input$nVar)*(input$sd^2))
  })
  
  observeEvent(input$limpVar, {
          v$data <- NULL
  })
  

  
  #Saídas
  output$resProp <- renderText({
      if (is.null(v$data)) return()
      v$data
  })
  
  output$resMed <- renderText({
          if (is.null(v$data))  return()
          v$data
  })
 
  output$resVar <- renderText({
          if (is.null(v$data)) return()
          v$data
  })
  
  output$nCurva <- renderPlot({
          resultado <- normalCurve(xbarra = input$xNorm, variancia = input$s2Norm
                                   ,n = input$nNorm, ic = input$nIC)
          x <- c(-3,3)
          y <- dnorm(x)
          
          alfa <- (100 - input$nIC)/100
          
          lInf <- resultado[1]
          lSup <- resultado[2]
          
          zinf <- qnorm(alfa/2)
          zsup <- qnorm(1-alfa/2)
          
          xy <- data.frame(x = x, y = y)
          
          g <- ggplot(data = data.frame(x), aes(x=x))
          
          g <- g + stat_function(fun = dnorm, n = 10000, args = list(mean = 0, sd = 1))
          
          
          g <- g + geom_segment(data = xy, aes(x = zinf, y = 0, xend = zinf, yend = dnorm(zinf)))
          
          g <- g + geom_label(aes(x=zinf,y=0), label = toString(round(lInf,2)))
          
          g <- g + geom_label(aes(x=zsup,y=0), label = toString(round(lSup,2)))
          
          
          g <- g + geom_segment(data = xy, aes(x = zsup, y = 0, xend = zsup, yend = dnorm(zsup)))
          
          g <- g + scale_y_continuous(breaks = NULL) + ylab("") + 
                  scale_x_continuous(breaks = NULL) + xlab(paste("IC", 
                                                                 toString(percent((1-alfa)))))
          
          g <- g + stat_function(fun= dnorm, n = 10000, args = list(0,1),
                                 geom = "area", xlim = c(zinf,zsup), fill = "red", alpha = 0.5)
          g
          
  })
  
  output$tsCurva <- renderPlot({
          resultado <- tsCurve(xbarra = input$xTStud, variancia = input$s2TStud,
                               n = input$nTStud,ic = input$tIC)
          gl <- input$nTStud - 1 
          x <- c(-3,3)
          y <- qt(pt(x,df=gl), df = gl)
          
          alfa <- (100 - input$tIC)/100
          
          lInf <- resultado[1]
          lSup <- resultado[2]
          
          tInf <- qt(alfa/2,df = gl)
          tSup <- qt(1-alfa/2,df = gl)
          
          xy <- data.frame(x = x, y = y)
          
          g <- ggplot(data = data.frame(x), aes(x=x))
          
          g <- g + stat_function(fun = dt, args = list(df = gl))
          
          
          g <- g + geom_segment(data = xy, aes(x = tInf, y = 0, xend = tInf, yend = dt(x = tInf, df = gl)))
          
          g <- g + geom_label(aes(x=tInf,y=0), label = toString(round(lInf,2)))
          
          g <- g + geom_label(aes(x=tSup,y=0), label = toString(round(lSup,2)))
          
          
          g <- g + geom_segment(data = xy, aes(x = tSup, y = 0, xend = tSup, yend = dt(x = tSup,
                                                                                       df = gl)))
          
          g <- g + scale_y_continuous(breaks = NULL) + ylab("") + 
                  scale_x_continuous(breaks = NULL) + xlab(paste("IC", 
                                                                 toString(percent((1-alfa)))))
          
          g <- g + stat_function(fun= dt, args = list(df = gl),
                                 geom = "area", xlim = c(tInf,tSup), fill = "blue", alpha = 0.5)
          g
  })
  
  output$quiCurva <- renderPlot({
          resultado <- quiCurve(variancia = input$s2Qui,n = input$nQui, ic = input$quiIC)
          
          alfa <- (100 - input$quiIC)/100
          
          gl <- input$nQui - 1
          
          lInf <- resultado[1]
          lSup <- resultado[2]
          
          quiInf <- qchisq(1-(alfa/2),df = gl)
          quiSup <- qchisq((alfa/2),df = gl)
          
          x = c(0,30)
          
          g <- ggplot(data = data.frame(x), aes(x=x))
          
          g <- g + stat_function(fun = dchisq, args = list(df = gl)) 
          
          
          g <- g + geom_segment(data = data.frame(x), aes(x = quiInf, y = 0, xend = quiInf, yend = dchisq(x = quiInf, df = gl)))
          
          g <- g + geom_label(aes(x=quiInf,y=0), label = toString(round(lSup,2)))
          
          g <- g + geom_label(aes(x=quiSup,y=0), label = toString(round(lInf,2)))
          
          
          g <- g + geom_segment(data = data.frame(x), aes(x = quiSup, y = 0, xend = quiSup, yend = dchisq(x = quiSup,
                                                                                       df = gl)))
          
          g <- g + scale_y_continuous(breaks = NULL) + ylab("") + 
                  scale_x_continuous(breaks = NULL) + xlab(paste("IC", 
                                                                 toString(percent((1-alfa)))))
          
          g <- g + stat_function(fun= dchisq, args = list(df = gl),
                                 geom = "area", xlim = c(quiInf,quiSup), fill = "green", alpha = 0.5)
          g
  })
  
  output$fCurva <- renderPlot({
          resultado <- fCurve(n1 = input$n1F,s1 = input$s21F, n2 = input$n2F, s2 = input$s22F, ic = input$fIC)
          
          alfa <- (100 - input$fIC)/100
          
          lSup <- resultado[2]
          lInf <- resultado[1]
          
          gl1 <- input$n1F - 1
          gl2 <- input$n2F - 1
          
          fInf <- qf(1-(alfa/2),df1 = gl1, df2 = gl2)
          fSup <- qf((alfa/2), df1 = gl1, df2 = gl2)
          
          x = c(0,4)
          
          g <- ggplot(data = data.frame(x), aes(x=x))
          
          g <- g + stat_function(fun = df, args = list(df1 = gl1, df2 = gl2)) 
          
          
          g <- g + geom_segment(data = data.frame(x), aes(x = fInf, y = 0, xend = fInf, 
                                                          yend = df(x = fInf, df1 = gl1, df2 = gl2)))
          
          g <- g + geom_label(aes(x=fInf,y=0), label = toString(round(lSup,2)))
          
          
          g <- g + geom_segment(data = data.frame(x), aes(x = fSup, y = 0, xend = fSup, 
                                                          yend = df(x = fSup, df1 = gl1, df2 = gl2)))
          
          g <- g + geom_label(aes(x=fSup,y=0), label = toString(round(lInf,2)))
          
          g <- g + scale_y_continuous(breaks = NULL) + ylab("") + 
                  scale_x_continuous(breaks = NULL) + xlab(paste("IC", 
                                                                 toString(percent((1-alfa)))))
          
          g <- g + stat_function(fun= df, args = list(df1 = gl1, df2 = gl2),
                                 geom = "area", xlim = c(fInf,fSup), fill = "yellow", alpha = 0.7)
          g
  })
  
})
