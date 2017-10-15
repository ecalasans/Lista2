
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
        dashboardHeader(title = "Lista 2 - TEEC"),
        dashboardSidebar(
                menuItem("Intervalo de Confiança", tabName = "ic",
                         menuSubItem("Normal", tabName = "norm"),
                         menuSubItem("t-Student", tabName = "tstud"),
                         menuSubItem("χ^2", tabName = "qui"),
                         menuSubItem("F", tabName = "distF"))
        ),
        dashboardBody(
                tabItems(
                        tabItem(tabName = "norm",
                                box(
                                        width = 6,
                                        h2("Intervalo de Confiança(IC) para a Média Populacional com Variância Conhecida"),
                                        withMathJax(
                                                p("O IC para a média de uma população com distribuição Normal
                                                  dada por:$$N(\\mu,\\sigma^{2})$$ com variância populacional conhecida e 
                                                  tamanho da amostra grande(n>30) utiliza a DISTRIBUIÇÃO NORMAL e
                                                  é dado por: $$\\bar{x} - Z_{1-\\frac{\\alpha}{2}}*\\frac{s^2}{\\sqrt(n)}
                                                  \\leq \\mu \\leq \\bar{x} + Z_{1-\\frac{\\alpha}{2}}*\\frac{s}{\\sqrt(n)}$$
                                                  onde:")
                                                ),
                                        numericInput(inputId = "nNorm", label = "n = Tamanho da amostra:  ", 
                                                     value = 30, width = '250px', min = 30),
                                        numericInput(inputId = "xNorm", label = "x_barra = Média da amostra:  ", 
                                                     value = 5, width = '250px'),
                                        numericInput(inputId = "s2Norm", label = "s^2 = Variância da amostra:  ", 
                                                     value = 10, width = '250px', min = 30),
                                        sliderInput(inputId = "nIC", label = "1-alfa = Intervalo de confiança",
                                                    min = 90, max = 100, step = 0.5, value = 95)
                                        ),
                                box(
                                        width = 6,
                                        withMathJax(
                                                h4("$$\\mu$$")
                                        ),
                                        plotOutput("nCurva")
                                )
                        ),
                        tabItem(tabName = "tstud",
                                box(
                                        width = 6,
                                        h2("Intervalo de Confiança(IC) para a Média Populacional com Variância Desconhecida"),
                                        withMathJax(
                                                p("O IC para a média de uma população com distribuição Normal
                                                  dada por:$$N(\\mu,\\sigma^{2})$$ com variância desconhecida e amostras 
                                                  consideradas pequenas(n<30) utiliza a DISTRIBUIÇÃO t-STUDENT com n-1 
                                                  graus de liberdade e
                                                  é dado por: $$\\bar{x} - T_{1-\\frac{\\alpha}{2},n-1}*\\frac{s^2}{\\sqrt(n)}
                                                  \\leq \\mu \\leq \\bar{x} + T_{1-\\frac{\\alpha}{2},n-1}*\\frac{s}{\\sqrt(n)}$$
                                                  onde:")
                                                ),
                                        numericInput(inputId = "nTStud", label = "n = Tamanho da amostra:  ", 
                                                     value = 30, width = '250px', min = 0, max = 30),
                                        numericInput(inputId = "xTStud", label = "x_barra = Média da amostra:  ", 
                                                     value = 5, width = '250px'),
                                        numericInput(inputId = "s2TStud", label = "s^2 = Variância da amostra:  ", 
                                                     value = 10, width = '250px'),
                                        sliderInput(inputId = "tIC", label = "1-alfa = Intervalo de confiança",
                                                    min = 90, max = 100, step = 0.5, value = 95)
                                                ),
                                box(
                                        withMathJax(
                                                h4("$$\\mu$$")
                                        ),
                                        width = 6,
                                        plotOutput("tsCurva")
                                )
                        ),
                        tabItem(tabName = "qui",
                                box(
                                        width = 6,
                                        h2("Intervalo de Confiança(IC) para a Variância Populacional"),
                                        withMathJax(
                                                p("O IC para a variância de uma população com distribuição Normal
                                                  dada por:$$N(\\mu,\\sigma^{2})$$ utiliza a DISTRIBUIÇÃO QUI-QUADRADA com n-1 
                                                  graus de liberdade e
                                                  é dado por: $$\\frac{(n-1)*S^{2}}{\\sigma^{2}} = 
                                                  \\frac{\\sum{}{}(X_{i} - \\bar{X})^{2}}{\\sigma^{2}}$$
                                                  onde:")
                                        ),
                                        numericInput(inputId = "nQui", label = "n = Tamanho da amostra:  ", 
                                                     value = 10, width = '250px', min = 0),
                                        numericInput(inputId = "s2Qui", label = "s^2 = Variância da amostra:  ", 
                                                     value = 10, width = '250px', min = 0),
                                        sliderInput(inputId = "quiIC", label = "1-alfa = Intervalo de confiança",
                                                    min = 90, max = 100, step = 0.5, value = 95)
                                ),
                                box(
                                        width = 6,
                                        withMathJax(
                                                h4("$$\\sigma^{2}$$")
                                        ),
                                        plotOutput("quiCurva")
                                )
                        ),
                        tabItem(tabName = "distF",
                                box(
                                        width = 6,
                                        h2("Intervalo de Confiança(IC) para a Razão de Variâncias de Duas Populações"),
                                        withMathJax(
                                                p("O IC para a razão de variâncias de duas populações com distribuição Normal
                                                  dada por:$$N(\\mu,\\sigma^{2})$$ utiliza a DISTRIBUIÇÃO F com gl1 = m-1 
                                                   e gl2 = n-1 graus de liberdade,
                                                  é dado por: $$F = 
                                                  \\frac{S_{1}^{2}/gl_{1}}{S^{2}_{2}/gl_{2}}$$
                                                  onde:")
                                                ),
                                        numericInput(inputId = "n1F", label = "n = Tamanho da primeira amostra:  ", 
                                                     value = 10, width = '250px', min = 0),
                                        numericInput(inputId = "s21F", label = "S1^2 = Variância da primeira amostra:  ", 
                                                     value = 10, width = '250px', min = 0),
                                        numericInput(inputId = "n2F", label = "m = Tamanho da seguna amostra:  ", 
                                                     value = 10, width = '250px', min = 0),
                                        numericInput(inputId = "s22F", label = "S2^2 = Variância da segunda amostra:  ", 
                                                     value = 15, width = '250px', min = 0),
                                        sliderInput(inputId = "fIC", label = "1-alfa = Intervalo de confiança",
                                                    min = 90, max = 100, step = 0.5, value = 95)
                                                ),
                                box(
                                        width = 6,
                                        withMathJax(
                                              h4("$$\\frac{\\sigma^{2}_{1}}{\\sigma^{2}_{2}}$$")  
                                        ),
                                        plotOutput("fCurva")
                                )
                                )
                        )        
                )
        )
)

