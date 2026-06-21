# Load packages ----------------------------------------------------------------
#library(shiny)


# https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html
# Run the shinylive::export line to populate the docs folder 
# so that shinylive works from github
#shinylive::export(appdir = "../DistributionProbabilityCalculator/", destdir = "docs")
#httpuv::runStaticServer("docs/", port = 8008)


# Functions
chiTail <-
  function(U=NULL, df = 10, curveColor=1, border=1, col="#569BBD", xlim=NULL, ylim=NULL, xlab='', ylab='', detail=999){
    temp <- diff(range(xlim))
    x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
    y    <- dchisq(x, df)
    ylim <- range(c(0,y))
    plot(x, y, type='l', xlim=xlim, ylim=ylim, axes=FALSE, col=curveColor, xlab = "", ylab = "")
    these <- (x >= U)
    X <- c(x[these][1], x[these], rev(x[these])[1])
    Y <- c(0, y[these], 0)
    polygon(X, Y, border=border, col=col)
    abline(h=0)
    axis(1, cex.axis = 1.5)
  }

FTail <-
  function(U=NULL, df_n=100, df_d = 100, curveColor=1, border=1, col="#569BBD", xlim=NULL, ylim=NULL, xlab='', ylab='', detail=999){
    if(U <= 5){xlim <- c(0,5)}
    if(U > 5){xlim <- c(0,U+0.01*U)}
    temp <- diff(range(xlim))
    x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
    y    <- df(x, df_n, df_d)
    ylim <- range(c(0,y))
    plot(x, y, type='l', xlim=xlim, ylim=ylim, axes=FALSE, col=curveColor, xlab = "", ylab = "")
    these <- (x >= U)
    X <- c(x[these][1], x[these], rev(x[these])[1])
    Y <- c(0, y[these], 0)
    polygon(X, Y, border=border, col=col)
    abline(h=0)
    axis(1, cex.axis = 1.5)
  }

normTail <-
  function(m=0, s=1, L=NULL, U=NULL, M=NULL, df=1000, curveColor=1, border=1, col='#569BBD', xlim=NULL, ylim=NULL, xlab='', ylab='', digits=2, axes=1, detail=999, xLab=c('number', 'symbol'), cex.axis=1, xAxisIncr=1, ...){
    if(is.null(xlim)[1]){
      xlim <- m + c(-1,1)*3.5*s
    }
    temp <- diff(range(xlim))
    x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
    y    <- dt((x-m)/s, df)/s
    if(is.null(ylim)[1]){
      ylim <- range(c(0,y))
    }
    plot(x, y, type='l', xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, axes=FALSE, col=curveColor, ...)
    if(!is.null(L[1])){
      these <- (x <= L)
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    if(!is.null(U[1])){
      these <- (x >= U)
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    if(all(!is.null(M[1:2]))){
      these <- (x >= M[1] & x <= M[2])
      X <- c(x[these][1], x[these], rev(x[these])[1])
      Y <- c(0, y[these], 0)
      polygon(X, Y, border=border, col=col)
    }
    
    if(axes == 1 || axes > 2){
      if(xLab[1]=='symbol'){
        xAt  <- m + (-3:3)*s
        xLab <- expression(mu-3*sigma, mu-2*sigma,
                           mu-sigma, mu,	mu+sigma,
                           mu+2*sigma, mu+3*sigma)
      } else if(xLab[1] != 'number'){
        stop('Argument "xLab" not recognized.\n')
      } else {
        temp <- seq(xAxisIncr, max(abs(xlim-m))/s, xAxisIncr)*s
        xAt <- m + c(-temp, 0, temp)
        xLab <- round(xAt, digits=digits)
      }
    }
    if(axes > 2){
      axis(1, at=xAt, labels=xLab, cex.axis=cex.axis)
      buildAxis(2, c(y,0), n=3, nMax=3, cex.axis=cex.axis)
    } else if(axes > 1){
      buildAxis(2, c(y,0), n=3, nMax=3, cex.axis=cex.axis)
    } else if(axes > 0){
      axis(1, at=xAt, labels=xLab, cex.axis=cex.axis)
    }
    
    abline(h=0)
  }

buildAxis<-function (side, limits, n, nMin = 2, nMax = 10, extend = 2, eps = 10^-12, 
                     ...) 
{
  if (!all(is.finite(limits))) {
    stop("Must provide finite limits.\n")
  }
  limits <- range(limits)
  if (limits[1] == limits[2]) {
    stop("Range of \"limits\" is too small. Scale the data.\n")
  }
  L <- limits
  l <- L + c(-1, 1) * diff(L) * extend
  s <- sign(l)
  l10 <- round(log10(abs(l)))
  d <- diff(l)
  d10 <- round(log10(d))
  L1 <- L
  temp <- round(L1[1]/10^(d10))
  L <- L1 - 10^(d10) * temp
  Lup <- temp * 10^(d10)
  l1 <- l
  l <- l1 - 10^(d10) * temp
  lup <- temp * 10^(d10)
  si <- list()
  si[[1]] <- seq(-6, 5, 0.01)/10
  si[[2]] <- seq(-6, 5, 0.015)/10
  si[[3]] <- seq(-6, 5, 0.02)/10
  si[[4]] <- seq(-6, 5, 0.025)/10
  si[[5]] <- seq(-6, 5, 0.03)/10
  si[[6]] <- seq(-6, 5, 0.04)/10
  si[[7]] <- seq(-6, 5, 0.05)/10
  si[[8]] <- seq(-6, 5, 0.06)/10
  si[[9]] <- seq(-7, 5, 0.07)/10
  si[[10]] <- seq(-6, 5, 0.08)/10
  AES <- c(8, 0, 7, 5, 3, 4, 7, 2, 1, 2)
  for (i in 0:2) {
    for (j in 1:10) {
      temp <- round(10000 * si[[j]] * 10^i)/10000
      si[[i * 10 + j]] <- temp
      AES[i * 10 + j] <- AES[j]
    }
  }
  if (0 >= L[1] && 0 <= L[2]) {
    start <- 0
  }
  else {
    start <- -10^max(round(log10(abs(L)) + 0.5))
    go <- rep(TRUE, 2)
    temp <- 10^max(round(log10(abs(L)) - 0.5))
    while (all(go)) {
      go <- FALSE
      if (start < L[1]) {
        start <- start + temp
        go <- TRUE
      }
    }
  }
  br <- list()
  se <- list()
  ss <- list()
  le <- c()
  for (i in 1:length(si)) {
    br[[i]] <- si[[i]] * 10^d10
    se[[i]] <- start + br[[i]]
    these <- (se[[i]] <= l[2] + eps) & (se[[i]] >= l[1] - 
                                          eps)
    ss[[i]] <- se[[i]][these]
    these <- (se[[i]] <= L[2] + eps) & (se[[i]] >= L[1] - 
                                          eps)
    le[i] <- sum(these)
    ss[[i]] <- ss[[i]] + Lup
  }
  L <- L1
  l <- l1
  these <- which(le >= nMin & le <= nMax)
  aes <- c()
  for (i in these) {
    min((ss[[i]][ss[[i]] > L[1]] - L[1])/d)
    min((L[1] - ss[[i]][ss[[i]] < L[1]])/d)
    abs(n - le[[i]])
    temp <- ss[[i]][ss[[i]] > L[1]] - L[1]
    temp1 <- -log(max(c(0.01, min(temp/d))), 5)
    temp <- L[1] - ss[[i]][ss[[i]] < L[1]]
    temp2 <- -log(max(c(0.01, min(temp/d))), 5)
    temp3 <- -abs(le[i] - n)^2/(n + 1)
    AES[i] <- AES[i] + temp1 + temp2 + temp3
  }
  select <- which.max(AES[these])[1]
  l <- ss[[these[select]]]
  temp <- -round(log10(eps))
  l <- (round(l * 10^temp)) * 10^(-temp)
  axis(side, at = l, ...)
  invisible(l)
}

# Set defaults -----------------------------------------------------------------
defaults <- list(
  "tail" = "lower",
  "lower_bound" = "open",
  "upper_bound" = "open"
)

# Define UI --------------------------------------------------------------------
ui <- pageWithSidebar(
  
  # Title ----
  headerPanel("Probability Distribution Calculator"),
  
  # Sidebar ----
  sidebarPanel(
    selectInput(
      inputId = "dist",
      label = "Distribution:",
      choices = c(
        "Normal" = "rnorm",
        "Binomial" = "rbinom",
        "t" = "rt",
        "F" = "rf",
        "Chi-Squared" = "rchisq"
      ),
      selected = "rnorm"
    ),
    
    uiOutput("mean"),
    uiOutput("sd"),
    uiOutput("df1"),
    uiOutput("df2"),
    uiOutput("n"),
    uiOutput("p"),
    
    helpText("Model:"),
    div(textOutput("model"), style = "text-indent:20px;font-size:125%;"),
    br(),
    
    uiOutput("tail"),
    uiOutput("lower_bound"),
    uiOutput("upper_bound"),
    
    uiOutput("a"),
    uiOutput("b"),
    
    br(),
    
    helpText("_____________________"),
    helpText("Glenn Tattersall, PhD"),
    helpText("For use in BIOL 3P96 - Biostatistics")
  ),
  
  mainPanel(
    plotOutput("plot"),
    div(textOutput("area"), align = "center", style = "font-size:150%;"),
    br(),
    uiOutput("interpretation_box")
  )
)

# Define server function -------------------------------------------------------
server <- function(input, output) {
  output$tail <- renderUI({
    if (input$dist == "rbinom") {
      selectInput(
        inputId = "tail",
        label = "Find Area:",
        choices = c(
          "Lower Tail" = "lower",
          "Upper Tail" = "upper",
          "Both Tails" = "both",
          "Middle" = "middle",
          "Equality" = "equal"
        ),
        selected = "lower"
      )
    }
    else if (input$dist == "rf" | input$dist == "rchisq") {
      selectInput(
        inputId = "tail",
        label = "Find Area:",
        choices = c("Upper Tail" = "upper"),
        selected = "upper"
      )
    }
    else {
      selectInput(
        inputId = "tail",
        label = "Find Area:",
        choices = c(
          "Lower Tail" = "lower",
          "Upper Tail" = "upper",
          "Both Tails" = "both",
          "Middle" = "middle"
        ),
        selected = "lower"
      )
    }
  })
  
  output$lower_bound <- renderUI({
    if (input$dist == "rbinom") {
      if (is.null(input$tail)) {
        shiny:::flushReact()
        return()
      }
      
      if (input$tail %in% c("both", "middle")) {
        tagList(
          selectInput(
            inputId = "lower_bound",
            label = "Lower bound:",
            choices = c(
              "<" = "open",
              "\u2264" = "closed"
            ),
            selected = "open"
          ),
          helpText("For the binomial, including or excluding the boundary value changes the probability.")
        )
      }
      else if (input$tail == "lower") {
        tagList(
          selectInput(
            inputId = "lower_bound",
            label = "Bound:",
            choices = c(
              "<" = "open",
              "\u2264" = "closed"
            ),
            selected = "open"
          ),
          helpText("For the binomial, including or excluding the boundary value changes the probability.")
        )
      }
      else if (input$tail == "upper") {
        tagList(
          selectInput(
            inputId = "lower_bound",
            label = "Bound:",
            choices = c(
              ">" = "open",
              "\u2265" = "closed"
            ),
            selected = "open"
          ),
          helpText("For the binomial, including or excluding the boundary value changes the probability.")
        )
      }
    }
  })
  
  output$upper_bound <- renderUI({
    if (input$dist == "rbinom") {
      if (is.null(input$tail)) {
        shiny:::flushReact()
        return()
      }
      
      if (input$tail == "middle") {
        tagList(
          selectInput(
            inputId = "upper_bound",
            label = "Upper bound:",
            choices = c(
              "<" = "open",
              "\u2264" = "closed"
            ),
            selected = "open"
          ),
          helpText("For the binomial, including or excluding the boundary value changes the probability.")
        )
      }
      else if (input$tail == "both") {
        tagList(
          selectInput(
            inputId = "upper_bound",
            label = "Upper bound:",
            choices = c(
              ">" = "open",
              "\u2265" = "closed"
            ),
            selected = "open"
          ),
          helpText("For the binomial, including or excluding the boundary value changes the probability.")
        )
      }
    }
  })
  
  get_model_text <- reactive({
    if (is.null(input$tail)) {
      shiny:::flushReact()
      return()
    }
    
    low_less <- "<"
    low_greater <- ">"
    up_less <- "<"
    up_greater <- ">"
    
    if (input$dist == "rbinom" & input$tail != "equal") {
      if (is.null(input$lower_bound)) {
        shiny:::flushReact()
        return()
      }
      if (input$lower_bound == "closed") {
        low_less <- "\u2264"
        low_greater <- "\u2265"
      }
      if (input$tail %in% c("middle", "both")) {
        if (is.null(input$upper_bound)) {
          shiny:::flushReact()
          return()
        }
        if (input$upper_bound == "closed") {
          up_less <- "\u2264"
          up_greater <- "\u2265"
        }
      }
    }
    
    text <- ""
    if (length(input$tail) != 0) {
      if (input$tail == "lower") {
        text <- paste0("P(X ", low_less, " a)")
      }
      else if (input$tail == "upper") {
        text <- paste0("P(X ", low_greater, " a)")
      }
      else if (input$tail == "middle") {
        text <- paste0("P(a ", low_less, " X ", up_less, " b)")
      }
      else if (input$tail == "both") {
        text <- paste0("P(X ", low_less, " a or X ", up_greater, " b)")
      }
      else if (input$tail == "equal") {
        text <- paste0("P(X = a)")
      }
    }
    
    return(text)
  })
  
  output$model <- renderText({
    get_model_text()
  })
  
  #######################
  # Normal distribution #
  #######################
  
  output$mean <- renderUI({
    if (input$dist == "rnorm") {
      sliderInput("mu",
                  "Mean",
                  value = 0,
                  min = -50,
                  max = 50
      )
    }
  })
  
  output$sd <- renderUI({
    if (input$dist == "rnorm") {
      sliderInput("sd",
                  "Standard deviation",
                  value = 1,
                  min = 0.1,
                  max = 30,
                  step = 0.1
      )
    }
  })
  
  ##########################
  # t, F, X^2 distribution #
  ##########################
  
  output$df1 <- renderUI({
    if (input$dist %in% c("rt", "rchisq", "rf")) {
      sliderInput(ifelse(input$dist %in% c("rt", "rchisq"), "df", "df1"),
                  "Degrees of freedom",
                  value = 10,
                  min = 1,
                  max = 50
      )
    }
  })
  
  output$df2 <- renderUI({
    if (input$dist == "rf") {
      sliderInput("df2",
                  "Degrees of freedom (2)",
                  value = 10,
                  min = 1,
                  max = 50
      )
    }
  })
  
  #########################
  # Binomial distribution #
  #########################
  
  output$n <- renderUI({
    if (input$dist == "rbinom") {
      sliderInput("n",
                  "n",
                  value = 10,
                  min = 1,
                  max = 250,
                  step = 1
      )
    }
  })
  
  output$p <- renderUI({
    if (input$dist == "rbinom") {
      sliderInput("p",
                  "p",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = .01
      )
    }
  })
  
  output$a <- renderUI({
    value <- 1
    min <- 0
    max <- 1
    step <- 1
    
    if (input$dist == "rnorm") {
      find_normal_step <- function(sd) {
        10^round(log(7 * sd / 100, 10))
      }
      if (is.null(input$mu) | is.null(input$sd)) {
        shiny:::flushReact()
        return()
      }
      mu <- input$mu
      sd <- input$sd
      if (is.null(mu)) mu <- 0
      if (is.null(sd)) sd <- 1
      value <- mu - 1.96 * sd
      min <- mu - 4 * sd
      max <- mu + 4 * sd
      step <- find_normal_step(sd)
      if (mu == 0 & sd == 1) {
        step <- .01
      }
    }
    else if (input$dist == "rt") {
      value <- -1.96
      min <- -6
      max <- 6
      step <- 0.01
    }
    else if (input$dist == "rf") {
      value <- round(qf(.95, as.numeric(input$df1), as.numeric(input$df2)), digits = 2)
      min <- 0
      max <- round(qf(.995, as.numeric(input$df1), as.numeric(input$df2)) * 1.05, digits = 2)
      step <- 0.01
    }
    else if (input$dist == "rchisq") {
      value <- round(qchisq(.95, as.numeric(input$df)), digits = 2)
      min <- 0
      max <- round(qchisq(.995, as.numeric(input$df)), digits = 2)
      step <- 0.01
    }
    else if (input$dist == "rbinom") {
      if (is.null(input$n)) {
        shiny:::flushReact()
        return()
      }
      value <- round(input$n / 4)
      min <- 0
      max <- input$n
      step <- 1
    }
    
    sliderInput("a", "a",
                value = value,
                min = min,
                max = max,
                step = step
    )
  })
  
  output$b <- renderUI({
    if (is.null(input$tail)) {
      shiny:::flushReact()
      return()
    }
    
    if (input$tail %in% c("middle", "both")) {
      value <- 1
      min <- 0
      max <- 1
      step <- 1
      
      if (input$dist == "rnorm") {
        find_normal_step <- function(sd) {
          10^round(log(7 * sd / 100, 10))
        }
        if (is.null(input$mu) | is.null(input$sd)) {
          shiny:::flushReact()
          return()
        }
        mu <- input$mu
        sd <- input$sd
        if (is.null(mu)) mu <- 0
        if (is.null(sd)) sd <- 1
        value <- mu + 1.96 * sd
        min <- mu - 4 * sd
        max <- mu + 4 * sd
        step <- find_normal_step(sd)
      }
      else if (input$dist == "rt") {
        value <- 1.96
        min <- -6
        max <- 6
        step <- 0.01
      }
      else if (input$dist == "rbinom") {
        if (is.null(input$n)) {
          shiny:::flushReact()
          return()
        }
        value <- round(input$n * 3 / 4)
        min <- 0
        max <- input$n
        step <- 1
      }
      
      sliderInput("b", "b",
                  value = value,
                  min = min,
                  max = max,
                  step = step
      )
    }
  })
  
  ############
  # Plotting #
  ############
  
  output$plot <- renderPlot({
    if (is.null(input$tail) | is.null(input$a)) {
      shiny:::flushReact()
      return()
    }
    
    L <- NULL
    U <- NULL
    error <- FALSE
    
    if (input$tail == "lower" | input$tail == "equal") {
      L <- input$a
    } else if (input$tail == "upper") {
      U <- input$a
    } else if (input$tail %in% c("both", "middle")) {
      if (is.null(input$b)) {
        shiny:::flushReact()
        return()
      }
      L <- input$a
      U <- input$b
      if (L > U) error <- TRUE
    }
    
    if (error) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", mar = c(1, 1, 1, 1))
      text(0, 0, "Error: Lower bound greater than upper bound.", col = "red", cex = 2)
    } else {
      
      # Helper: add threshold lines and arrows after a continuous distribution is plotted
      add_threshold_decorations <- function(tail, L, U, y_peak) {
        arrow_y  <- y_peak * 0.60   # arrow sits at 60% of curve height
        line_col <- "#444444"
        arr_col  <- "#444444"
        arr_len  <- 0.18            # arrowhead size (inches)
        arr_ang  <- 25
        lbl_col  <- "#569BBD"       # blue label colour to match threshold lines
        
        # Helper to draw an a or b label just below the x-axis at position xpos
        add_ab_label <- function(xpos, label) {
          mtext(label, side = 1, at = xpos, line = 2.2,
                col = lbl_col, font = 2, cex = 1.2)
        }
        
        if (tail == "lower") {
          abline(v = L, lty = 2, col = line_col, lwd = 1.5)
          arrows(x0 = L, y0 = arrow_y, x1 = L - 0.55 * abs(L), y1 = arrow_y,
                 length = arr_len, angle = arr_ang, col = arr_col, lwd = 1.5)
          add_ab_label(L, "a")
        } else if (tail == "upper") {
          abline(v = U, lty = 2, col = line_col, lwd = 1.5)
          arrows(x0 = U, y0 = arrow_y, x1 = U + 0.55 * abs(U), y1 = arrow_y,
                 length = arr_len, angle = arr_ang, col = arr_col, lwd = 1.5)
          add_ab_label(U, "a")
        } else if (tail == "both") {
          abline(v = L, lty = 2, col = line_col, lwd = 1.5)
          abline(v = U, lty = 2, col = line_col, lwd = 1.5)
          arrows(x0 = L, y0 = arrow_y, x1 = L - 0.55 * abs(L), y1 = arrow_y,
                 length = arr_len, angle = arr_ang, col = arr_col, lwd = 1.5)
          arrows(x0 = U, y0 = arrow_y, x1 = U + 0.55 * abs(U), y1 = arrow_y,
                 length = arr_len, angle = arr_ang, col = arr_col, lwd = 1.5)
          add_ab_label(L, "a")
          add_ab_label(U, "b")
        } else if (tail %in% c("middle", "equal")) {
          abline(v = L, lty = 2, col = line_col, lwd = 1.5)
          abline(v = U, lty = 2, col = line_col, lwd = 1.5)
          mid <- (L + U) / 2
          arrows(x0 = L, y0 = arrow_y, x1 = mid, y1 = arrow_y,
                 length = arr_len, angle = arr_ang, col = arr_col, lwd = 1.5)
          arrows(x0 = U, y0 = arrow_y, x1 = mid, y1 = arrow_y,
                 length = arr_len, angle = arr_ang, col = arr_col, lwd = 1.5)
          add_ab_label(L, "a")
          add_ab_label(U, "b")
        }
      }
      
      if (input$dist == "rnorm" | input$dist == "rt") {
        M <- NULL
        if (input$tail == "middle") {
          M <- c(L, U)
          L <- NULL
          U <- NULL
        }
        if (input$dist == "rnorm") {
          if (is.null(input$mu) | is.null(input$sd)) {
            shiny:::flushReact()
            return()
          }
          normTail(m = input$mu, s = input$sd, L = L, U = U, M = M, axes = 3, cex.axis = 1.5)
          y_peak <- dnorm(input$mu, input$mu, input$sd)
          title(main = "Normal Distribution")
        } else if (input$dist == "rt") {
          if (is.null(input$df)) {
            shiny:::flushReact()
            return()
          }
          normTail(m = 0, s = 1, df = input$df, L = L, U = U, M = M, axes = 3, cex.axis = 1.5)
          y_peak <- dt(0, input$df)
          title(main = "t Distribution")
        }
        # Restore L/U for decoration after middle swap
        if (input$tail == "middle") {
          L <- input$a
          U <- input$b
        } else if (input$tail == "lower" | input$tail == "equal") {
          L <- input$a
        } else if (input$tail == "upper") {
          U <- input$a
        }
        add_threshold_decorations(input$tail, L, U, y_peak)
        
      } else if (input$dist == "rchisq") {
        if (is.null(input$df)) {
          shiny:::flushReact()
          return()
        }
        x_max <- round(qchisq(.995, input$df), digits = 2) + 1
        chiTail(U = U, df = input$df, xlim = c(0, x_max))
        y_peak <- dchisq(max(input$df - 2, 0.1), input$df)
        title(main = "Chi^2 Distribution")
        add_threshold_decorations(input$tail, L, U, y_peak)
        
      } else if (input$dist == "rf") {
        req(U)
        FTail(U = U, df_n = input$df1, df_d = input$df2)
        mode_f <- max((input$df1 - 2) / (input$df1) * (input$df2) / (input$df2 + 2), 0.1)
        y_peak <- df(mode_f, input$df1, input$df2)
        title(main = "F Distribution")
        add_threshold_decorations(input$tail, L, U, y_peak)
        
      } else if (input$dist == "rbinom") {
        if (is.null(input$n) | is.null(input$p) | is.null(input$lower_bound)) {
          shiny:::flushReact()
          return()
        }
        if (input$tail %in% c("both", "middle") & is.null(input$upper_bound)) {
          shiny:::flushReact()
          return()
        }
        
        d <- dbinom(0:input$n, input$n, input$p)
        plot(0, 0,
             type = "n", xlim = c(-0.5, input$n + 0.5), ylim = c(0, max(d)),
             xlab = "", ylab = "", axes = FALSE
        )
        axis(1, cex.axis = 1.5)
        axis(2, cex.axis = 1.5)
        title(main = paste("Binomial Distribution"))
        
        for (k in 1:length(d)) {
          col <- NA
          if (input$tail == "lower") {
            if (input$lower_bound == "open"   & k - 1 <  L) col <- "#569BBD"
            if (input$lower_bound == "closed" & k - 1 <= L) col <- "#569BBD"
          } else if (input$tail == "upper") {
            if (input$lower_bound == "open"   & k - 1 >  U) col <- "#569BBD"
            if (input$lower_bound == "closed" & k - 1 >= U) col <- "#569BBD"
          } else if (input$tail == "equal") {
            if (k - 1 == L) col <- "#569BBD"
          } else if (input$tail == "both") {
            if (input$lower_bound == "open"   & input$upper_bound == "open"   & (k - 1 <  L | k - 1 >  U)) col <- "#569BBD"
            if (input$lower_bound == "open"   & input$upper_bound == "closed" & (k - 1 <  L | k - 1 >= U)) col <- "#569BBD"
            if (input$lower_bound == "closed" & input$upper_bound == "open"   & (k - 1 <= L | k - 1 >  U)) col <- "#569BBD"
            if (input$lower_bound == "closed" & input$upper_bound == "closed" & (k - 1 <= L | k - 1 >= U)) col <- "#569BBD"
          } else if (input$tail == "middle") {
            if (input$lower_bound == "open"   & input$upper_bound == "open"   & k - 1 >  L & k - 1 <  U) col <- "#569BBD"
            if (input$lower_bound == "open"   & input$upper_bound == "closed" & k - 1 >  L & k - 1 <= U) col <- "#569BBD"
            if (input$lower_bound == "closed" & input$upper_bound == "open"   & k - 1 >= L & k - 1 <  U) col <- "#569BBD"
            if (input$lower_bound == "closed" & input$upper_bound == "closed" & k - 1 >= L & k - 1 <= U) col <- "#569BBD"
          }
          p <- matrix(c(-1.5 + k, 0, -0.5 + k, 0, -0.5 + k, d[k], -1.5 + k, d[k], -1.5 + k, 0),
                      ncol = 2, byrow = TRUE)
          polygon(p, col = col)
        }
        
        # Dashed threshold lines only for binomial (no arrows)
        if (!is.null(L)) {
          abline(v = L, lty = 2, col = "#444444", lwd = 1.5)
          mtext("a", side = 1, at = L, line = 2.2, col = "#569BBD", font = 2, cex = 1.2)
        }
        if (!is.null(U)) {
          abline(v = U, lty = 2, col = "#444444", lwd = 1.5)
          mtext("b", side = 1, at = U, line = 2.2, col = "#569BBD", font = 2, cex = 1.2)
        }
      }
    }
  })
  
  ################
  # Calculations #
  ################
  
  output$area <- renderText({
    if (is.null(input$tail) | is.null(input$a)) {
      shiny:::flushReact()
      return()
    }
    
    L <- input$a
    U <- NULL
    
    if (input$tail %in% c("both", "middle")) {
      if (is.null(input$b)) {
        shiny:::flushReact()
        return()
      }
      U <- input$b
      if (L > U) return()
    }
    
    f <- function() NULL
    
    if (input$dist == "rnorm") {
      if (is.null(input$mu) | is.null(input$sd)) {
        shiny:::flushReact()
        return()
      }
      f <- function(x) pnorm(x, input$mu, input$sd)
    }
    else if (input$dist == "rt") {
      if (is.null(input$df)) {
        shiny:::flushReact()
        return()
      }
      f <- function(x) pt(x, input$df)
    }
    else if (input$dist == "rchisq") {
      if (is.null(input$df)) {
        shiny:::flushReact()
        return()
      }
      f <- function(x) pchisq(x, input$df)
    }
    else if (input$dist == "rf") {
      if (is.null(input$df1) | is.null(input$df2)) {
        shiny:::flushReact()
        return()
      }
      f <- function(x) pf(x, input$df1, input$df2)
    }
    else if (input$dist == "rbinom") {
      if (is.null(input$n) | is.null(input$p) | is.null(input$lower_bound)) {
        shiny:::flushReact()
        return()
      }
      if (input$tail == "equal") {
        f <- function(x) dbinom(x, input$n, input$p)
      }
      else {
        f <- function(x) pbinom(x, input$n, input$p)
        if (input$tail %in% c("lower", "both") & input$lower_bound == "open") L <- L - 1
        if (input$tail %in% c("upper") & input$lower_bound == "closed") L <- L - 1
        if (input$tail %in% c("middle") & input$lower_bound == "closed") L <- L - 1
        if (input$tail %in% c("both", "middle")) {
          if (is.null(input$upper_bound)) {
            shiny:::flushReact()
            return()
          }
          if (input$tail == "both" & input$upper_bound == "closed") U <- U - 1
          if (input$tail == "middle" & input$upper_bound == "open") U <- U - 1
        }
      }
    }
    
    val <- NA
    if (input$tail == "lower") {
      val <- f(L)
    } else if (input$tail == "upper") {
      val <- 1 - f(L)
    } else if (input$tail == "equal") {
      val <- f(L)
    } else if (input$tail == "both") {
      val <- f(L) + (1 - f(U))
    } else if (input$tail == "middle") {
      val <- f(U) - f(L)
    }
    
    text <- paste(get_model_text(), "=", signif(val, 3))
    text <- sub("a", input$a, text)
    if (input$tail %in% c("both", "middle")) {
      text <- sub("b", input$b, text)
    }
    text
  })
  
  ###################################
  # Interpretation + R code panel  #
  ###################################
  
  output$interpretation_box <- renderUI({
    if (is.null(input$tail) | is.null(input$a)) return()
    
    # ---- Compute the probability value (mirrors output$area logic) ----
    L <- input$a
    U <- NULL
    
    if (input$tail %in% c("both", "middle")) {
      if (is.null(input$b)) return()
      U <- input$b
      if (L > U) return()
    }
    
    # Build distribution-specific parameters and R code strings
    dist_name  <- ""
    param_desc <- ""
    r_code     <- ""
    
    # Adjusted L/U for pbinom (mirroring the area calculation logic)
    La <- L
    Ua <- U
    
    if (input$dist == "rnorm") {
      if (is.null(input$mu) | is.null(input$sd)) return()
      mu <- input$mu; sd <- input$sd
      dist_name  <- "Normal"
      param_desc <- paste0("mean = ", mu, ", SD = ", sd)
      
      if (input$tail == "lower") {
        val <- pnorm(La, mu, sd)
        r_code <- paste0("pnorm(", La, ", mean = ", mu, ", sd = ", sd, ", lower.tail = TRUE)")
      } else if (input$tail == "upper") {
        val <- 1 - pnorm(La, mu, sd)
        r_code <- paste0("pnorm(", La, ", mean = ", mu, ", sd = ", sd, ", lower.tail = FALSE)\n",
                         "# equivalent: 1 - pnorm(", La, ", mean = ", mu, ", sd = ", sd, ", lower.tail = TRUE)")
      } else if (input$tail == "middle") {
        val <- pnorm(Ua, mu, sd) - pnorm(La, mu, sd)
        r_code <- paste0("pnorm(", Ua, ", mean = ", mu, ", sd = ", sd, ", lower.tail = TRUE) - ",
                         "pnorm(", La, ", mean = ", mu, ", sd = ", sd, ", lower.tail = TRUE)")
      } else if (input$tail == "both") {
        val <- pnorm(La, mu, sd) + (1 - pnorm(Ua, mu, sd))
        r_code <- paste0("pnorm(", La, ", mean = ", mu, ", sd = ", sd, ", lower.tail = TRUE) + ",
                         "pnorm(", Ua, ", mean = ", mu, ", sd = ", sd, ", lower.tail = FALSE)")
      }
      
    } else if (input$dist == "rt") {
      if (is.null(input$df)) return()
      df <- input$df
      dist_name  <- "t"
      param_desc <- paste0("df = ", df)
      
      if (input$tail == "lower") {
        val <- pt(La, df)
        r_code <- paste0("pt(", La, ", df = ", df, ", lower.tail = TRUE)")
      } else if (input$tail == "upper") {
        val <- 1 - pt(La, df)
        r_code <- paste0("pt(", La, ", df = ", df, ", lower.tail = FALSE)\n",
                         "# equivalent: 1 - pt(", La, ", df = ", df, ", lower.tail = TRUE)")
      } else if (input$tail == "middle") {
        val <- pt(Ua, df) - pt(La, df)
        r_code <- paste0("pt(", Ua, ", df = ", df, ", lower.tail = TRUE) - ",
                         "pt(", La, ", df = ", df, ", lower.tail = TRUE)")
      } else if (input$tail == "both") {
        val <- pt(La, df) + (1 - pt(Ua, df))
        r_code <- paste0("pt(", La, ", df = ", df, ", lower.tail = TRUE) + ",
                         "pt(", Ua, ", df = ", df, ", lower.tail = FALSE)")
      }
      
    } else if (input$dist == "rchisq") {
      if (is.null(input$df)) return()
      df <- input$df
      dist_name  <- "Chi-squared"
      param_desc <- paste0("df = ", df)
      val <- 1 - pchisq(La, df)
      r_code <- paste0("pchisq(", La, ", df = ", df, ", lower.tail = FALSE)\n",
                       "# equivalent: 1 - pchisq(", La, ", df = ", df, ", lower.tail = TRUE)")
      
    } else if (input$dist == "rf") {
      if (is.null(input$df1) | is.null(input$df2)) return()
      df1 <- input$df1; df2 <- input$df2
      dist_name  <- "F"
      param_desc <- paste0("df1 = ", df1, ", df2 = ", df2)
      val <- 1 - pf(La, df1, df2)
      r_code <- paste0("pf(", La, ", df1 = ", df1, ", df2 = ", df2, ", lower.tail = FALSE)\n",
                       "# equivalent: 1 - pf(", La, ", df1 = ", df1, ", df2 = ", df2, ", lower.tail = TRUE)")
      
    } else if (input$dist == "rbinom") {
      if (is.null(input$n) | is.null(input$p) | is.null(input$lower_bound)) return()
      n <- input$n; p <- input$p
      dist_name  <- "Binomial"
      param_desc <- paste0("n = ", n, ", p = ", p)
      
      if (input$tail == "equal") {
        val <- dbinom(La, n, p)
        r_code <- paste0("dbinom(", La, ", size = ", n, ", prob = ", p, ")")
      } else {
        # Mirror the bound adjustments from the area output
        if (input$tail %in% c("lower", "both") & input$lower_bound == "open") La <- La - 1
        if (input$tail == "upper" & input$lower_bound == "closed") La <- La - 1
        if (input$tail == "middle" & input$lower_bound == "closed") La <- La - 1
        if (!is.null(Ua) & input$tail %in% c("both", "middle")) {
          if (is.null(input$upper_bound)) return()
          if (input$tail == "both" & input$upper_bound == "closed") Ua <- Ua - 1
          if (input$tail == "middle" & input$upper_bound == "open") Ua <- Ua - 1
        }
        
        if (input$tail == "lower") {
          val <- pbinom(La, n, p)
          r_code <- paste0("pbinom(", La, ", size = ", n, ", prob = ", p, ", lower.tail = TRUE)")
        } else if (input$tail == "upper") {
          val <- 1 - pbinom(La, n, p)
          r_code <- paste0("pbinom(", La, ", size = ", n, ", prob = ", p, ", lower.tail = FALSE)\n",
                           "# equivalent: 1 - pbinom(", La, ", size = ", n, ", prob = ", p, ", lower.tail = TRUE)")
        } else if (input$tail == "middle") {
          val <- pbinom(Ua, n, p) - pbinom(La, n, p)
          r_code <- paste0("pbinom(", Ua, ", size = ", n, ", prob = ", p, ", lower.tail = TRUE) - ",
                           "pbinom(", La, ", size = ", n, ", prob = ", p, ", lower.tail = TRUE)")
        } else if (input$tail == "both") {
          val <- pbinom(La, n, p) + (1 - pbinom(Ua, n, p))
          r_code <- paste0("pbinom(", La, ", size = ", n, ", prob = ", p, ", lower.tail = TRUE) + ",
                           "pbinom(", Ua, ", size = ", n, ", prob = ", p, ", lower.tail = FALSE)")
        }
      }
    } else {
      return()
    }
    
    # ---- Plain-English interpretation ----
    pct <- paste0(round(val * 100, 2), "%")
    
    interp <- switch(input$tail,
                     "lower" = paste0(
                       "There is a ", pct, " chance of observing a value of ", input$a,
                       " or less under the ", dist_name, " distribution (", param_desc, ")."
                     ),
                     "upper" = paste0(
                       "There is a ", pct, " chance of observing a value of ", input$a,
                       " or greater under the ", dist_name, " distribution (", param_desc, ").",
                       " This is the p-value when your test statistic equals ", input$a, "."
                     ),
                     "middle" = paste0(
                       "There is a ", pct, " chance of observing a value between ", input$a,
                       " and ", input$b, " under the ", dist_name, " distribution (", param_desc, ")."
                     ),
                     "both" = paste0(
                       "There is a ", pct, " chance of observing a value as extreme as ",
                       input$a, " or more in either direction under the ", dist_name,
                       " distribution (", param_desc, ").",
                       " This corresponds to a two-tailed p-value."
                     ),
                     "equal" = paste0(
                       "The probability of observing exactly ", input$a,
                       " under the ", dist_name, " distribution (", param_desc, ") is ", pct, "."
                     )
    )
    
    # ---- Render the panel ----
    tagList(
      tags$div(
        style = paste(
          "border: 1px solid #d0d0d0;",
          "border-radius: 6px;",
          "padding: 14px 18px;",
          "background-color: #f9f9f9;",
          "font-size: 95%;"
        ),
        tags$p(
          tags$strong("Interpretation:"),
          tags$span(interp)
        ),
        tags$hr(style = "margin: 8px 0;"),
        tags$p(
          tags$strong("R code:"),
          style = "margin-bottom: 4px;"
        ),
        tags$pre(
          style = paste(
            "background-color: #f0f0f0;",
            "border: 1px solid #ccc;",
            "border-radius: 4px;",
            "padding: 8px 12px;",
            "font-size: 90%;",
            "white-space: pre-wrap;",
            "word-break: break-word;"
          ),
          r_code
        )
      )
    )
  })
}

# Create the Shiny app object --------------------------------------------------
shinyApp(ui = ui, server = server)