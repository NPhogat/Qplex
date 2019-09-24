library(shiny)

library(Qplex)

shinyServer(function(input,output){

  ftype <- reactive({input$filetype})

  dp <- reactive({input$decp})

  yint <- reactive({input$yin})

  mslp <- reactive({input$mslp})

  cint <- reactive({input$cint})

  file <- reactive({

    if(is.null(input$file)){

      infile <- NULL

    }
    else{

      infile <- readQD(input$file$datapath, type = ftype(),decp = dp())

      x.initdata <- as.data.frame(slot(infile,"initialData"))

      x.initdata
    }

  })

  crep <- eventReactive(input$compute,{

    runif(input$file)

    isolate({

      if(!(is.null(file))){

        x.init <- file()
        x.data <- new("Qdata",initialData = x.init)
        x.crep <- Repsd(x.data)
        x.nsg1 <- as.data.frame(slot(x.crep,"cgData"))
        x.nsg <- x.nsg1[,"Green_NI"]
        x.sdg1 <- as.data.frame(slot(x.crep,"sdgData"))
        x.sdg <- x.sdg1[,"Green_NI"]
        x.nsr1 <- as.data.frame(slot(x.crep,"crData"))
        x.nsr <- x.nsr1[,"Red_NI"]
        x.sdr1 <- as.data.frame(slot(x.crep,"sdrData"))
        x.sdr <- x.sdr1[,"Red_NI"]
        x.concn <- as.data.frame(slot(x.crep,"Concentration"))
        x.concn1 <- nrow((!(is.na(x.concn))))
        x.concn2 <- x.concn[1:x.concn1,]
        x.rep <- as.data.frame(cbind(x.nsg,x.sdg,x.nsr,x.sdr,x.concn2))
        names(x.rep) <- c("Green_NI","NI.sd.green","Red_NI","NI.sd.red","Concn")
        x.rep

      }

      else{

        return(NULL)

      }

    })

  })

  kd.g <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni.g <- x.crep[,"Green_NI"]
      sd.g <- x.crep[,"NI.sd.green"]
      conc <- x.crep[,"Concn"]
      ni.new <- as.data.frame(cbind(conc,ni.g,sd.g))
      names(ni.new) <- c("x","y","sd")
      plot1 <- ggplot(data =ni.new, aes(x,y))+ ggtitle("Intensity vs Concentration")+

        geom_point(colour = "black", na.rm = TRUE)+

        geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+

        geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=.2,

                      position=position_dodge(0.05))+

        xlab("Concentration (nM)") +

        ylab("Normalized Intensity (tl/cl) [arbitrary unit]")+

        stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,

                                                      sep = "~~~")),

                     parse = TRUE)


      plot1

    }

  })

  kd.r <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{
        x.crep <- crep()
        ni.r <- x.crep[,"Red_NI"]
        sd.r <- x.crep[,"NI.sd.red"]
        conc <- x.crep[,"Concn"]
        ni.new <- as.data.frame(cbind(conc,ni.r,sd.r))
        names(ni.new) <- c("x","y","sd")
        plot1 <- ggplot(data =ni.new, aes(x,y))+ ggtitle("Intensity vs Concentration")+

          geom_point(colour = "black", na.rm = TRUE)+

          geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+

          geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=.2,

                        position=position_dodge(0.05))+

          xlab("Concentration (nM)") +

          ylab("Normalized Intensity (tl/cl) [arbitrary unit]")+

          stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,

                                                        sep = "~~~")),

                       parse = TRUE)


        plot1
      }

  })

  conf_new <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni.g <- as.data.frame(x.crep[,"Green_NI"])
      ni.r <- as.data.frame(x.crep[,"Red_NI"])
      colnames(ni.g) <- ("Green_NI")
      colnames(ni.r) <- ("Red_NI")
      x.data <- new("Qdata",cgData = ni.g, crData = ni.r)
      conf1 <- QDConf(x.data)
      conf2 <- as.data.frame(slot(conf1,"CIData"))
      #rownames(conf2) <- c("Conf_G_NI","Conf_R_NI")
      conf2

    }

  })

  cor_new <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni_green <- as.data.frame(x.crep[,"Green_NI"])
      ni_red <- as.data.frame(x.crep[,"Red_NI"])
      colnames(ni_green) <- ("Green_NI")
      colnames(ni_red) <- ("Red_NI")
      concn <- as.data.frame(x.crep[,"Concn"])
      colnames(concn) <- ("Concentration")
      x.data <- new("Qdata",cgData = ni_green, crData = ni_red,
                    Concentration = concn)

      cor1 <- QDCor_Cal(x.data)

      cor2 <- as.data.frame(slot(cor1,"corData"))
      #rownames(cor2) <- c("Cor_G_NI","Cor_R_NI")
      #colnames(cor2) <- ("Correlation")
      cor2

    }

  })

  lod_new <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni_green <- as.data.frame(x.crep[,"Green_NI"])
      colnames(ni_green) <- ("Green_NI")
      ni_red <- as.data.frame(x.crep[,"Red_NI"])
      colnames(ni_red) <- ("Red_NI")
      sd.g <- as.data.frame(x.crep[,"NI.sd.green"])
      colnames(sd.g) <- ("Green_NI")
      sd.r <- as.data.frame(x.crep[,"NI.sd.red"])
      colnames(sd.r) <- ("Red_NI")

      x.data <- new("Qdata",cgData = ni_green, crData = ni_red,
                    sdgData = sd.g, sdrData = sd.r)

      lod1 <- QDLOD_calib(x.data)
      lod2 <- as.data.frame(slot(lod1,"lobData"))
      lod2
    }
  })

  lod_new2 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni_green <- as.data.frame(x.crep[,"Green_NI"])
      colnames(ni_green) <- ("Green_NI")
      ni_red <- as.data.frame(x.crep[,"Red_NI"])
      colnames(ni_red) <- ("Red_NI")
      sd.g <- as.data.frame(x.crep[,"NI.sd.green"])
      colnames(sd.g) <- ("Green_NI")
      sd.r <- as.data.frame(x.crep[,"NI.sd.red"])
      colnames(sd.r) <- ("Red_NI")

      x.data <- new("Qdata",cgData = ni_green, crData = ni_red,
                    sdgData = sd.g, sdrData = sd.r)

      lod2 <- QDLOD_calib(x.data, LOB = TRUE)

      lod3 <- as.data.frame(slot(lod2,"lobData"))

      lod3

    }

  })

  concentration <- reactive({

    conc <- QplexConc(yint(),mslp(),cint())
    conc

  })

  output$tabset <- renderUI({

    if(is.null(input$file)) {

      tabPanel("No input detected")

    }

    else {

      tabsetPanel(

        tabPanel("Initial Data", tableOutput("file")),

        tabPanel("Combine replicates", tableOutput("crep")),

        tabPanel("QD_G_plot", plotOutput("kd.g", height = 600, width = 550)),

        tabPanel("QD_R_plot", plotOutput("kd.r", height = 600, width = 550)),

        tabPanel("Confidence Interval",tableOutput("conf_new")),

        tabPanel("Correlation", tableOutput("cor_new")),

        tabPanel("LOD_First Method", tableOutput("lod_new")),

        tabPanel("LOD_Second Method", tableOutput("lod_new2")),

        tabPanel("Concentration", tableOutput("concentration"))

      )

    }

  })

  output$file <- renderTable({

    x <- file()

    x

  })

  output$crep <- renderTable({

    x <- crep()

    x

  })

  output$kd.g <- renderPlot({kd.g()})

  output$kd.r <- renderPlot({kd.r()})

  output$conf_new <- renderTable({

    x <- conf_new()

    x

  })

  output$cor_new <- renderTable({

    x <- cor_new()

    x

  })

  output$lod_new <- renderTable({

    x <- lod_new()

    x

  })

  output$lod_new2 <- renderTable({

    x <- lod_new2()

    x

  })

  output$concentration <- renderTable({

    x <- concentration()

    x

  })

  output$download <- downloadHandler(

    filename  = "result_report.html",

    content <- function(file) {

      knitr:::knit(input = "result_report.Rmd",

                   output = "result_report.md", quiet = TRUE)

      markdown:::markdownToHTML("result_report.md", "result_report.html")

    }

  )

})
