library(shiny)

library(Qplex)

shinyServer(function(input,output){

  ftype <- reactive({input$filetype})

  dp1 <- reactive({input$decp1})

  dp2 <- reactive({input$decp2})

  yint <- reactive({input$yin})

  mslp <- reactive({input$mslp})

  cint <- reactive({input$cint})

  file1 <- reactive({

    if(is.null(input$file1)){

      infile <- NULL

    }
    else{

      infile <- readQD(input$file1$datapath, type = ftype(),decp = dp1())

      x.initdata <- as.data.frame(slot(infile,"initialData"))

      x.initdata
    }

  })

  file2 <- reactive({

    if(is.null(input$file2)){

      infile <- NULL

    }
    else{

      infile <- readQD(input$file2$datapath, type = ftype(),decp = dp2())

      x.initdata <- as.data.frame(slot(infile,"initialData"))

      x.initdata
    }

  })

  crep <- eventReactive(input$compute,{

    runif(input$file2)

    isolate({

      if((!(is.null(file1))) && (!(is.null(file2)))){

        x.init1 <- file1()
        x.init2 <- file2()
        x.data1 <- new("Qdata",initialData = x.init1)
        x.data2 <- new("Qdata", initialData = x.init2)
        x.data3 <- ReadQD_combine(x.data1,x.data2)
        x.crep <- Repsd(x.data3, calib = FALSE)
        x.nsg1 <- as.data.frame(slot(x.crep,"cgData"))
        x.nsg <- x.nsg1[,"Green_NI"]
        x.sdg1 <- as.data.frame(slot(x.crep,"sdgData"))
        x.sdg <- x.sdg1[,"Green_NI"]
        x.nsr1 <- as.data.frame(slot(x.crep,"crData"))
        x.nsr <- x.nsr1[,"Red_NI"]
        x.sdr1 <- as.data.frame(slot(x.crep,"sdrData"))
        x.sdr <- x.sdr1[,"Red_NI"]
        x.concn_g <- as.data.frame(slot(x.crep,"Concentration_CRP"))
        x.concn1_g <- nrow((!(is.na(x.concn_g))))
        x.concn2_g <- x.concn_g[1:x.concn1_g,]

        x.concn_r <- as.data.frame(slot(x.crep,"Concentration_IL6"))
        x.concn1_r <- nrow((!(is.na(x.concn_r))))
        x.concn2_r <- x.concn_r[1:x.concn1_r,]

        x.rep <- as.data.frame(cbind(x.nsg,x.sdg,x.nsr,x.sdr,
                                     x.concn2_g,x.concn2_r))
        names(x.rep) <- c("Green_NI","NI.sd.green","Red_NI","NI.sd.red",
                           "Conc_CRP_Green","Conc2_IL6_Red")
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
      conc <- x.crep[,"Conc_CRP_Green"]
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

  kd.g_15 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni.g1 <- as.data.frame(x.crep[,"Green_NI"])
      ni.g <- ni.g1[1:(nrow(ni.g1)-1),]
      sd.g1 <- as.data.frame(x.crep[,"NI.sd.green"])
      sd.g <- sd.g1[1:(nrow(sd.g1)-1),]
      conc1 <- as.data.frame(x.crep[,"Conc_CRP_Green"])
      conc <- conc1[1:(nrow(ni.g1)-1),]
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

  kd.g_10 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni.g1 <- as.data.frame(x.crep[,"Green_NI"])
      ni.g <- ni.g1[1:(nrow(ni.g1)-2),]
      sd.g1 <- as.data.frame(x.crep[,"NI.sd.green"])
      sd.g <- sd.g1[1:(nrow(sd.g1)-2),]
      conc1 <- as.data.frame(x.crep[,"Conc_CRP_Green"])
      conc <- conc1[1:(nrow(conc1)-2),]
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
      conc <- x.crep[,"Conc2_IL6_Red"]
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

  kd.r_15 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{
      x.crep <- crep()
      ni.r1 <- as.data.frame(x.crep[,"Red_NI"])
      ni.r <- ni.r1[2:nrow(ni.r1),]
      sd.r1 <- as.data.frame(x.crep[,"NI.sd.red"])
      sd.r <- sd.r1[2:nrow(sd.r1),]
      conc1 <- as.data.frame(x.crep[,"Conc2_IL6_Red"])
      conc <- conc1[2:nrow(conc1),]
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

  kd.r_10 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{
      x.crep <- crep()
      ni.r1 <- as.data.frame(x.crep[,"Red_NI"])
      ni.r <- ni.r1[3:nrow(ni.r1),]
      sd.r1 <- as.data.frame(x.crep[,"NI.sd.red"])
      sd.r <- sd.r1[3:nrow(sd.r1),]
      conc1 <- as.data.frame(x.crep[,"Conc2_IL6_Red"])
      conc <- conc1[3:nrow(conc1),]
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

  conf_new_15 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni.g1 <- as.data.frame(x.crep[,"Green_NI"])
      ni.g <- as.data.frame(ni.g1[1:(nrow(ni.g1)-1),])
      ni.r1 <- as.data.frame(x.crep[,"Red_NI"])
      ni.r <- as.data.frame(ni.r1[2:nrow(ni.r1),])
      colnames(ni.g) <- ("Green_NI")
      colnames(ni.r) <- ("Red_NI")
      x.data <- new("Qdata",cgData = ni.g, crData = ni.r)
      conf1 <- QDConf(x.data)
      conf2 <- as.data.frame(slot(conf1,"CIData"))
      #rownames(conf2) <- c("Conf_G_NI","Conf_R_NI")
      conf2

    }

  })

  conf_new_10 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni.g1 <- as.data.frame(x.crep[,"Green_NI"])
      ni.g <- as.data.frame(ni.g1[1:(nrow(ni.g1)-2),])
      ni.r1 <- as.data.frame(x.crep[,"Red_NI"])
      ni.r <- as.data.frame(ni.r1[3:nrow(ni.r1),])
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
      concn1 <- as.data.frame(x.crep[,"Conc_CRP_Green"])
      concn2 <- as.data.frame(x.crep[,"Conc2_IL6_Red"])
      #colnames(concn1) <- ("Conc_CRP_Green")
      #colnames(concn2) <- ("Conc2_IL6_Red")
      x.data <- new("Qdata",cgData = ni_green, crData = ni_red,
                    Concentration_CRP = concn1, Concentration_IL6 = concn2)

      cor1 <- QDCor_exp(x.data)

      cor2 <- as.data.frame(slot(cor1,"corData_exp"))
      #rownames(cor2) <- c("Cor_G_NI","Cor_R_NI")
      #colnames(cor2) <- ("Correlation")
      cor2

    }

  })

  cor_new_15 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni_green1 <- as.data.frame(x.crep[,"Green_NI"])
      ni_green <- as.data.frame(ni_green1[1:(nrow(ni_green1)-1),])
      ni_red1 <- as.data.frame(x.crep[,"Red_NI"])
      ni_red <- as.data.frame(ni_red1[2:nrow(ni_red1),])
      colnames(ni_green) <- ("Green_NI")
      colnames(ni_red) <- ("Red_NI")
      concn_1 <- as.data.frame(x.crep[,"Conc_CRP_Green"])
      concn1 <- as.data.frame(concn_1[1:(nrow(concn_1)-1),])
      concn_2 <- as.data.frame(x.crep[,"Conc2_IL6_Red"])
      concn2 <- as.data.frame(concn_2[2:nrow(concn_2),])
      x.data <- new("Qdata",cgData = ni_green, crData = ni_red,
                    Concentration_CRP = concn1, Concentration_IL6 = concn2)

      cor1 <- QDCor_exp(x.data)

      cor2 <- as.data.frame(slot(cor1,"corData_exp"))
      #rownames(cor2) <- c("Cor_G_NI","Cor_R_NI")
      #colnames(cor2) <- ("Correlation")
      cor2

    }

  })

  cor_new_10 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      ni_green1 <- as.data.frame(x.crep[,"Green_NI"])
      ni_green <- as.data.frame(ni_green1[1:(nrow(ni_green1)-2),])
      ni_red1 <- as.data.frame(x.crep[,"Red_NI"])
      ni_red <- as.data.frame(ni_red1[3:nrow(ni_red1),])
      colnames(ni_green) <- ("Green_NI")
      colnames(ni_red) <- ("Red_NI")
      concn_1 <- as.data.frame(x.crep[,"Conc_CRP_Green"])
      concn1 <- as.data.frame(concn_1[1:(nrow(concn_1)-2),])
      concn_2 <- as.data.frame(x.crep[,"Conc2_IL6_Red"])
      concn2 <- as.data.frame(concn_2[3:nrow(concn_2),])
      #colnames(concn1) <- ("Conc_CRP_Green")
      #colnames(concn2) <- ("Conc2_IL6_Red")
      x.data <- new("Qdata",cgData = ni_green, crData = ni_red,
                    Concentration_CRP = concn1, Concentration_IL6 = concn2)

      cor1 <- QDCor_exp(x.data)

      cor2 <- as.data.frame(slot(cor1,"corData_exp"))
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

      lod1 <- QDLOD_exp(x.data)
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

      lod2 <- QDLOD_exp(x.data, LOB = TRUE)

      lod3 <- as.data.frame(slot(lod2,"lobData"))

      lod3

    }

  })

  concentration <- reactive({

    conc <- QplexConc(yint(),mslp(),cint())
    conc

  })

  output$tabset <- renderUI({

    if((is.null(input$file1)) || (is.null(input$file2))) {

      tabPanel("No input detected")

    }

    else {

      tabsetPanel(

        tabPanel("Initial Data_file1", tableOutput("file1")),

        tabPanel("Initial Data_file2", tableOutput("file2")),

        tabPanel("Combine replicates", tableOutput("crep")),

        tabPanel("QD_G_plot", plotOutput("kd.g", height = 600, width = 550)),

        tabPanel("QD_G_plot_15", plotOutput("kd.g_15", height = 600, width = 550)),

        tabPanel("QD_G_plot_10", plotOutput("kd.g_10", height = 600, width = 550)),

        tabPanel("QD_R_plot", plotOutput("kd.r", height = 600, width = 550)),

        tabPanel("QD_R_plot_15", plotOutput("kd.r_15", height = 600, width = 550)),

        tabPanel("QD_R_plot_10", plotOutput("kd.r_10", height = 600, width = 550)),

        tabPanel("Confidence Interval",tableOutput("conf_new")),

        tabPanel("Confidence Interval_15",tableOutput("conf_new_15")),

        tabPanel("Confidence Interval_10",tableOutput("conf_new_10")),

        tabPanel("Correlation", tableOutput("cor_new")),

        tabPanel("Correlation_15", tableOutput("cor_new_15")),

        tabPanel("Correlation_10", tableOutput("cor_new_10")),

        tabPanel("LOD_First Method", tableOutput("lod_new")),

        tabPanel("LOD_Second Method", tableOutput("lod_new2")),

        tabPanel("Concentration", tableOutput("concentration"))

      )

    }

  })

  output$file1 <- renderTable({

    x <- file1()

    x

  })

  output$file2 <- renderTable({

    x <- file2()

    x

  })

  output$crep <- renderTable({

    x <- crep()

    x

  })

  output$kd.g <- renderPlot({kd.g()})

  output$kd.g_15 <- renderPlot({kd.g_15()})

  output$kd.g_10 <- renderPlot({kd.g_10()})

  output$kd.r <- renderPlot({kd.r()})

  output$kd.r_15 <- renderPlot({kd.r_15()})

  output$kd.r_10 <- renderPlot({kd.r_10()})

  output$conf_new <- renderTable({

    x <- conf_new()

    x

  })

  output$conf_new_15 <- renderTable({

    x <- conf_new_15()

    x

  })

  output$conf_new_10 <- renderTable({

    x <- conf_new_10()

    x

  })

  output$cor_new <- renderTable({

    x <- cor_new()

    x

  })

  output$cor_new_15 <- renderTable({

    x <- cor_new_15()

    x

  })

  output$cor_new_10 <- renderTable({

    x <- cor_new_10()

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
