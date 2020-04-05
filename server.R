library(shiny)
library(shinydashboard)
library(RxODE)
library(ggplot2)
library(shinyWidgets)
library(rmarkdown)
library(gdata)
library(tools)
library(reshape2)
library(scales)
library(dplyr)
library(egg)
library(grid)
library(shinyhelper)
library(MASS)

#---------------------initalize plotting------------------------
pl <- ggplot()+
  theme_bw()+
  theme(axis.title.x=element_text(face='bold',size=15),
        axis.title.y=element_text(face='bold',size=15),
        axis.text.x=element_text(face='bold',size=10,color='black'),
        axis.text.y=element_text(face='bold',size=10,color='black'))
pk  <- pl+xlim (0,100) + ylim (0,100) + ylab("Concentration (ug/ml)") + xlab("Time (day)")
p1  <- pl+xlim (0,100) + ylim (0,100) + ylab('Trough Concentration / EC50')+theme(axis.title.x = element_blank())
p2  <- pl+xlim (0,100) + ylim (0,100)+ xlab("Time (day)")


server <- function(input,output){
  observe_helpers()
  source("www/collapse_box.R", local = TRUE)
  
  r<- eventReactive(input$plot,{
    
    #------- Plotting Progress Bar-------
    withProgress(message = 'Plotting in progress',
                 detail = 'This may take a while...', value = 0, {
  
    # plotting units
    tu <- 24 #switch(input$timeunit, hour = 1, day = 24, week = 24*7)
    cu <- 1 #switch(input$concunit, 'mg/ml' = 10^3, 'ug/ml' = 1, 'ng/ml' = 10^-3) 
  
    
    if (input$drugname == ""){
      pk  <- pk
      p1  <- p1
      p2  <- p2
    }else{
      
      WT <- input$bw
      
      if(input$drugname == "Chloroquine"){
        
        amt1_1 <- input$amt1_1_1
        amt1_2 <- input$amt1_2_1
        amt2_1 <- input$amt2_1_1
        amt2_2 <- input$amt2_2_1
        D1     <- input$D1_1/24
        ii1    <- input$ii1_1
        D2     <- input$D2_1
        ii2    <- input$ii2_1
        
      }else if(input$drugname == "Hydroxychloroquine"){
        
        amt1_1 <- input$amt1_1_2*155/200
        amt1_2 <- input$amt1_2_2*155/200
        amt2_1 <- input$amt2_1_2*155/200
        amt2_2 <- input$amt2_2_2*155/200
        D1     <- input$D1_2/24
        ii1    <- input$ii1_2
        D2     <- input$D2_2
        ii2    <- input$ii2_2
        
      }else{
        
        amt1_1 <- input$amt1_1_3
        amt1_2 <- input$amt1_2_3
        amt2_1 <- input$amt2_1_3
        amt2_2 <- input$amt2_2_3
        D1     <- input$D1_3/24
        ii1    <- input$ii1_3
        D2     <- input$D2_3
        ii2    <- input$ii2_3
        
      }

      if (input$bwd == "yes"){
        amount1 <- WT*amt1_1
        amount2 <- WT*amt2_1
      }else{
        amount1 <- amt1_2
        amount2 <- amt2_2
      }
      
      if(amount1 == 0 & amount2 == 0 ){
        pk  <- pk
        p1  <- p1
        p2  <- p2
      }else{
        #----------------dosing and sampling-----------------------
        D1 <- max(D1,ii1/24)
        D2 <- max(D2,ii2/24)
        
        nbr1 <- (D1*24)%/%ii1
        nbr2 <- (D2*24)%/%ii2
        
        ev1 <- eventTable()
        if (amount1 >0) {
          if (nbr1 > 1){
          ev1$add.dosing(dose = amount1,start.time = 0,nbr.doses = nbr1,dosing.interval = ii1,dosing.to = "depot")
          }else{
            ev1$add.dosing(dose = amount1,start.time = 0,dosing.to = "depot")
          }
        }
        if (amount2 >0) {
          if (nbr2 >1){
          ev1$add.dosing(dose = amount2,start.time = D1*24,nbr.doses = nbr2,dosing.interval = ii2,dosing.to = "depot")
          }else{
            ev1$add.dosing(dose = amount2,start.time = D1*24,dosing.to = "depot")
          }
        }
        
        ev1$add.sampling(seq(0,(D1+D2)*24, 1))
        
        
        fu <- switch(input$drugname,
                     "Chloroquine" = 0.4,
                     "Hydroxychloroquine" = 0.5,
                     "Lopinavir + Ritonavir" = 0.0145)
        
        if (input$drugname == "Chloroquine"){

          mod_CQ1 <- RxODE("
                           CL = (p_CL * (WT/M_WT)^0.75)* exp(eta_CL)   ;# Clearance normalized for bioavailability
                           Q2 =  p_Q2 * (WT/M_WT)^0.75;
                           V1 = (p_V1 * (WT/M_WT)^1) * exp(eta_V1)   ;# central compartment
                           V2 =  p_V2 * (WT/M_WT)^1                          ;# first peripheral compartment
                           C1 = centr/V1                             ;# C plasma~
                           C2 = peri/V2                              ;
                           Cf = C1*fu;
                           Clung = effect;
                           Rl = Clung/EC50;
                           Rp = C1/EC50;
                           Rf = Cf/EC50;
                           
                           d/dt(depot)   = -Ka*depot;
                           lag(depot)    = TLAG     ;
                           d/dt(centr)   = Ka*depot-CL*C1-Q2*C1+Q2*C2;
                           d/dt(peri)    = Q2*C1-Q2*C2 ;
                           d/dt(effect) = ke0*C1-ke0*effect/Kp;
                           ")
          # Theta
          EC50 <- switch(input$ec50_1,
                         "SARS-cov-2 1.75ug/ml (Yao et al. CID(2020))" = 1.75,
                         "SARS-cov-2 0.36ug/ml (Wang et al. Cell Res(2020))" = 0.36,
                         "SARS-cov-2 2.33ug/ml (Jeon et al. bioRxiv(2020))" = 2.33,
                         "SARS-cov-1 2.81ug/ml (Keyaerts E et al. BBRC(2004))" = 2.81,
                         "SARS-cov-1 1.41ug/ml (Vincent MJ et al. Virol J(2005))" = 1.41,
                         "SARS-cov-1 1.31ug/ml (de Wilde AH et al. AAC(2014))" = 1.31,
                         "Mers-cov 0.96ug/ml (de Wilde AH et al. AAC(2014))" = 0.96)

          par1 <- c(Ka   = 6.12,
                    TLAG = 0.387,
                    p_CL  = 59.1, 
                    p_V1  = 2870, 
                    p_V2  = 1890,
                    p_Q2  = 61.4,
                    WT = WT, 
                    M_WT = 70,
                    EC50 = EC50,
                    fu = fu,
                    ke0 = 6.166944,
                    Kp = 171.847380)
          # Omega
          omega <- diag(2)
          diag(omega) <- c(0.093, 0.217)
          dimnames(omega) <- list(NULL,c("eta_CL","eta_V1"))
          # Inits
          inits <- c(depot=0,centr=0,peri=0,effect=0)
          sim_p <- solve(mod_CQ1, par1, ev1, inits, omega=omega, nSub=100, seed = 12)
          
        }else if (input$drugname == "Hydroxychloroquine"){
          #-------------HCQ-----------
          mod_HCQ2 <- RxODE("
                            CL = (TV_CL * (WT/M_WT)^F_CL) * exp(eta_CL);
                            V1 = (TV_V1 * (WT/M_WT)^F_V1) * exp(eta_V1);
                            V2 = (TV_V2 * (WT/M_WT)^F_V2) * exp(eta_V2) ;
                            Q = TV_Q * (WT/M_WT)^F_Q;
                            C1 = centr/V1;
                            C2 = peri/V2;
                            Cf = C1*fu;
                            Clung = effect;
                            Rl = Clung/EC50;
                            Rp = C1/EC50;
                            Rf = Cf/EC50;
                            d/dt(depot) = - ka*depot;
                            lag(depot) = Tlag * exp(eta_Tlag);
                            d/dt(centr) = ka*depot - CL*C1 - Q*C1 + Q*C2;
                            d/dt(peri) = - Q*C2 + Q*C1;
                            d/dt(effect) = ke0*C1-ke0*effect/Kp;
                            ")
          
          EC50 = 0.24
          
          params <- c(TV_CL = 10.9, 
                      TV_V1  = 437, 
                      TV_V2  = 1390, 
                      TV_Q = 45.1, 
                      F_CL = 0.75, 
                      F_V1 = 1.0, 
                      F_Q = 0.75, 
                      F_V2 = 1.0,
                      ka = 1.15, 
                      Tlag = 0.389,
                      WT = WT, 
                      M_WT = 66.76,
                      EC50 = EC50,
                      fu = fu,
                      ke0 = 6.166944,
                      Kp = 171.847380*0.2892984)
          omega <- diag(4);
          diag(omega) <- c(0.161, 0.232, 0.715, 0.0359)
          dimnames(omega) <- list(NULL, c("eta_CL", "eta_V1", "eta_V2", "eta_Tlag"))

          inits <- c(depot=0, centr=0, peri=0,effect = 0)
          sim_p <- solve(mod_HCQ2, params, ev1, inits, omega=omega, nSub=100, seed = 12)
          
        }else{
          
          if (input$bwd == "yes"){
            amount1_2 <- WT*input$amt1_1_4
            amount2_2 <- WT*input$amt2_1_4
          }else{
            amount1_2 <- input$amt1_2_4
            amount2_2 <- input$amt2_2_4
          }

          nsub <- 100
          
          ##### Ritonavir part #############
          # correlation to covariance 
          omega_rtv <- matrix(c(0.1467, 0, 0, 0, 0.64, 1.1735, 0, 1.1735, 2.8561), nrow = 3, 
                              dimnames=list(c("eta_CL", "eta_V", "eta_ka"), c("eta_CL", "eta_V", "eta_ka")))
          # IIV covariance matrix
          
          #  parameters or covariates
          LPV <- 1    # 1 for individuals using lopinavir
          TV_CL_rtv <- 10.5
          CL_LPV = 2.72
          
          # Dose for RTV: 100 mg twice a day for 14 days
          Dose_rtv <- amount1_2       # mg, mantainance dose for ritonavir
          
          ##### Lopinavir part #############
          # 1-cmt model for Lopinavir
          mod_lpv1 <- RxODE("
                            C1 = centr/V;
                            Cf = C1*fu;
                            Kp = 0.51;
                            Clung = Kp*C1;
                            Rl = Clung/EC50;
                            Rp = C1/EC50;
                            Rf = Cf/EC50;
                            d/dt(depot) = - ka*depot;
                            d/dt(centr) = ka*depot - CL*C1"
          )
          
          # correlation to covariance
          omega_lpv <- matrix(c(0.9565, -0.0449, 0.5129, -0.0449, 0.0296, 0.0266, 0.5129, 0.0266, 0.4070), nrow = 3, 
                              dimnames=list(c("eta_CL", "eta_V", "eta_ka"), c("eta_CL", "eta_V", "eta_ka")))   # IIV covariance matrix
          
          # initial states
          inits_lpv <- c(depot=0, centr=0)
          
          # parameters or covariates
          IND <- 1    # without treatment with efavirenz or nevirapine, otherwise = 1.39
          Imax <- 1
          AUC50 <- 2.26
          
          TV_ka <- 0.564
          TV_CL <- 14.8
          TV_V <- 61.6
          TV_F <- 1
          fu <- 0.0145
          EC50 <- switch(input$ec50_3,
                         "SARS-cov-2 5.73ug/ml (Jeon et al. bioRxiv(2020))" = 5.73,
                         "SARS-cov-1 4ug/ml (Chu CM et al. Thorax(2004))" = 4,
                         "SARS-cov-1 10.75ug/ml (de Wilde AH et al. AAC(2014))" = 10.75,
                         "Mers-cov 5.03ug/ml (de Wilde AH et al. AAC(2014))" = 5.03)
          
          ####### sample F for each individual and each dose event #####
          # Information from shiny, changable
          # dosing schedule for lopinarvir
          # duration
          duration_L <- D1*24 # days - hours, for loading doses
          duration_M <- D2*24 # days - hours, for maintanance doses
          duration <- duration_L + duration_M   # 7 days
          
          # dose interval and nbr.dose
          ii_L <- ii1
          ii_M <- ii2
          ndose_L <- duration_L/ii_L
          ndose_M <- duration_M/ii_M
          ii <- rep(c(ii_L, ii_M), c(ndose_L, ndose_M))
          ndose <- ndose_L + ndose_M    # ndose = 9
          
          # amount
          amt_L <- amount1  # loading 
          amt_M <- amount2  # maintainance
          amt <- rep(c(amt_L, amt_M), c(ndose_L, ndose_M))
          
          # Dose for RTV: 100 mg twice a day for 14 days
          Dose_rtv <- amount1_2       # mg, mantainance dose for ritonavir
          
          
          # IIV and IOV sampling
          set.seed(0403)
          pi_lpv <- rnorm(n=nsub*ndose, 0, 0.175)  # Sample for IOV
          count <- 0
          
          mv_rtv <- mvrnorm(n=nsub, rep(0,3), omega_rtv) # IIV sampling for ritonavir: CL
          CL_rtv = TV_CL_rtv * CL_LPV ^ LPV * exp(mv_rtv[,1])
          AUC_rtv = Dose_rtv/CL_rtv
          I_rtv = 1 - (Imax * AUC_rtv / (AUC50 + AUC_rtv))
          
          mv_lpv <- mvrnorm(n=nsub, rep(0,3), omega_lpv) # IIV Sampling for lopinariv: ka, CL and V
          
          ######### simulation test ############
          
          sim_p <- NULL
          for (i in 1:nsub){
            start <- 0
            # sample one IIV for CL/V/Ka, one IOV for F and one CL_rtv
            ka <- TV_ka * exp(mv_lpv[i,1])
            CL <- TV_CL * I_rtv[i] * IND * exp(mv_lpv[i,2])
            V <- TV_V * exp(mv_lpv[i,3])
            pars_lpv <- cbind(ka, CL, V, fu, EC50)
            
            F1 <- 1 * exp(pi_lpv[i])
            ev1 <- eventTable() %>% add.sampling(0:duration) 
            for (j in 1:ndose){
              # sample one F for each dose event
              count <- count+1
              F1 <- 1 * exp(pi_lpv[count])
              
              ev1 <- ev1 %>% add.dosing(dose = amt[j] * F1, start.time = start, dosing.to = "depot")
              start <- start + ii[j]
            }
            x <- solve(mod_lpv1, pars_lpv, ev1, inits_lpv)
            x <- cbind(x, sim.id = i) 
            sim_p <- rbind(sim_p, x)
          }
        }
        incProgress(1/3)
        
        
        #------------------plotting plasma concentration-------------------
        toxi <- switch(input$drugname,
                       "Chloroquine" = 0.52,
                       "Hydroxychloroquine" = 3.14,
                       "Lopinavir + Ritonavir" = 45.37)
        
        # 95% CI
        sim_qt_p <- data.frame(time=NULL, qt_025=NULL, qt_50=NULL, qt_975=NULL)
        # for plasma
        for (i in 1:length(unique(sim_p$time))-1){
          sim_time <- sim_p[sim_p$time==i,]
          qt <- quantile(sim_time$C1, prob = c(.025, .5, .975))
          sim_qt1 <- data.frame(time=i, qt_025=qt[1], qt_50=qt[2], qt_975=qt[3])
          sim_qt_p <- rbind(sim_qt_p, sim_qt1)
        }
        
        sim_qt_p <- cbind(sim_qt_p, site = "Total Plasma",toxi = toxi,EC50 = EC50)
        
        # 95% CI
        sim_qt_f <- data.frame(time=NULL, qt_025=NULL, qt_50=NULL, qt_975=NULL)
        # for free plasma
        for (i in 1:length(unique(sim_p$time))-1){
          sim_time <- sim_p[sim_p$time==i,]
          qt <- quantile(sim_time$Cf, prob = c(.025, .5, .975))
          sim_qt1 <- data.frame(time=i, qt_025=qt[1], qt_50=qt[2], qt_975=qt[3])
          sim_qt_f <- rbind(sim_qt_f, sim_qt1)
        }

        sim_qt_f <- cbind(sim_qt_f, site = "Free Plasma",toxi = fu*toxi,EC50 = EC50)
        
        # 95% CI
        sim_qt_l <- data.frame(time=NULL, qt_025=NULL, qt_50=NULL, qt_975=NULL)
        # for lung
        for (i in 1:length(unique(sim_p$time))-1){
          sim_time <- sim_p[sim_p$time==i,]
          qt <- quantile(sim_time$Clung, prob = c(.025, .5, .975))
          sim_qt1 <- data.frame(time=i, qt_025=qt[1], qt_50=qt[2], qt_975=qt[3])
          sim_qt_l <- rbind(sim_qt_l, sim_qt1)
        }
        
        sim_qt_l <- cbind(sim_qt_l, site = "Lung",toxi = NA,EC50 = EC50)
        
        sim_qt_1 <- rbind(sim_qt_p,sim_qt_l)
        sim_qt_2 <- rbind(sim_qt_f,sim_qt_l)
        
        sim_qt <- switch(input$free, "total" = sim_qt_1, "free" = sim_qt_2)
        
        pk <- ggplot(data=sim_qt,aes(x=time/tu))+
          facet_wrap(~site, nrow = 2)+
          geom_line(aes(y=qt_50/cu), color = "#f46e32",linetype=1, size=1)+
          geom_ribbon(aes(ymin=qt_025/cu, ymax=qt_975/cu), fill = "#f46e32", alpha = 0.2)+
          geom_hline(aes(yintercept = toxi), linetype="dashed", size = 1, color = "#D62728")+
          geom_hline(aes(yintercept = EC50), linetype = "dashed", size = 1, color = "#2CA02C")+
          geom_text(aes(max(time/tu), toxi, hjust = 1,vjust = 1.5), color = "#D62728",label = "MTD",size = 5)+
          geom_text(aes(max(time/tu), EC50, hjust = 1,vjust = -0.5), color = "#2CA02C",label = "EC50",size = 5)+
          xlab("Time (day)")+
          ylab("Concentration (ug/ml)")+
          theme_bw()+
          theme(legend.position = "none")+
          scale_x_continuous(breaks = seq(0,(D1+D2)*24/tu, by = 24/tu),limits = c(0,(D1+D2)*24/tu))+
          scale_y_log10()+ 
          theme(axis.title.x = element_text(face = 'bold', size = 15),
                axis.title.y = element_text(face = 'bold', size = 15),
                axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
                axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
                strip.text.x = element_text(face = 'bold', size = 10),
                plot.caption = element_text(size = 15, color = "#D62728", face = "bold.italic"))+
          labs(caption = "MTD: Maximum tolerated dose threshold")
        

        incProgress(1/3)
        #--------------------------plotting PD----------------------------
        
        fold1 <- 1
        fold2 <- 2
        fold3 <- 5
        fold4 <- 10
        fold5 <- 50
        fold6 <- 100
        
        #------------- total plasma / EC50-------------
        Rpmin <- NULL
        
        sum_hm_p <- data.frame(time = NULL, bin = NULL, perc = NULL)
        sum_toxi_p <- data.frame(time = NULL, bin = NULL, perc = NULL)
        
        for (i in (1:ceiling(D1+D2))*24){
          if (i == 24) {
            Rpmin <- sim_p[sim_p$time==ii1,]$Rp
          }else{
            
            for (k in 1:100){
              Rpmin[k] <- min(sim_p[sim_p$sim.id == k & sim_p$time > (i-24) & sim_p$time <= i,]$Rp)
            }
          }
          
          perc2 <- sum(Rpmin>=fold1)/100
          perc3 <- sum(Rpmin>=fold2)/100
          perc4 <- sum(Rpmin>=fold3)/100
          perc5 <- sum(Rpmin>=fold4)/100
          perc6 <- sum(Rpmin>=fold5)/100
          perc7 <- sum(Rpmin>=fold6)/100
          
          count <- 0
          for (j in 1:100){
            if (max(sim_p[sim_p$sim.id == j & sim_p$time > (i-24) & sim_p$time <= i,]$C1) >= toxi){
              count <- count + 1
            }else{
              count <- count
            }
          }
          
          sum1 <- data.frame(time = rep(i,6)/24, 
                             bin = c(fold1,fold2,fold3,fold4,fold5,fold6), 
                             perc = c(perc2,perc3,perc4,perc5,perc6,perc7))
          sum2 <- data.frame(time = i/24, bin = "MTD", perc = count/100)
          
          sum_hm_p <- rbind(sum_hm_p,sum1)
          sum_toxi_p <- rbind(sum_toxi_p,sum2)
        }
        sum_hm_p <- melt(sum_hm_p,id.vars = c("time","bin"),value.name = "perc")
        sum_hm_p$time <- as.factor(sum_hm_p$time)
        sum_hm_p$bin <- as.factor(sum_hm_p$bin)
        sum_hm_p <- cbind(sum_hm_p, site = "Total Plasma")
        
        sum_toxi_p$time <- as.factor(sum_toxi_p$time)
        sum_toxi_p$bin <- as.factor(sum_toxi_p$bin)
        sum_toxi_p <- cbind(sum_toxi_p, site = "Total Plasma")
        
        #----- free conc/EC50 --------
        
        Rfmin <- NULL
        
        sum_hm_f <- data.frame(time = NULL, bin = NULL,perc = NULL)
        sum_toxi_f <- data.frame(time = NULL, bin =NULL, perc = NULL)
        
        for (i in (1:ceiling(D1+D2))*24){
          if (i == 24) {
            Rfmin <- sim_p[sim_p$time==ii1,]$Rf
          }else{
            
            for (k in 1:100){
              Rfmin[k] <- min(sim_p[sim_p$sim.id == k & sim_p$time > (i-24) & sim_p$time <= i,]$Rf)
            }
          }
          
          count <- 0
          for (j in 1:100){
            if (max(sim_p[sim_p$sim.id == j & sim_p$time > (i-24) & sim_p$time <= i,]$Cf) >= fu*toxi){
              count <- count + 1
            }else{
              count <- count
            }
          }
          
          perc2 <- sum(Rfmin>=fold1)/100
          perc3 <- sum(Rfmin>=fold2)/100
          perc4 <- sum(Rfmin>=fold3)/100
          perc5 <- sum(Rfmin>=fold4)/100
          perc6 <- sum(Rfmin>=fold5)/100
          perc7 <- sum(Rfmin>=fold6)/100
          
          sum1 <- data.frame(time = rep(i,6)/24, 
                             bin = c(fold1,fold2,fold3,fold4,fold5,fold6), 
                             perc = c(perc2,perc3,perc4,perc5,perc6,perc7))
          sum2 <- data.frame(time = i/24, bin = "MTD", perc = count/100)
          
          sum_hm_f <- rbind(sum_hm_f,sum1)
          sum_toxi_f <- rbind(sum_toxi_f,sum2)
        }
        sum_hm_f <- melt(sum_hm_f,id.vars = c("time","bin"),value.name = "perc")
        sum_hm_f$time <- as.factor(sum_hm_f$time)
        sum_hm_f$bin <- as.factor(sum_hm_f$bin)
        sum_hm_f <- cbind(sum_hm_f, site = "Free Plasma")
        
        sum_toxi_f$time <- as.factor(sum_toxi_f$time)
        sum_toxi_f$bin <- as.factor(sum_toxi_f$bin)
        sum_toxi_f <- cbind(sum_toxi_f, site = "Free Plasma")
        
        #----- lung conc / EC50 --------
        
        Rlmin <- NULL
        
        sum_hm_l <- data.frame(time = NULL, bin = NULL,perc = NULL)
        sum_toxi_l <- data.frame(time = NULL, bin = NULL, perc = NULL)
        
        for (i in (1:ceiling(D1+D2))*24){
          if (i == 24) {
            Rlmin <- sim_p[sim_p$time==ii1,]$Rl
          }else{
            
            for (k in 1:100){
              Rlmin[k] <- min(sim_p[sim_p$sim.id == k & sim_p$time > (i-24) & sim_p$time <= i,]$Rl)
            }
          }
          
          count <- 0
          for (j in 1:100){
            if (max(sim_p[sim_p$sim.id == j & sim_p$time > (i-24) & sim_p$time <= i,]$Clung) >= toxi){
              count <- count + 1
            }else{
              count <- count
            }
          }
          
          perc2 <- sum(Rlmin>=fold1)/100
          perc3 <- sum(Rlmin>=fold2)/100
          perc4 <- sum(Rlmin>=fold3)/100
          perc5 <- sum(Rlmin>=fold4)/100
          perc6 <- sum(Rlmin>=fold5)/100
          perc7 <- sum(Rlmin>=fold6)/100
          
          #perc0 <- sum(sim_hm$Rl>=toxi)/100
          
          sum1 <- data.frame(time = rep(i,6)/24, 
                             bin = c(fold1,fold2,fold3,fold4,fold5,fold6), 
                             perc = c(perc2,perc3,perc4,perc5,perc6,perc7))
          sum2 <- data.frame(time = i/24, bin = "MTD", perc = count/100)
          sum_hm_l <- rbind(sum_hm_l,sum1)
          sum_toxi_l <- rbind(sum_toxi_l,sum2)
        }
        sum_hm_l <- melt(sum_hm_l,id.vars = c("time","bin"),value.name = "perc")
        sum_hm_l$time <- as.factor(sum_hm_l$time)
        sum_hm_l$bin <- as.factor(sum_hm_l$bin)
        sum_hm_l <- cbind(sum_hm_l, site = "Lung")
        
        sum_toxi_l$time <- as.factor(sum_toxi_l$time)
        sum_toxi_l$bin <- as.factor(sum_toxi_l$bin)
        sum_toxi_l <- cbind(sum_toxi_l, site = "Lung")
        
        sum_hm_1 <- rbind(sum_hm_p,sum_hm_l)
        sum_hm_2 <- rbind(sum_hm_f,sum_hm_l)
        
        sum_toxi_1 <- rbind(sum_toxi_p, sum_toxi_l)
        sum_toxi_2 <- rbind(sum_toxi_f, sum_toxi_l)
        
        sum_hm <- switch(input$free, "total" = sum_hm_1, "free" = sum_hm_2)
        sum_toxi <- switch(input$free, "total" = sum_toxi_p, "free" = sum_toxi_f)
 
        p1 <- ggplot(sum_hm, aes(x = time, y = bin)) + 
          facet_wrap(~site, nrow = 2)+
          geom_tile(aes(fill = perc),color = "white") + 
          scale_fill_gradient(low = "#D6ECF0",high = "#2CA02C",limits = c(0,1),name = "Patients above threshold (%)",labels = percent)+
          ylab("Trough concentration / EC50")+
          theme_bw()+
          theme(panel.grid.major=element_line(colour=NA),
                legend.position="top")+
          theme(axis.title.x = element_blank(),
                axis.title.y = element_text(face = 'bold', size = 15),
                axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
                axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
                strip.text.x = element_text(face = 'bold', size = 10))+
          geom_text(aes(label=100*perc))
        
        p2 <- ggplot(data = sum_toxi, aes(x = time,y = bin))+
          facet_wrap(site~.)+
          theme_bw()+
          #geom_text(aes(label=count),position=position_dodge(width=0.9), vjust=-0.25)+
          #geom_bar(stat="identity",fill = "#D62728")+
          geom_tile(aes(fill = perc),color = "white") + 
          geom_text(aes(label=100*perc))+
          scale_fill_gradient(low = "#FCEFE8",high = "#D62728",limits = c(0,1),name = "Patients above MTD concentrations (%)",labels = percent)+
          #ylim(0,100)+
          xlab("Time (day)")+
          theme(panel.grid.major=element_line(colour=NA),
                legend.position="bottom",
                axis.title.x = element_text(face = 'bold', size = 15),
                axis.title.y = element_text(face = 'bold', size = 15),
                axis.text.x = element_text(face = 'bold', size = 10, color = 'black'),
                axis.text.y = element_text(face = 'bold', size = 10, color = 'black'),
                strip.text.x = element_blank())+
          ylab("")

        
        #p2 <- ggplot(sum_toxi, aes(x = time, y = count)) +
         # geom_blank() +
         # theme(axis.text = element_blank(),
          #      axis.title = element_blank(),
           #     line = element_blank(),
            #    panel.background = element_blank())
      }
    }
    incProgress(1/3)
                 })          
    return(list(pk = pk, p1 = p1, p2 = p2))
  })
  #-----------------output pharmacokinetics---------------
  output$PK <- renderPlot({
    if(input$plot == FALSE){
      pk
    }else{
      r()$pk
      }
  },height = 600)

  #--------------output pharmacodynamics---------------------
  output$PD <- renderPlot({
    if(input$plot == FALSE){
      ggarrange(p1, p2,nrow=2, heights = c(4,1))
    }else{
      ggarrange(r()$p1, r()$p2,nrow=2, heights = c(12,1))
    }
  },height = 600)
  
  #--------------submit-------------
  observeEvent(input$submit, {
    time <- as.character(Sys.time())
    txt <- c(time, input$name, input$aff, input$email)
    
    cat("\n",file = "userinfo.txt",append = TRUE)
    cat(txt, file = "userinfo.txt",sep = ", ",append = TRUE)
    
    output$thanks <- renderText(
      "Thanks for your submission!"
    )
  })
  
}