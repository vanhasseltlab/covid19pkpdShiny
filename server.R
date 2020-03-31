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
        
        amt1_1 <- input$amt1_1_2
        amt1_2 <- input$amt1_2_2
        amt2_1 <- input$amt2_1_2
        amt2_2 <- input$amt2_2_2
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
                         "SARS-cov-2 3.67ug/ml (Jeon et al. bioRxiv(2020))" = 3.67,
                         "SARS-cov-1 2.84ug/ml (Keyaerts E et al. BBRC(2004))" = 2.84,
                         "SARS-cov-1 1.43ug/ml (Vincent MJ et al. Virol J(2005))" = 1.43,
                         "SARS-cov-1 1.33ug/ml (de Wilde AH et al. AAC(2014))" = 1.33,
                         "Mers-cov 0.95ug/ml (de Wilde AH et al. AAC(2014))" = 0.95)

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
                            V2 = TV_V2 * exp(eta_V2) ;
                            Q = TV_Q;
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
                      ka = 1.15, 
                      Tlag = 0.389,
                      WT = WT, 
                      M_WT = 76.2,
                      EC50 = EC50,
                      fu = fu,
                      ke0 = 6.166944,
                      Kp = 171.847380*0.2892984)
          omega <- diag(4);
          diag(omega) <- c(0.161^2, 0.232^2, 0.715^2, 0.0359^2)
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
          
          if(amount1_2 == 0 & amount2_2 == 0 ){
            
            #pk2 <- pl+xlim (0,100) + ylim (0,100) + ylab(paste0('Concentration (',input$concunit,')'))
            AUC_rtv <- 0
            
          }else{
          #--------------RTV PK-----------------
          mod_rtv1 <- RxODE("
                  F = TV_F * exp(eta_F);
                            CL = F * TV_CL * CL_LPV ^ LPV* exp(eta_CL) ;
                            V = F * TV_V * exp(eta_V);
                            ka = TV_ka * exp(eta_ka);
                            C1 = centr/V;
                            Kp = 1;
                            Clung = Kp*C1;
                            d/dt(depot) = - ka*depot;
                            lag(depot) = Tlag;
                            d/dt(centr) = ka*depot - CL*C1;
                            d/dt(AUC) = C1;"
          )
          # parameters
          pars_rtv <- c(TV_F = 1, TV_CL = 10.5, TV_V = 96.6, 
                        TV_ka = 0.871, Tlag = 0.778, 
                        CL_LPV = 2.72, LPV = 1)
          # IIV without correlation
          omega_rtv <- diag(4);
          diag(omega_rtv) <- c(0.349, 0.147, 0.64, 2.856)
          dimnames(omega_rtv) <- list(c("eta_F", "eta_CL", "eta_V", "eta_ka"), 
                                      c("eta_F", "eta_CL", "eta_V", "eta_ka"))
          
          inits_rtv <- c(depot=0, centr=0)

          ev2 <- eventTable()
          if (amount1_2 >0) {
            if(nbr1 > 1){
              ev2$add.dosing(dose = amount1_2, start.time = 0, nbr.doses = nbr1, dosing.interval = ii1, dosing.to = "depot")
            }else{
              ev2$add.dosing(dose = amount1_2, start.time = 0, dosing.to = "depot")  
            }
          }
          if (amount2_2 >0) {
            if (nbr2 > 1){
              ev2$add.dosing(dose = amount2_2, start.time = D1*24,nbr.doses = nbr2,dosing.interval = ii2,dosing.to = "depot")
            }else{
              ev2$add.dosing(dose = amount2_2, start.time = D1*24,dosing.to = "depot")
            }
          }
          ev2$add.sampling(seq(0,(D1+D2)*24,1))

          sim_rtv <- solve(mod_rtv1, pars_rtv, ev2, inits_rtv, omega=omega_rtv, nSub=100,seed = 12)
          
          # calculate AUC0-12 for ritonavir at steady state
          AUC0_12 <- sim_rtv %>%
            group_by(sim.id) %>% 
            filter(time == 12) %>%
            ungroup()
          
          # co-variates 
          AUC_rtv =  median(AUC0_12$AUC)      # median ritonavir AUC0-12
          }
          # 1-cmt model for Lopinavir
          mod_lpv1 <- RxODE("
                            F = TV_F * exp(eta_F);
                            ka = TV_ka * exp(eta_ka);
                            I_rtv = 1 - (Imax * AUC_rtv / (AUC50 + AUC_rtv));
                            CL = F * TV_CL * I_rtv * IND * exp(eta_CL);
                            V = F * TV_V * exp(eta_V);
                            C1 = centr/V;
                            Cf = C1*fu;
                            Kp = 0.51;
                            Clung = Kp*C1;
                            Rl = Clung/EC50;
                            Rp = C1/EC50;
                            Rf = Cf/EC50;
                            d/dt(depot) = - ka*depot;
                            d/dt(centr) = ka*depot - CL*C1;"
          )
          
          # parameters
          EC50 <- switch(input$ec50_3,
                         "SARS-cov-2 5.73ug/ml (Jeon et al. bioRxiv(2020))" = 5.73,
                         "SARS-cov-1 4ug/ml (Chu CM et al. Thorax(2004))" = 4,
                         "SARS-cov-1 2.52ug/ml (de Wilde AH et al. AAC(2014))" = 2.52,
                         "Mers-cov 4.4ug/ml (de Wilde AH et al. AAC(2014))" = 4.4)

          pars_lpv <- c(Imax = 1, AUC50 = 2.26, TV_ka = 0.564, AUC_rtv = AUC_rtv,
                        TV_F = 1, TV_CL = 14.8, TV_V = 61.6, IND = 1, EC50 = EC50, fu = fu)
          
          # IIV without correlation
          omega_lpv <- diag(4);
          diag(omega_lpv) <- c(0.031, 0.030, 0.256, 0.956)
          dimnames(omega_lpv) <- list(c("eta_F", "eta_CL", "eta_V", "eta_ka"), 
                                      c("eta_F", "eta_CL", "eta_V", "eta_ka"))
          
          inits_lpv <- c(depot=0, centr=0)
          
          sim_p <- solve(mod_lpv1, pars_lpv, ev1, inits_lpv, omega=omega_lpv, nSub=100,seed = 12)

        }
        incProgress(1/3)
        
        
        #------------------plotting plasma concentration-------------------
        toxi <- switch(input$drugname,
                       "Chloroquine" = 0.52,
                       "Hydroxychloroquine" = 4.42,
                       "Lopinavir + Ritonavir" = 27.05)
        
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
        sim_hm <- sim_p[0,]
        
        sum_hm_p <- data.frame(time = NULL, bin = NULL, perc = NULL)
        sum_toxi_p <- data.frame(time = NULL, bin = NULL, perc = NULL)
        
        for (i in (1:ceiling(D1+D2))*24){
          if (i <= D1) {
            ii <- ii1
          }else{
            if (amount2 == 0) {
              ii <- 24
            }else{
              ii <- ii2
            } 
          }
          if (ii >24) {
            ii <- 24
          }
          sim_hm <- sim_p[sim_p$time==(i - 24 + ii),]
          
          #perc1 <- sum(sim_hm$Rp<fold1)/100
          perc2 <- sum(sim_hm$Rp>=fold1)/100
          perc3 <- sum(sim_hm$Rp>=fold2)/100
          perc4 <- sum(sim_hm$Rp>=fold3)/100
          perc5 <- sum(sim_hm$Rp>=fold4)/100
          perc6 <- sum(sim_hm$Rp>=fold5)/100
          perc7 <- sum(sim_hm$Rp>=fold6)/100
          
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
        
        sim_hm <- sim_p[0,]
        
        sum_hm_f <- data.frame(time = NULL, bin = NULL,perc = NULL)
        sum_toxi_f <- data.frame(time = NULL, bin =NULL, perc = NULL)
        
        for (i in (1:ceiling(D1+D2))*24){
          if (i <= D1) {
            ii <- ii1
          }else{
            if (amount2 == 0) {
              ii <- 24
            }else{
              ii <- ii2
            } 
          }
          if (ii >24) {
            ii <- 24
          }
          sim_hm <- sim_p[sim_p$time==(i - 24 + ii),]
          
          count <- 0
          for (j in 1:100){
            if (max(sim_p[sim_p$sim.id == j & sim_p$time > (i-24) & sim_p$time <= i,]$Cf) >= fu*toxi){
              count <- count + 1
            }else{
              count <- count
            }
          }
          
          #perc1 <- sum(sim_hm$Rp<fold1)/100
          perc2 <- sum(sim_hm$Rf>=fold1)/100
          perc3 <- sum(sim_hm$Rf>=fold2)/100
          perc4 <- sum(sim_hm$Rf>=fold3)/100
          perc5 <- sum(sim_hm$Rf>=fold4)/100
          perc6 <- sum(sim_hm$Rf>=fold5)/100
          perc7 <- sum(sim_hm$Rf>=fold6)/100
          
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
        
        sim_hm <- sim_p[0,]
        
        sum_hm_l <- data.frame(time = NULL, bin = NULL,perc = NULL)
        sum_toxi_l <- data.frame(time = NULL, bin = NULL, perc = NULL)
        
        for (i in (1:ceiling(D1+D2))*24){
          if (i <= D1) {
            ii <- ii1
          }else{
            if (amount2 == 0) {
              ii <- 24
            }else{
              ii <- ii2
            } 
          }
          if (ii >24) {
            ii <- 24
          }
          sim_hm <- sim_p[sim_p$time==(i - 24 + ii),]
          
          count <- 0
          for (j in 1:100){
            if (max(sim_p[sim_p$sim.id == j & sim_p$time > (i-24) & sim_p$time <= i,]$Clung) >= toxi){
              count <- count + 1
            }else{
              count <- count
            }
          }
          
          #perc1 <- sum(sim_hm$Rl<fold1)/100
          perc2 <- sum(sim_hm$Rl>=fold1)/100
          perc3 <- sum(sim_hm$Rl>=fold2)/100
          perc4 <- sum(sim_hm$Rl>=fold3)/100
          perc5 <- sum(sim_hm$Rl>=fold4)/100
          perc6 <- sum(sim_hm$Rl>=fold5)/100
          perc7 <- sum(sim_hm$Rl>=fold6)/100
          
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
  
}