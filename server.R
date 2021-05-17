library(shiny)

n<-dim(read.csv("risk_groups.csv", stringsAsFactors = F))[1]
T=dim(read.csv("supply.csv", stringsAsFactors = F))[1]
#magnitude of increase or decrease (currently fixed)
x=10

shinyServer(function(input, output) {
    
    rg <- reactiveValues(data = { 
        read.csv("risk_groups.csv", stringsAsFactors = F)
    })
    
    output$rg_datatable <- renderDT({
        DT::datatable(rg$data, 
                      colnames = c("Risk group", "Population size", "IFR", "RR of infection", "Pre-existing immunity (%)", "Coverage target (%)"), 
                      caption = "Priority groups for vaccination (editable)", 
                      editable = TRUE)
    })
    
    observeEvent(input$rg_datatable_cell_edit, {
        #get values
        info = input$rg_datatable_cell_edit
        i = as.numeric(info$row)
        j = as.numeric(info$col)
        k = as.numeric(info$value)
        #write values to reactive
        rg$data[i,j] <- k
    })
    
    
    supply <- reactiveValues(data = { 
        read.csv("supply.csv", stringsAsFactors = F)
    })
    output$supply_datatable <- renderDT({
        DT::datatable(supply$data, 
                      colnames = c("Week", "Doses", "Max delivery/wk"), 
                      caption = "Vaccine supply and delivery constraints (editable)", 
                      editable = TRUE)
    })
    
    observeEvent(input$supply_datatable_cell_edit, {
        #get values
        info = input$supply_datatable_cell_edit
        i = as.numeric(info$row)
        j = as.numeric(info$col)
        k = as.numeric(info$value)
        #write values to reactive
        supply$data[i,j] <- k
    })
    
    rv<-reactive({
        #calculate vaccine efficacy as a function of time since vaccination
        es1<-c(rep(input$ve1_p[1],12),rep(input$ve1_p[2],T-12))/100
        es2<-c(rep(input$ve2_p[1],12),rep(input$ve2_p[2],T-12))/100
        er1<-es2
        er2<-es2
        
        #calculate force of infection over time
        if(input$foi_trend=="constant"){
            ll<-rep(input$foi,T)/100
        }else if(input$foi_trend == "increasing"){
            ll<-(((2*input$foi)/(1+x))+(2*input$foi*(x-1)/((1+x)*T))*c(1:T))/100
        }else if(input$foi_trend == "decreasing"){
            ll<-(((2*input$foi)/(1+x))+(2*input$foi*(x-1)/((1+x)*T))*c(T:1))/100
        }
       
        m_1st<-run_model(rg, supply, list(es1 = es1, es2 = es2, er1 = er2, er2 = er2, ll = ll), "1st doses", input$mixed_2nd_dose)
        m_2nd<-run_model(rg, supply, list(es1 = es1, es2 = es2, er1 = er2, er2 = er2, ll = ll), "2nd doses", input$mixed_2nd_dose)
        m_mixed<-run_model(rg, supply, list(es1 = es1, es2 = es2, er1 = er2, er2 = er2, ll = ll), "Mixed strategy", input$mixed_2nd_dose)
        
        return(list(m_1st, m_2nd, m_mixed))
    })
    
    
    
    output$plot_impact <- renderPlot({
          
            res<-rv()
            m1<-res[[1]]
            m2<-res[[2]]
            mm<-res[[3]]
        
            d<-cbind(apply(m1$DA,2,sum),
                     apply(m2$DA,2,sum),
                     apply(mm$DA,2,sum))
            d<-data.frame(d)
            names(d)<-c("first","second","mixed")
            d$risk_group<-rg$data$Risk.group
            d<-rbind(d,c(apply(d[,1:3],2,sum),"Total"))
            d$risk_group<-factor(d$risk_group,levels=unique(d$risk_group))
            d<- d %>% gather(strategy, d_averted, 1:3)
            d$d_averted<-as.numeric(d$d_averted)
            p3<-ggplot(data=d, aes(x=risk_group, y=d_averted, fill=strategy)) +
                geom_bar(stat="identity", position=position_dodge()) + ylab("Deaths averted") + xlab("Risk Group") +
                #  geom_text(aes(label=d_averted), vjust=1.6, color="white",
                #           position = position_dodge(0.9), size=3.5)+
                scale_fill_brewer(palette ="Dark2" ) + theme_minimal() 
            p3
        })
 
    output$tab_impact <- renderTable({
        
        res<-rv()
        m1<-res[[1]]
        m2<-res[[2]]
        mm<-res[[3]]
        
        d<-cbind(apply(m1$DA,2,sum),
                 apply(m2$DA,2,sum),
                 apply(mm$DA,2,sum))

        d<-data.frame(round(d,0))
        d$risk_group<-rg$data$Risk.group
        d<-rbind(d,c(apply(d[,1:3],2,sum),"Total"))
        d$risk_group<-factor(d$risk_group,levels=unique(d$risk_group))
        d<-cbind(d[,4],d[,-4])
        names(d)<-c("group","first","second","mixed")
        d

    })
    
        output$plot_cov_final <- renderPlot({
            res<-rv()
            m1<-res[[1]]
            m2<-res[[2]]
            mm<-res[[3]]
            d<-data.frame(matrix(data = NA, nrow = 6*n, ncol = 3))
            names(d)<-c("group", "strategy", "coverage")
            d$coverage<-c(m1$d1_cov[T,],m2$d1_cov[T,],mm$d1_cov[T,],m1$d2_cov[T,],m2$d2_cov[T,],mm$d2_cov[T,])
            d$coverage<-100*d$coverage/rep(rg$data$Size)
          
            d$group<-rep(factor(rg$data$Risk.group, levels=unique(rg$data$Risk.group)),6)
            d$strategy<-rep(c(rep("1st dose", n),rep("2nd dose", n),rep("Mixed strategy", n)),2)
            d$dose<-c(rep("Dose 1",3*n),rep("Dose 2",3*n))
            
            ggplot(data=d, aes(x=group, y=coverage, fill=strategy)) +
                geom_bar(stat="identity", position=position_dodge()) + ylab("Coverage (%)") + xlab("Risk Group") +
                facet_wrap( ~ dose) + scale_fill_brewer(palette ="Dark2" ) + theme_minimal() 

        })
        
        output$plot_cov1 <- renderPlot({
            res<-rv()
            m1<-res[[1]]
            d1<-data.frame(m1$d1_cov)
            names(d1)<-rg$data$Risk.group
            d1$week<-c(1:T)
            d1 <- d1 %>% gather(risk_group, coverage, rg$data$Risk.group[1]:rg$data$Risk.group[n])
            ggplot(data = d1, aes(x=week, y=coverage, group=risk_group, color=risk_group)) +
                geom_line() + geom_point() + ylab("Number vaccinated") + theme_minimal() + ggtitle("One dose strategy: 1 dose coverage")
        })
 
        output$plot_cov2 <- renderPlot({
            res<-rv()
            m1<-res[[1]]
            d1<-data.frame(m1$d2_cov)
            names(d1)<-rg$data$Risk.group
            d1$week<-c(1:T)
            d1 <- d1 %>% gather(risk_group, coverage, rg$data$Risk.group[1]:rg$data$Risk.group[n])
            ggplot(data = d1, aes(x=week, y=coverage, group=risk_group, color=risk_group)) +
                geom_line() + geom_point() + ylab("Number vaccinated") + theme_minimal() + ggtitle("One dose strategy: 2 dose coverage")
        })
 
        output$plot_cov3 <- renderPlot({
            res<-rv()
            m2<-res[[2]]
            d1<-data.frame(m2$d1_cov)
            names(d1)<-rg$data$Risk.group
            d1$week<-c(1:T)
            d1 <- d1 %>% gather(risk_group, coverage, rg$data$Risk.group[1]:rg$data$Risk.group[n])
            ggplot(data = d1, aes(x=week, y=coverage, group=risk_group, color=risk_group)) +
                geom_line() + geom_point() + ylab("Number vaccinated") + theme_minimal() + ggtitle("Two dose strategy: 1 dose coverage")
        })
        
        output$plot_cov4 <- renderPlot({
            res<-rv()
            m2<-res[[2]]
            d1<-data.frame(m2$d2_cov)
            names(d1)<-rg$data$Risk.group
            d1$week<-c(1:T)
            d1 <- d1 %>% gather(risk_group, coverage, rg$data$Risk.group[1]:rg$data$Risk.group[n])
            ggplot(data = d1, aes(x=week, y=coverage, group=risk_group, color=risk_group)) +
                geom_line() + geom_point() + ylab("Number vaccinated") + theme_minimal() + ggtitle("Two dose strategy: 2 dose coverage")
        })

        output$plot_cov5 <- renderPlot({
            res<-rv()
            mm<-res[[3]]
            d1<-data.frame(mm$d1_cov)
            names(d1)<-rg$data$Risk.group
            d1$week<-c(1:T)
            d1 <- d1 %>% gather(risk_group, coverage, rg$data$Risk.group[1]:rg$data$Risk.group[n])
            ggplot(data = d1, aes(x=week, y=coverage, group=risk_group, color=risk_group)) +
                geom_line() + geom_point() + ylab("Number vaccinated") + theme_minimal() + ggtitle("Mixed strategy: 1 dose coverage")
        })
        
        output$plot_cov6 <- renderPlot({
            res<-rv()
            mm<-res[[3]]
            d1<-data.frame(mm$d2_cov)
            names(d1)<-rg$data$Risk.group
            d1$week<-c(1:T)
            d1 <- d1 %>% gather(risk_group, coverage, rg$data$Risk.group[1]:rg$data$Risk.group[n])
            ggplot(data = d1, aes(x=week, y=coverage, group=risk_group, color=risk_group)) +
                geom_line() + geom_point() + ylab("Number vaccinated") + theme_minimal() + ggtitle("Mixed strategy: 2 dose coverage")
        })

})

#model code that does vaccination and calculates outputs
run_model <-function(rgs, s, v_params, strat, mixed_cat){
    #initiate variables
    
    rg<-rgs$data
    ss<-s$data
    S<-R<-matrix(data = NA, nrow = T, ncol = n)
    S[1,]<-(1-rg$Natural.immune/100)*rg$Size*rg$Coverage.target/100
    R[1,]<-(rg$Natural.immune/100)*rg$Size*rg$Coverage.target/100
    vs1<-vs2<-vr1<-vr2<-d1_cov<-d2_cov<-matrix(data = 0, nrow = T, ncol = n)
    DA<-matrix(data = 0, nrow = T, ncol = n)
    #vaccinate
    V<-rep(NA,T) 
    V[1]<-ss$Supply[1] #V = vaccine availability
    A<-min(V[1],ss$Max.delivery.per.week[1]) #available doses within capacity to administer
    V[1]<-V[1]-A
    #deliver first doses according to priority
    for(i in 1:n){
        v1<-S[1,i]+R[1,i]
        if(v1>0){
            del<-min(A,v1)
            vs1[1,i]<-del*S[1,i]/v1
            vr1[1,i]<-del*R[1,i]/v1
            A<-A-del
        }
    }
    d1_cov[1,]<-apply(vs1,2,sum)+apply(vr1,2,sum)
    d2_cov[1,]<-apply(vs2,2,sum)+apply(vr2,2,sum)
    #loop over time
    for(i in 2:T){
        #update state variables
        V[i]<-V[i-1]+A+ss$Supply[i]
        S[i,]<-S[i-1,]-vs1[i-1,]
        R[i,]<-R[i-1,]-vr1[i-1,]
        A<-min(V[i],ss$Max.delivery.per.week[1])
        V[i]<-V[i]-A
        if(strat=="2nd doses"){
            #prioritise those who received 1st dose earliest and >12 weeks previously
            for(j in 1:(i-1)){
                if((i-j)>11 && A>0){ 
                    for(k in 1:n){
                        v1<-vs1[j,k]+vr1[j,k]
                        if(v1>0){
                            del<-min(A,v1)
                            vs2[j,k]<-vs2[j,k]+del*vs1[j,k]/v1
                            vr2[j,k]<-vr2[j,k]+del*vr1[j,k]/v1
                            vs1[j,k]<-vs1[j,k]-del*vs1[j,k]/v1
                            vr1[j,k]<-vr1[j,k]-del*vr1[j,k]/v1
                            A<-A-del
                        }
                    }
                }
            }
            #then continue distributing first doses if vaccine supply and capacity available
            if(A>0){
                for(k in 1:n){
                    v1<-S[i,k]+R[i,k]
                    if(v1>0){
                        del<-min(A,v1)
                        vs1[i,k]<-del*S[i,k]/v1
                        vr1[i,k]<-del*R[i,k]/v1
                        A<-A-del
                    }
                }
            }
        }else if(strat=="1st doses"){
            #keep distributing first doses if vaccine supply and capacity available
            if(A>0){
                for(k in 1:n){
                    v1<-S[i,k]+R[i,k]
                    if(v1>0){
                        del<-min(A,v1)
                        vs1[i,k]<-del*S[i,k]/v1
                        vr1[i,k]<-del*R[i,k]/v1
                        A<-A-del
                    }
                }
            }  
            #then prioritise those who received 1st dose earliest and >12 weeks previously
            for(j in 1:(i-1)){
                if((i-j)>11 && A>0){ 
                    for(k in 1:n){
                        v1<-vs1[j,k]+vr1[j,k]
                        if(v1>0){
                            del<-min(A,v1)
                            vs2[j,k]<-vs2[j,k]+del*vs1[j,k]/v1
                            vr2[j,k]<-vr2[j,k]+del*vr1[j,k]/v1
                            vs1[j,k]<-vs1[j,k]-del*vs1[j,k]/v1
                            vr1[j,k]<-vr1[j,k]-del*vr1[j,k]/v1
                            A<-A-del
                        }
                    }
                }
            }
        }else if(strat=="Mixed strategy"){
            max_group<-match(mixed_cat,rg$Risk.group)
            #prioritise those who received 1st dose earliest and >12 weeks previously for risk groups up to mixed_2nd_dose
            for(j in 1:(i-1)){
                if((i-j)>11 && A>0){ 
                    for(k in 1:max_group){
                        v1<-vs1[j,k]+vr1[j,k]
                        if(v1>0){
                            del<-min(A,v1)
                            vs2[j,k]<-vs2[j,k]+del*vs1[j,k]/v1
                            vr2[j,k]<-vr2[j,k]+del*vr1[j,k]/v1
                            vs1[j,k]<-vs1[j,k]-del*vs1[j,k]/v1
                            vr1[j,k]<-vr1[j,k]-del*vr1[j,k]/v1
                            A<-A-del
                        }
                    }
                }
            }
            #then continue distributing first doses if vaccine supply and capacity available
            if(A>0){
                for(k in 1:n){
                    v1<-S[i,k]+R[i,k]
                    if(v1>0){
                        del<-min(A,v1)
                        vs1[i,k]<-del*S[i,k]/v1
                        vr1[i,k]<-del*R[i,k]/v1
                        A<-A-del
                    }
                }
            }     
        }else {
            print("error: invalid vaccination strategy")
        }
        d1_cov[i,]<-apply(vs1,2,sum)+apply(vr1,2,sum)
        d2_cov[i,]<-apply(vs2,2,sum)+apply(vr2,2,sum)
        
        #calculate deaths averted by first dose vaccines administered up to time i at time i+1 (since it takes 1 week for second dose to take effect)
        if(i>2 && i<T){
            for(tau in 3:i){ #sum from 3 to account for 2 week delay until first dose has effect
                DA[i+1,]<-DA[i+1,]+vs1[i+1-tau,]*v_params$es1[tau]*v_params$ll[i+1]*rg$RR.Infection*rg$IFR
            }
            for(tau in 2:i){ #sum from 2 to account for 1 week delay until second dose has effect
                DA[i+1,]<-DA[i+1,]+(vs2[i+1-tau,]*v_params$es2[tau]+vr1[i+1-tau,]*v_params$er1[tau]+vr2[i+1-tau,]*v_params$er2[tau])*v_params$ll[i+1]*rg$RR.Infection*rg$IFR
            }
        }
        
    }
    #return model outputs
    return(list("d1_cov" = d1_cov, "d2_cov" =d2_cov, "DA" = DA))
}

