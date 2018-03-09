library(shiny)
library(shinyIncubator)
library(ggplot2)
library(DT)
library(splines)
library(mvtnorm)

wd<-getwd()
Shiny.data <- read.csv(paste0(wd,"/outputData.csv"))
Shiny.data<-Shiny.data[-c(1)]
#Shiny.data<-read.csv("/Users/haosicheng/Desktop/BIDMC/Shiny-EBART/Shiny_App/outputData.csv")




# Define UI for application that draws a histogram
ui <- fluidPage(
            headerPanel("Empirical Bayes Analysis for Randomized Trials "),
            sidebarLayout(
              sidebarPanel(
                titlePanel("Section 1: Specify the trials used to estimate the prior distribution of the effect size"),
                helpText("(Default value includes every trial)"),
                selectInput('e0', 'Conditions', choices = Shiny.data$condition,multiple = TRUE),
                uiOutput("ui_1"),
                selectInput('e2', 'Intervention', choices = Shiny.data$intervention_type,multiple = TRUE),
                uiOutput("ui_2"),
                selectInput('e4', 'Age groups', choices = Shiny.data$Age_Groups,multiple = TRUE),
                selectInput('e5', 'Gender', choices = Shiny.data$Gender,multiple = TRUE),
                sliderInput("range", "Enrollment Range:", min = 6, max = 26449, value = c(6,26449)),#update min and max when new data added
                dateRangeInput("date","Primary Completion Date Range:",start = "2008-01-01",end = "2013-12-01")
                #checkboxInput("n.date", "Include trials with no completion date", value = TRUE)
                #submitButton("Subset")
            ),
            sidebarPanel(
                titlePanel("Section 2: Specify the point estimate and the standard error of the efficacy measure of the trial of interest"),
                numericInput("e6", "Point Estimate (e.g. difference in means, difference in proportions, logarithm of odds ratio, logarithm of hazard ratio)",value = NA),
                numericInput("e7", "Standard Error:",value = NA),
                sliderInput("e8", "Level of confidence:",min = 0.90,max = 1,value = 0.95),
                actionButton(inputId = 'go',label = 'Calculate'),
                span(textOutput("warning"),style = "color:red")
                
            )),
  
            fluidRow(
                titlePanel("Section 3: Result"),
                span(textOutput('out_1'),style = "color:red"),
                span(textOutput('out_2'),style = "color:red"),
                span(textOutput('out_3'),style = "color:red")
    
            ),
            fluidRow(
                helpText('Density curves:'),
                plotOutput('out_4')
             ),
            fluidRow(
              titlePanel("Section 4:  Details of trials used to estimate the prior distribution of the effect size"),
              DT::dataTableOutput("table")
            ),
            fluidRow(
              helpText("Maintainer: Sicheng Hao, E-mail: shao@bidmc.harvard.edu"),
              helpText("Principal Investigator: Changyu Shen")
            )
        )
      

# Define server logic required to draw a histogram
server <- function(input, output) {
  dat <- Shiny.data
  data0<-dat
  data1<-dat
  data2<-dat
  data3<-dat
  #functions!!!!!!!!!!
  qmle <-function(a,y,Q,P,c0=1){
      # negative likelihood function, for use in nlm to find MLE alphahat:
      #    eg  nlm(qmle,startvalue,y=y,Q=Q,P=P,c0=c0)
      # a is any trial value of alpha
      #Q is structure matrix in (2.2) (do not include column of 1's)
      #P is probability matrix p[i,j], (2.5), or (2.20) for iid case.
      # y is vector of counts (2.22). For non-iid case set y=1
      # c0 is penalty multiplier (3.2)
      eqa=exp(Q%*%a)
      ga=as.vector(eqa/sum(eqa))
      fa=as.vector(P%*%ga)
      -sum(y*log(fa))+c0*sum(a^2)^.5
      
    }
  
  qformula <-function(ahat,N,Q,P,c0=1){
      # Standard deviation and bias estimates from formula (3.6)
      # ahat is MLE of alpha
      #     Set N=1 for non-iid case
      #Q is structure matrix in (2.2) (do not include column of 1's)
      #P is probability matrix p[i,j], (2.5), or (2.20) for iid case.
      # c0 is penalty multiplier (3.2)
      
      g=as.vector(exp(Q%*%ahat)); g=g/sum(g)
      f=as.vector(P%*%g)
      y=N*f
      if(N==1)y=1
      Pt=P/f
      W=g*(t(Pt)-1)
      qw=t(Q)%*%W
      ywq=(y*t(W))%*%Q
      I1=qw%*%ywq
      
      aa=sqrt(sum(ahat^2))
      s.=c0*ahat/aa; s..=(c0/aa)*(diag(length(ahat))-outer(ahat,ahat)/aa^2)
      I2=solve(I1+s..)
      Bias=as.vector(-I2%*%s.); Cov=I2%*%(I1%*%t(I2));Se=diag(Cov)^.5
      Dq=(diag(g)-outer(g,g))%*%Q
      Biasg=Dq%*%Bias; Covg=Dq%*%Cov%*%t(Dq); Seg=diag(Covg)^.5
      Mat=cbind(g,Seg,Biasg)
      dimnames(Mat)[[2]]=c("g","Stdev","Bias")
      
      # return(list(Mat,Covg))  
      return(Mat)
    }
  
  
  decon<-function(z,theta.range,z.thre=c(-Inf,Inf),wt=1,grid=0.01,degree=2,penalty=0)
  {
    # Create y, Q, P
    
    z.grid<-seq(min(z)-grid,max(z)+grid,grid)
    theta.grid<-seq(theta.range[1],theta.range[2],grid)
    if (wt==1) wt<-rep(1,length(z))
    temp<-sapply(z,function(x) which.min(abs(x-z.grid)))
    temp<-aggregate(wt,list(temp),sum)
    y<-rep(0,length(z.grid))
    y[temp[,1]]<-temp[,2]
    
    Q<-ns(theta.grid,df=degree)
    Q<-matrix(Q,nrow(Q),ncol(Q))
    Q<-t((t(Q)-apply(Q,2,mean))/apply(Q,2,sd))
    P<-t(sapply(z.grid,function(x) dnorm(x,mean=theta.grid)/(pnorm(z.thre[2]-theta.grid)-pnorm(z.thre[1]-theta.grid))))
    
    alpha.hat<-nlm(qmle,rep(0,degree),c0=penalty,y=y,Q=Q,P=P,iterlim=200)
    
    out<-qformula(alpha.hat$estimate, N=length(z),c0=penalty, Q=Q, P=P)
    
    neg.ll<-qmle(alpha.hat$estimate,c0=penalty,y=y,Q=Q,P=P)
    
    return(list(theta.grid=theta.grid,out=out,neg.ll=neg.ll))
  }
  
  post.cal<-function(z,g.hat,theta.grid,g.thre=c(-Inf,Inf),cover = 0.95)
  {
    temp<-sapply(theta.grid,function(x) dnorm(z,mean=x,log=T)-log(pnorm(g.thre[2],mean=x)-pnorm(g.thre[1],mean=x)))
    temp<-log(g.hat)+temp
    temp<-exp(temp-max(temp))
    post.dis<-temp/sum(temp)
    post.cdf<-cumsum(post.dis)
    l<-theta.grid[which.min(abs(post.cdf-(1-cover)/2))]
    u<-theta.grid[which.min(abs(post.cdf-(1+cover)/2))]
    out<-list(c(point=sum(theta.grid*post.dis),ci=c(l,u)),post.dis=post.dis)
    return(out)
  }  
  
  #end of functions!!!!!!!!!!!!!!!!!!
  
  #condition_detail
  output$ui_1 <- renderUI({
    if (is.null(input$e0))
      return()
    sub<-NULL
    l<-length(input$e0)
    for(i in 0:l){
      new<-subset(dat,condition==input$e0[i])
      sub<-rbind(sub,new)
    }
    selectInput('e1', 'Conditions_detail', choices = sub$Conditions,multiple = TRUE)
  })
  #intervention_detail
  output$ui_2 <- renderUI({
    if (is.null(input$e2))
      return()
    sub<-NULL
    l<-length(input$e2)
    for(i in 0:l){
      new<-subset(dat,intervention_type==input$e2[i])
      sub<-rbind(sub,new)
    }
    selectInput('e3', 'Intervention_detail', choices = sub$Interventions,multiple = TRUE)
  })
  output$warning<-renderText({
    d<-data()
    t<-NULL
    if(nrow(d)<20){
      t<-("Warning: number of trials less than 20")
    }
    paste0("",t)
  })
  data<-reactive({
    #select datatable by user input
    #subset condition
    if(!is.null(input$e0)){
      sub<-NULL
      #print(input$e0)
      l0<-length(input$e0)
      for(i in 0:l0){
        new<-subset(dat,condition==input$e0[i])
        sub<-rbind(sub,new)
      }
      data0<-sub
    }
    if(!is.null(input$e1)){
      sub<-NULL
      #print(input$e1)
      l1<-length(input$e1)
      for(i in 0:l1){
        new<-subset(data0,Conditions==input$e1[i])
        sub<-rbind(sub,new)
      }
      data0<-sub
    }
    #subset intervention
    if(!is.null(input$e2)){
      sub<-NULL
      #print(input$e2)
      l2<-length(input$e2)
      for(i in 0:l2){
        new<-subset(dat,intervention_type==input$e2[i])
        sub<-rbind(sub,new)
      }
      data1<-sub
    }
    if(!is.null(input$e3)){
      sub<-NULL
      l3<-length(input$e3)
      for(i in 0:l3){
        new<-subset(data1,Interventions==input$e3[i])
        sub<-rbind(sub,new)
      }
      data1<-sub
    }
    #subset age_groups
    if(!is.null(input$e4)){
      sub<-NULL
      #print(input$e3)
      l4<-length(input$e4)
      for(i in 0:l4){
        new<-subset(dat,Age_Groups==input$e4[i])
        sub<-rbind(sub,new)
      }
      data2<-sub
    }
    
    #subset gender
    if(!is.null(input$e5)){
      sub<-NULL
      #print(input$e5)
      l5<-length(input$e5)
      for(i in 0:l5){
        new<-subset(dat,Gender==input$e5[i])
        sub<-rbind(sub,new)
      }
      data3<-sub
    }
    
    #merge
    d1<-merge(data0,data1)
    d2<-merge(data2,data3)
    d<-merge(d1,d2)
    if(!is.null(input$range)){
      d<-subset(d,d$Enrollment >= input$range[1])
      d<-subset(d,d$Enrollment <= input$range[2])
    } 
    date<-as.character(input$date)
    year<-as.numeric(substr(date,start = 1,stop = 4))
    mon<-as.numeric(substr(date,start = 6,stop = 7))
    lm2<-year[1]*12+mon[1]
    um2<-year[2]*12+mon[2]
    d<-subset(d,m2>=lm2)
    d<-subset(d,m2<=um2)
    d
  })
  output$table <- DT::renderDataTable(DT::datatable({
    #output user selected datatable
    dat<-data()
    dat<-dat[-c(14:16)]
    dat
  }))
  print.result<-reactive({
    #reactive for point estimate, post probability and confidence interval
    z<-z()
    dt<-calculation()
    a<-post.cal(z,g.hat=dt$prior,theta.grid=dt$grid,cover = input$e8)
    a1<-unlist(a)
    a1
  })
  output$out_1 <- renderPrint({
    a1<-print.result()
    point<-round(a1[1],digits = 3)
    paste("Point Estimate:",(point*input$e7))
  })
  output$out_2 <- renderPrint({
    z<-z()
    dt<-calculation()
    #calculate probability
    positive.len<-sum(dt$grid>0)
    len<-length(dt$grid)
    post.p<-round(sum(dt$post[(len-positive.len+1):len]),digits = 3)
    paste("Posterior probability of efficacy:", post.p)
  })
  output$out_3<-renderPrint({
    a1<-print.result()
    ci1<-round(a1[2],digits = 3)
    ci2<-round(a1[3],digits = 3)
    paste0((input$e8)*100,"% Confidence Interval: (",(ci1*input$e7)," , ",(ci2*input$e7),")")
  })
  z<-reactive({
    if((!is.na(input$e6))&&(!is.na(input$e7))){
      z<-input$e6 / input$e7
      z
    }
  })
  calculation<-eventReactive(input$go,
  {
    z<-z()
    if(!is.na(z)){
      dat<-data()
      n.na<-sum(is.na(dat$z))
      n.99<-sum(dat$z>=99&!is.na(dat$z))
      #main function!!!!!!!!!!!!!!!!!!!!!!!!
      zvec<-dat$z[!is.na(dat$z) & dat$z<99]
      n.sample<-length(zvec)
      z.max<-round(max(zvec),digit = 2)
      z.min<-round(min(zvec),digit = 2)
      n1<-sum(zvec<=1.96)
      n2<-sum(zvec>3.29)
      if(n1==0){
        z.wt.1<-rep(0,length(zvec))
        print("1")
      }else{
        z.wt.1<-(zvec<1.96)*(n1+n.na)/n1
      }

      if(n2==0){
        z.wt.2<-rep(0,length(zvec))
        print("2")
      }else{
        z.wt.2<-(zvec>3.29)*(n2+n.99)/n2
      }
      z.wt<-z.wt.1+z.wt.2+(zvec>=1.96 & zvec<=3.29)
      z.range = c(z.min,z.max)
      df<-2
      out<-decon(z=zvec[zvec<=10],theta.range=z.range,wt=z.wt[zvec<=10],degree=df)
      min.bic<-log(n.sample)*df+2*out$neg.ll
      for(df in 3:5){
        out_temp<-decon(z=zvec[zvec<=10],theta.range=z.range,wt=z.wt[zvec<=10],degree=df)
        bic = log(n.sample)*df+2*out_temp$neg.ll
       if(bic < min.bic){
         min.bic<-bic
         out<-out_temp
        }
      }
    }
    a<-post.cal(z,g.hat=out$out[,1],theta.grid=out$theta.grid,cover = input$e8)
    #calculate likelihood
    grid.n = length(out$theta.grid)
    grid.l = out$theta.grid-0.005
    grid.u = out$theta.grid+0.005
    likelihood<-pnorm(grid.u,mean = z)-pnorm(grid.l,mean = z)
    dt<-data.frame(grid = out$theta.grid,post = a$post.dis,prior = out$out[,1],likelihood = likelihood)
    dt
  })
  

  output$out_4<-renderPlot({
    z<-z()
    dt<-calculation()
    qplot(xlab = "Effect Size",ylab = "probability")+
      geom_line(data = dt,aes(x = grid,y = post,colour = "Posterior"))+
      geom_line(data = dt,aes(x = grid,y = prior,colour = "Prior"))+
      geom_line(data = dt,aes(x = grid,y = likelihood,colour = "Likelihood"))+
      scale_colour_manual("",breaks = c("Posterior","Prior","Likelihood"),values = c("red","blue","green"))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)