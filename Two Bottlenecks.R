library(tidyr)
library(dplyr)
library("grDevices", lib.loc="C:/Program Files/R/R-3.5.1/library")

#set up day-to-day bottleneck travel choice 
seed=123
#scale the experiment
agg=100
#Ser metering rate for the bottlenecks
metering.rate1=700/agg
metering.rate2=800/agg
iterations=200

#Try for one seed and one demand level
for (u in 1:length(seed)){
  demand=seq(from=80000/agg, to=80000/agg, by=10000/agg)
  #demand=500
  for (d in 1:length(demand)){
    num_ind=demand[d]
    
    
    #Keep Track acress days
    cost.iter=c()
    cost.el.iter=c()
    cost.bn1.iter=c()
    cost.bn2.iter=c()
    
    ind.index=seq(1:num_ind)
    
    #Initialize arrival and exit time ta and te
    
    #Random trip start times in highway
    set.seed(seed[u])
    ta.bn1=round(runif(num_ind,min=-130,max=130))
    te.bn1=ta.bn1*Inf
    ta.bn2=te.bn1
    te.bn2=te.bn1
    cost=ta.bn1*0
    #Tracks if the car has crossed the bottleneck threathshold
    crossed1=ta.bn1*0
    crossed2=ta.bn1*0
    
    ta.new=ta.bn1
    
    cost.bn1=cost
    cost.bn2=cost
    
    
    data=data.frame(ind.index,ta.bn1,te.bn1,ta.bn2,te.bn2,
                    ta.new,cost,cost.bn1,cost.bn2,crossed1,crossed2)
    
    
    
    #Earliness and Lateness Parameters
    b=0.5
    g=2
    
    
    
    
    
    #Simulation period
    t.start=-300
    t.end=300
    #Start of Simulation loop
    
    for (n in 1:iterations) {
      bounded=0
      print(n)
      
      
      
      #Go through every minute in the simulation time-period to determine the accum speed 
      #and when each person exits the system
      
      for (i in t.start:t.end) {
        #first minute initialize minute, accumilation and speed
        if (i==t.start) {
          minute=t.start
          
          accum=0
          
          data$ta.bn1=data$ta.new
          
          
          
          data$te.bn1=Inf
          data$ta.bn2=Inf
          data$te.bn2=Inf
          
          
          data$crossed1=0
          data$crossed2=0
          
          Arrivals.bn1=c()
          Departures.bn1=c()
          processed.bn1=c()
          
          Arrivals.bn2=c()
          Departures.bn2=c()
          processed.bn2=c()
          
        } else {
          #identify minute, find the people that are traveling in that minute (travel set) by determining
          #if ta has passed or if they started their trip but haven't finished it yet
          #determine accumilation and consequently the speed in every minute
          #the speed is used to detemine the distance covered by the car in every minute
          #Once they cover their trip distance they exit and te is recorded
          minute=c(minute,i)
          
          Arrivals.bn1=c(Arrivals.bn1,sum(data$ta.bn1==i))
          e1=which(data$ta.bn1<=i & data$crossed1==0) # index being processed
          
          ta.bn1.queue.processed=data$ta.bn1[e1]                
          if (is.null(ta.bn1.queue.processed)==FALSE){
            queue.processed.bn1=data$ind.index[e1][order(ta.bn1.queue.processed)]
            
            
            if(length(queue.processed.bn1)>=metering.rate1){
              queue.bn1=queue.processed.bn1[metering.rate1:length(queue.processed.bn1)]
              processed.bn1=queue.processed.bn1[1:metering.rate1]
              data$crossed1[processed.bn1]=1
              Departures.bn1=c(Departures.bn1,metering.rate1)
              data$te.bn1[processed.bn1]=i
              data$ta.bn2[processed.bn1]=i+1#enter city
              data$cost.bn1[processed.bn1]=data$te.bn1[processed.bn1]-data$ta.bn1[processed.bn1]
            }else{
              queue.bn1=c()
              processed.bn1=queue.processed.bn1
              len.processed=length(processed.bn1)
              Departures.bn1=c(Departures.bn1,len.processed)
              data$crossed1[processed.bn1]=1
              data$te.bn1[processed.bn1]=i
              data$ta.bn2[processed.bn1]=i+1#enter city
              data$cost.bn1[processed.bn1]=data$te.bn1[processed.bn1]-data$ta.bn1[processed.bn1]
            }
          }
          
          ###bN2 Bottleneck 2
          Arrivals.bn2=c(Arrivals.bn2,sum(data$ta.bn2==i))
          e2=which(data$ta.bn2<=i & data$crossed1==1& data$crossed2==0) # index being processed
          
          ta.bn2.queue.processed=data$ta.bn2[e2]                
          #if (is.null(ta.bn2.queue.processed)==FALSE){
          if (length(ta.bn2.queue.processed)==0){
            Departures.bn2=c(Departures.bn2,0)
          }else{
            queue.processed.bn2=data$ind.index[e2][order(ta.bn2.queue.processed)]
            
            #print(e2)
            if(length(queue.processed.bn2)>=metering.rate2){
              queue.bn2=queue.processed.bn2[metering.rate2:length(queue.processed.bn2)]
              processed.bn2=queue.processed.bn2[1:metering.rate2]
              data$crossed2[processed.bn2]=1
              Departures.bn2=c(Departures.bn2,metering.rate2)
              data$te.bn2[processed.bn2]=i
              for (s in 1:length(processed.bn2)){
                data$cost.bn2[processed.bn2[s]]=data$te.bn2[processed.bn2[s]]-data$ta.bn2[processed.bn2[s]]+max(-b*data$te.bn2[processed.bn2[s]],g*data$te.bn2[processed.bn2[s]])
              }
            }else{
              queue.bn2=c()
              processed.bn2=queue.processed.bn2
              len.processed=length(processed.bn2)
              #print(len.processed)
              Departures.bn2=c(Departures.bn2,len.processed)
              data$crossed2[processed.bn2]=1
              data$te.bn2[processed.bn2]=i
              for (s in 1:length(processed.bn2)){ 
                data$cost.bn2[processed.bn2[s]]=data$te.bn2[processed.bn2[s]]-data$ta.bn2[processed.bn2[s]]+max(-b*data$te.bn2[processed.bn2[s]],g*data$te.bn2[processed.bn2[s]])
                #print(max(-b*data$te.bn2[processed.bn2[s]],g*data$te.bn2[processed.bn2[s]]))
              }
            }
          }
          
          
          
          
          
        }
      }
      
      data$cost=data$cost.bn1+data$cost.bn2
      
      for (o in 1:num_ind){if (data$crossed2[o]==0) {data$cost[o]=Inf}}
      
      
      
      if(n<iterations){
        #Now determine the cost at every minute for every driver to see if they would switch
        #initiate
        time=seq(t.start,t.end-1)
        
        te.bn1=0*time
        te.bn2=0*time
        A.bn1=cumsum(Arrivals.bn1)
        D.bn1=cumsum(Departures.bn1)
        A.bn2=cumsum(Arrivals.bn2)
        D.bn2=cumsum(Departures.bn2)
        cost.bn1=0
        cost.bn2=0
        
        x.t=0*time
        x.p.t=x.t*0
        te=time*0
        cost.bn2=Inf
        
        cost=Inf
        
        
        #calculate costs associated with each possible arrival time 
        
        ta.te.comb=data.frame(time,A.bn1,D.bn1,A.bn2,D.bn2,te.bn1,cost.bn1,te.bn2,cost.bn2,cost)
        
        
        for (k in 1:length(ta.te.comb$time)){
          ta.te.comb$te.bn1[k]=max(ta.te.comb$time[which(((ta.te.comb$D.bn1<ta.te.comb$A.bn1[k]+1)&(ta.te.comb$D.bn1>ta.te.comb$A.bn1[k]-1))|(ta.te.comb$D.bn1==ta.te.comb$A.bn1[k]))[1]],ta.te.comb$time[k])
          ta.te.comb$cost.bn1[k]=ta.te.comb$te.bn1[k]-ta.te.comb$time[k]
        }
        
        for (k in 1:length(ta.te.comb$time)){
          ta.te.comb$te.bn2[k]=max(ta.te.comb$time[which(((ta.te.comb$D.bn2<ta.te.comb$A.bn2[k]+1)&(ta.te.comb$D.bn2>ta.te.comb$A.bn2[k]-1))|(ta.te.comb$D.bn2==ta.te.comb$A.bn2[k]))[1]],ta.te.comb$time[k])
          ta.te.comb$cost.bn2[k]=ta.te.comb$te.bn2[k]-ta.te.comb$time[k]+max(-b*ta.te.comb$te.bn2[k],g*ta.te.comb$te.bn2[k])
        }
        
        ta.te.comb$cost=Inf
        #a=ta.te.comb$time[which(ta.te.comb$A.bn2==num_ind)[1]]
        #max.ta=ta.te.comb$time[which(ta.te.comb$te.bn1==a)]
        #find costs and te for every possible ta(time)
        for (m in 1:length(which(ta.te.comb$time<=(length(ta.te.comb$time)-1)))){
          ta.te.comb$cost[m]=ta.te.comb$cost.bn1[m]+ta.te.comb$cost.bn2[which(ta.te.comb$time==(ta.te.comb$te.bn1[m]+1))[1]]
        }
        
        #determine time that each shifting indiv wants to shift to
        perc_shift=0.05
        shifters=sample(1:num_ind,size=round(perc_shift*num_ind),replace=FALSE)
        
        #Shifters 
        for (p in 1:length(shifters)) {
          l=shifters[p]
          
          ta.new=which(ta.te.comb$cost==min(ta.te.comb$cost))
          pick1=which(ta.te.comb$cost==min(ta.te.comb$cost,na.rm=TRUE))
          if(length(pick1)>1){ pick=sample(pick1,1)} else{pick=pick1}
          ta.new=ta.te.comb$time[pick]
          ta.new.pick=ta.new
          #bounded rationality
          if (ta.te.comb$cost[pick]<(data$cost[l]-0)){
            #print(paste0('Shift from ', data$ta.bn[l], " to ",ta.new.pick))
            data$ta.new[l]=ta.new.pick
          } else{
            bounded=bounded+1
            #print(paste0('Shift from ', data$ta.bn[l], " to ",ta.new.pick))
            data$ta.new[l]=data$ta.bn1[l]
          }
          
          
        }
        
        
        
        #update ta, and cost based on shifting done at minute 1
      }
      if (n==iterations){
        A.bn1=cumsum(Arrivals.bn1)
        D.bn1=cumsum(Departures.bn1)
        A.bn2=cumsum(Arrivals.bn2)
        D.bn2=cumsum(Departures.bn2)
      }
      #print(Departures.bn)
      total.cost=sum(data$cost)
      print(total.cost)  
      cost.iter=c(cost.iter,total.cost)
      cost1=sum(data$cost.bn1)
      cost2=sum(data$cost.bn2)
      cost.el.iter=c(cost.el.iter,cost2-sum(data$te.bn2-data$ta.bn2))
      cost.bn1.iter=c(cost.bn1.iter,cost1)
      cost.bn2.iter=c(cost.bn2.iter,cost2)
      print(bounded)
      #if(y==1){break}
      
      # #Saving images, possible visualiztion when needed
      # if(n%%5==0){
      # # 1. Open jpeg file
      # filename=paste0("C:/Users/barho/Desktop/Research/November 2020/bn700br/Cumulative Iter",n,".jpeg")
      # jpeg(file=filename, width =581, height =367)
      # # 2. Create the plot
      # plot(A.bn2~seq(from=-300, to=299,by=1),xlim=c(-125,100),xlab="Time (min)",ylab="N",col="red")
      # #points(A.bn2~seq(from=-300, to=299,by=1),col="red")
      # points(D.bn2~seq(from=-300, to=299,by=1),col="blue")
      # # 3. Close the file
      # dev.off()
      # }
    }
    #direct=paste0("C:/Users/barho/Desktop/Research/July 2020/Bridge City/",pricing,banstatus,"/",num_ind,"seed",seed[u],".RData")
    #save.image(direct)
  }
}

