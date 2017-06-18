library(data.table)
library(bit64)
library(e1071)
library(ggplot2)
library(mclust)
library(datasets)
library(caret)
library(rpart)
library(lattice)

#print multiple plots to pdf by given plots list and number of colums
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols), byrow = T)
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#gets data.table and returns new data.table with facebook related colums - sums all facebook likes for each movie
#and removes unnecessary colums
get.facebook.dt = function(dt){
  facebook.dt = dt[,c("movie_title","cast_total_facebook_likes","director_facebook_likes","movie_facebook_likes","imdb_score","title_year")]
  facebook.dt[,likes_sum := cast_total_facebook_likes+director_facebook_likes+movie_facebook_likes]
  facebook.dt = facebook.dt[cast_total_facebook_likes>0 & director_facebook_likes>0 & movie_facebook_likes>0,]
  
  facebook.dt$cast_total_facebook_likes = facebook.dt$cast_total_facebook_likes/ max(facebook.dt$cast_total_facebook_likes)
  facebook.dt$director_facebook_likes = facebook.dt$director_facebook_likes/max(facebook.dt$director_facebook_likes)
  facebook.dt$movie_facebook_likes = facebook.dt$movie_facebook_likes/max(facebook.dt$movie_facebook_likes)
  
  facebook.dt = facebook.dt[,c("movie_title","title_year","likes_sum","imdb_score")]
  facebook.dt
}

#gets data.table and returns new data.table with imdb reviews related colums - sums user reviews and critics reviews
get.reviews.dt = function(dt){
  reviews.dt = dt[,c("movie_title","num_critic_for_reviews","num_user_for_reviews","imdb_score","title_year")]
  reviews.dt[,reviews.sum:=num_user_for_reviews + num_critic_for_reviews]
  reviews.dt = reviews.dt[,c("movie_title","imdb_score","reviews.sum","title_year")]
  reviews.dt
}

#gets data.table and returns new data.table with budget related colums - removes budgets<0 and return log10 of each budget
get.budget.dt = function(dt){
  budget.dt = dt[,c("movie_title","title_year","imdb_score","budget")]
  budget.dt = budget.dt[budget>0,]
  budget.dt$budget = as.double(log10(budget.dt$budget))
  budget.dt
}

#gets data.table and returns new data.table with gross(total income for a movie) related colums - removes gross<0 and return log10 of each gross
get.gross.dt = function(dt){
  gross.dt = dt[,c("movie_title","title_year","imdb_score","gross")]
  gross.dt = gross.dt[gross>0,]
  gross.dt$gross = as.double(log10(gross.dt$gross))
  gross.dt
}

#gets data.table and returns new data.table with profit(total income of a movie - movie budget) related colums - returns norm of each profit
get.profit.dt = function(dt){
  profit.dt = dt[,c("movie_title","title_year","imdb_score","budget","gross")]
  profit.dt[, profit:= gross-budget ]
  profit.dt$profit = as.double(profit.dt$profit) / (max(profit.dt$profit) - min(profit.dt$profit))
  profit.dt = profit.dt[,c("movie_title","title_year","profit","imdb_score")]
  profit.dt
}

#gets data.table, labels for plots and number of centers wanted.
#preforms kmeans algorithem on dt,
#preform kmeans after pca on dt,
#save both plots and returns them
kmeans.pca = function(dt, centers_num, xlab, ylab, title_a, title_b,genre.title){
  result.plots = c()
  colnames(dt)=c("y","x")
  original = dt
  dt$y = factor(dt$y)
  
  km = kmeans(dt,centers = centers_num)
  corr1 = adjustedRandIndex(dt$y, km$cluster)
  title = paste(genre.title,title_a,"- Correlation", corr1)
  cluster = factor(km$cluster)
  g=ggplot(original, aes(x=x, y=y,color=cluster) )
  g = g + xlab(xlab)
  g = g + ylab(ylab)
  g = g + ggtitle(title)
  g = g + geom_point()
  result.plots[[1]] = g
  #---------- pca -------------------
  dt.2 = dt
  dt.2$y = NULL
  dt.2$x = as.numeric(dt.2$x)
  pca = scale(dt.2,center = T )
  km.pca = kmeans(pca,centers = centers_num)
  cluster.pca = factor(km.pca$cluster)
  corr2 = adjustedRandIndex(dt$y, km.pca$cluster)
  title = paste(genre.title,title_b,"- Correlation", corr2)
  g=ggplot(original, aes(x=x, y=y,color=cluster.pca))
  g = g + xlab(xlab)
  g = g + ylab(ylab)
  g = g + ggtitle(title)
  g = g + geom_point()
  
  result.plots[[2]] = g
  result.plots
}

#gets data.table
#divide 'field' colum to p sized intervals.
#removes intervals with less than 2 values.
#sets new range based on 'field' colum
find.outlires.range = function(dt, field, p, remove=10){
  max.range = max(dt[,get(field)])
  min.range = min(dt[,get(field)])
  range.size = max.range - min.range
  strip.size = p * range.size
  result.range = c(min.range, max.range)
  curr.strip = min.range
  removed.cnt = 0
  for(i in 0:((1/p)-1)){
    n = nrow(dt[curr.strip + i*strip.size <= get(field) & curr.strip + (i+1)*strip.size > get(field),])
    if (n <= 1){
      result.range[1] = curr.strip
      curr.strip = min.range + strip.size*i
      removed.cnt = removed.cnt+n
      
    }
    else
      break
  }
  curr.strip = max.range
  removed.cnt = 0
  for(i in 0:((1/p)-1)){
    n= nrow(dt[curr.strip - i*strip.size >= get(field) & curr.strip - (i+1)*strip.size < get(field),])
    if (n <= 1){
      result.range[2] = curr.strip
      curr.strip = max.range - strip.size*i
      removed.cnt = removed.cnt + n
    }
    else
      break
  }
  dt[result.range[1] <= get(field) & result.range[2] >= get(field),]
}

#aid func to remove outliers
remove.outlires = function(dt, field, p=0.05){
  dt = find.outlires.range(dt, field, p)
  dt
}

#gets data.table
#for each genre in genres - 
#gets facebook data.table from 'get.facebook.dt'
#save the linear regression to plot list
#preform 'kmeans.pca' function
#later print all 3 plots with 'multiplot' function
facebook.likes.corr = function(dt, genres) {
  plots = c()
  cnt = 1
  for(genre in genres$V1){
    temp.dt = get.genre.dt(genre, dt)
    if(nrow(temp.dt) > 100)
    {
      facebook.dt = get.facebook.dt(temp.dt)
      facebook.dt = remove.outlires(facebook.dt, "likes_sum")
      if(nrow(facebook.dt) > 100)
      {
        two.plots = kmeans.pca(facebook.dt[,c("imdb_score","likes_sum")],5, "Total Likes", "IMDB Score", "K-Means", "K-Means after pca",genre)
        plots[[cnt]] = two.plots[[1]]
        plots[[cnt+1]] = two.plots[[2]]
        plots[[cnt+2]] = (ggplot(facebook.dt, aes(x=likes_sum, y=imdb_score))
                        + xlab("Total Likes")
                        + ylab("IMDB Score")
                        + ggtitle(paste("Genre",genre,"Linear regression model"))
                        + geom_point(color='blue')
                        + geom_smooth(method = "lm", colour = "black"))
        cnt = cnt + 3
      }
    }
  }
    pdf("facebook.crr.pdf", width = 22, height = 117)
    multiplot(plotlist = plots, cols = 3)
    dev.off()
}

#gets data.table
#for each genre in genres -
#gets reviews data.table from 'get.reviews.dt'
#save the linear regression to plot list
#preform 'kmeans.pca' function
#later print all 3 plots with 'multiplot' function
reviews.corr = function(dt, genres){
  plots = c()
  cnt = 1
  for(genre in genres$V1){
    temp.dt = get.genre.dt(genre, dt)
    if(nrow(temp.dt) > 100)
    {
      reviews.dt = get.reviews.dt(temp.dt)
      reviews.dt = remove.outlires(reviews.dt,"reviews.sum")
      if(nrow(reviews.dt) > 100)
      {
        two.plots = kmeans.pca(reviews.dt[,c("imdb_score","reviews.sum")], 5, "Total Reviews", "IMDB Score", "K-Means", "K-Means after pca",genre)
        plots[[cnt]] = two.plots[[1]]
        plots[[cnt+1]] = two.plots[[2]]
        plots[[cnt+2]] = (ggplot(reviews.dt, aes(x=reviews.sum, y=imdb_score))
                        + xlab("Total Reviews")
                        + ylab("IMDB Score")
                        + ggtitle(paste("Genre",genre,"Linear regression model"))
                        + geom_point(color='blue')
                        + geom_smooth(method = "lm", colour = "black"))
        cnt = cnt + 3
      }
    }
  }
  pdf("reviews.corr.pdf", width = 22, height = 117)
  multiplot(plotlist = plots, cols = 3)
  dev.off()
}

#gets data.table
#for each genre in genres -
#gets budget data.table from 'get.budget.dt'
#save the linear regression to plot list
#preform 'kmeans.pca' function
#later print all 3 plots with 'multiplot' function
budget.corr = function(dt, genres){
  plots = c()
  cnt = 1
  for(genre in genres$V1){
    temp.dt = get.genre.dt(genre, dt)
    if(nrow(temp.dt) > 100)
    {
      budget.dt = get.budget.dt(temp.dt)
      budget.dt = remove.outlires(budget.dt,"budget")
      if(nrow(budget.dt) > 100)
      {
        two.plots = kmeans.pca(budget.dt[,c("imdb_score","budget")], 5, "Budget", "IMDB Score", "K-Means", "K-Means after pca",genre)
        plots[[cnt]] = two.plots[[1]]
        plots[[cnt+1]] = two.plots[[2]]
        plots[[cnt+2]] = (ggplot(budget.dt, aes(x=budget, y=imdb_score))
                        + xlab("Budget")
                        + ylab("IMDB Score")
                        + ggtitle(paste("Genre",genre,"Linear regression model"))
                        + geom_point(color='blue')
                        + geom_smooth(method = "lm", colour = "black"))
        cnt = cnt + 3
      }
    }
  }
      
  pdf("budget.corr.pdf", width = 22, height = 117)
  multiplot(plotlist = plots, cols = 3)
  dev.off()
}

#gets data.table
#for each genre in genres - 
#gets budget data.table from 'get.profit.dt'
#save the linear regression to plot list
#preform 'kmeans.pca' function
#later print all 3 plots with 'multiplot' function
profit.corr = function(dt, genres){
  plots = c()
  cnt = 1
  for(genre in genres$V1){
    temp.dt = get.genre.dt(genre, dt)
    if(nrow(temp.dt) > 100)
    {
      profit.dt = get.profit.dt(temp.dt)
      profit.dt = remove.outlires(profit.dt,"profit",0.00001)
      if(nrow(profit.dt) > 100)
      {
        two.plots = kmeans.pca(profit.dt[,c("imdb_score","profit")], 5, "Profit", "IMDB Score", "K-Means", "K-Means after pca",genre)
        plots[[cnt]] = two.plots[[1]]
        plots[[cnt+1]] = two.plots[[2]]
        plots[[cnt+2]] = (ggplot(profit.dt, aes(x=profit, y=imdb_score))
                        + xlab("Profit")
                        + ylab("IMDB Score")
                        + ggtitle(paste("Genre",genre,"Linear regression model"))
                        + geom_point(color='blue')
                        + geom_smooth(method = "lm", colour = "black"))
        cnt = cnt + 3
      }
    }
  }
  pdf("profit.corr.pdf", width = 22, height = 117)
  multiplot(plotlist = plots, cols = 3)
  dev.off()
}

#gets data.table
#for each genre in genres - 
#gets budget data.table from 'get.gross.dt'
#save the linear regression to plot list
#preform 'kmeans.pca' function
#later print all 3 plots with 'multiplot' function
gross.corr = function(dt, genres){
  plots = c()
  cnt = 1
  for(genre in genres$V1){
    temp.dt = get.genre.dt(genre, dt)
    if(nrow(temp.dt) > 100)
    {
      gross.dt = get.gross.dt(temp.dt)
      gross.dt = remove.outlires(gross.dt,"gross",0.0001)
      if(nrow(gross.dt) > 100)
      {
        two.plots = kmeans.pca(gross.dt[,c("imdb_score","gross")], 5, "Gross", "IMDB Score", "K-Means", "K-Means after pca",genre)
        plots[[cnt]] = two.plots[[1]]
        plots[[cnt+1]] = two.plots[[2]]
        plots[[cnt+2]] = (ggplot(gross.dt, aes(x=gross, y=imdb_score))
                        + xlab("Gross")
                        + ylab("IMDB Score")
                        + ggtitle(paste("Genre",genre,"Linear regression model"))
                        + geom_point(color='blue')
                        + geom_smooth(method = "lm", colour = "black"))
        cnt = cnt + 3
      }
    }
  }
  pdf("gross.corr.pdf", width = 22, height = 100)
  multiplot(plotlist = plots, cols = 3)
  dev.off()
}

#plots all correlation plots
plot.kmeans.and.linear.regression = function(dt,genres){
  facebook.likes.corr(dt, genres)
  reviews.corr(dt, genres)
  budget.corr(dt, genres)
  profit.corr(dt, genres)
  gross.corr(dt, genres)
}

#get data.table and genre(string)
#return genre data.table(if the genre string appears in movie genre)
get.genre.dt = function (genre, dt){
  dt = dt[genres %like% genre,]
  dt
}

#aid function for 'genre.predictions' - return data.table with wanted colums
get.colums.dt = function(dt){
  dt = data.table(dt, key = c("movie_title","title_year"))
  dt.2 = data.table(get.facebook.dt(dt), key = c("movie_title","title_year"))
  dt = dt[dt.2[,c("movie_title","title_year","likes_sum")]]
  
  dt.2 = data.table(get.profit.dt(dt), key = c("movie_title","title_year"))
  dt = dt[dt.2[,c("movie_title","title_year","profit")]]
  
  dt.2 = data.table(get.gross.dt(dt), key = c("movie_title","title_year"))
  dt = dt[,-c("gross")]
  dt = dt[dt.2[,c("movie_title","title_year","gross")]]
  
  dt.2 = data.table(get.budget.dt(dt), key = c("movie_title","title_year"))
  dt = dt[,-c("budget")]
  dt = dt[dt.2[,c("movie_title","title_year","budget")]]
  
  
  dt.2 = data.table(get.reviews.dt(dt), key = c("movie_title","title_year"))
  dt = dt[dt.2[,c("movie_title","reviews.sum","title_year")]]
  dt = dt[,c("imdb_score","movie_title","title_year","reviews.sum","budget","gross","profit","likes_sum","genres")]
  dt
}

#aid function for 'genre.predictions' - calls for 'get.colums.dt' function,
#later use 'genrs.predictions' function for predicting and ploting results
geners.dt.predictions = function(dt,genres) {
  dt = get.colums.dt(dt)
  genrs.predictions(dt, genres)
}

#predict IMDB score using Random forest, Desicion tree and different colums in dt,
#plots the results to pdf
genrs.predictions = function(dt, genres){
  dt$imdb_score = floor(dt$imdb_score)
  dt[,imdb_score:=ifelse(imdb_score<5.75,1,
                         ifelse(imdb_score>=5.75 & imdb_score<6.5,2,
                                ifelse(imdb_score>=6.5 & imdb_score<7.5,3,4)))]
  
  dt$imdb_score = factor(dt$imdb_score)
  p = createDataPartition(dt$imdb_score, p=0.6, list=FALSE)
  traning.set = dt[p]
  testing.set = dt[-p]
  run_times = 0


  summary.plot.dt = matrix(nrow = dim(genres)[1],ncol = 6)
  colnames(summary.plot.dt) = c("genre" , "best.alg", "alg.acc" , "best.col.i" , "best.col.j" , "best.loop.acc")
  plots = c()
  cnt = 1
  counter = 0
  
  for(genre in genres$V1){
    counter = counter+1
    print(genre)
    dt.genre = get.genre.dt(genre, dt)
    trainging.genre.set = get.genre.dt(genre, traning.set)
    testing.genre.set = get.genre.dt(genre, testing.set)
    if((nrow(trainging.genre.set)>=36) & (nrow(testing.genre.set)>=36)){
      trainging.genre.set = trainging.genre.set[,sum.cat:=.N , by=imdb_score]
      testing.genre.set = testing.genre.set[,sum.cat:=.N , by=imdb_score]
      if(((length(unique(trainging.genre.set$imdb_score))>=4)&(min(unique(trainging.genre.set[,sum.cat])) >= 9 ))&
         ((length(unique(testing.genre.set$imdb_score))>=4)&(min(unique(testing.genre.set[,sum.cat])) >= 9 ))){
        run_times = run_times+1
        trainging.genre.set = trainging.genre.set[,-c("genres","movie_title","sum.cat","title_year")]
        
        decision.tree = train(imdb_score~.,data=trainging.genre.set , method="rpart")
        # decision.tree = train(trainging.genre.set ,trainging.genre.set$imdb_score, method="rpart")
        # print(decision.tree)
        # plot(decision.tree)
        
        random.forest = train(imdb_score~., data=trainging.genre.set, method="rf")
        # random.forest = train(trainging.genre.set,trainging.genre.set$imdb_score, method="rf")
        # print(random.forest)
        # plot(random.forest)
        
        mean.decision.tree = mean(decision.tree$results$Accuracy)
        mean.random.forest = mean(random.forest$results$Accuracy)
       
        score = 0
        max = score
        bst.i = 2
        bst.j = bst.i + 1
        
        mean.rf = mean(random.forest$results$Accuracy)
        mean.dt = mean(decision.tree$results$Accuracy)
        
        if(mean.rf > mean.dt){
          alg = "rf"
          pr = predict(random.forest,data.table(testing.genre.set))
          pt.table=table(pr,testing.genre.set$imdb_score)
          accuracy = (pt.table[1,1] + pt.table[2,2] + pt.table[3,3] + pt.table[4,4])/nrow(testing.genre.set)
          dt.result.vs.predict = data.table(result=testing.genre.set$imdb_score,predict=pr)
          g2 = ggplot(dt.result.vs.predict,aes(x=predict,fill=result))+geom_bar(position="dodge")+ggtitle(paste(genre,"-",alg,"with",accuracy,"accuracy"))
          plots[[cnt]] = g2
          cnt = cnt+1
        }
        else{
          alg = "rpart"
          pr = predict(decision.tree,data.table(testing.genre.set))
          pt.table=table(pr,testing.genre.set$imdb_score)
          accuracy = (pt.table[1,1] + pt.table[2,2] + pt.table[3,3] + pt.table[4,4])/nrow(testing.genre.set)
          dt.result.vs.predict = data.table(result=testing.genre.set$imdb_score,predict=pr)
          g2 = ggplot(dt.result.vs.predict,aes(x=predict,fill=result))+geom_bar(position="dodge")+ggtitle(paste(genre,"-",alg,"with",accuracy,"accuracy"))
          plots[[cnt]] = g2
          cnt = cnt+1
        }
        score = adjustedRandIndex(pr,testing.genre.set$imdb_score)
        
        pt.table=table(pr,testing.genre.set$imdb_score)
        accuracy = (pt.table[1,1] + pt.table[2,2] + pt.table[3,3] + pt.table[4,4])/nrow(testing.genre.set)
        print(pt.table)
        print(paste("Prediction accuracy is:",accuracy, "with algorithm",alg))
        
        testing.genre.set = testing.genre.set[,-c("genres","movie_title","title_year","sum.cat")]
        dt.genre = dt.genre[,-c("genres","movie_title","title_year")]
        colnum = ncol(testing.genre.set)
        bst.pr = NULL
        columns = colnames(dt.genre)
        for(i in 2:(colnum-1)){
          for(j in (i+1): colnum){
            # dt.data.frame = data.frame(dt.genre)[,c(1,i,j)]
            dt.data.frame = data.frame(trainging.genre.set)[,c(1,i,j)]
            testing.data.frame = data.frame(testing.genre.set)[,c(1,i,j)]
            model = train(imdb_score~.,method = alg , data = dt.data.frame)
            pr = predict(model,data.table(testing.data.frame))
            score = adjustedRandIndex(pr,testing.data.frame$imdb_score)
            
            if(i == 2 & j==3){
              max = score
              bst.pr = pr
              bst.testing.set = testing.data.frame
            }
            if(score > max){
              bst.i = i
              bst.j = j
              max = score
              bst.pr = pr
              bst.testing.set = testing.data.frame
            }
          }
        }
       
        #Summary
        text = paste("Genre", genre, ", Best columns are:" ,
                       columns[bst.i],"," ,columns[bst.j],".Score is:",toString(max))
        print(text)
        acc1 = accuracy
        pt.table = table(bst.pr,bst.testing.set$imdb_score)
        accuracy = (pt.table[1,1] + pt.table[2,2] + pt.table[3,3] + pt.table[4,4])/nrow(bst.testing.set)
        tst.dt = data.table(testing.data.frame)
        dt.result.vs.predict.loop = data.table(result=tst.dt$imdb_score,predict=bst.pr)
        g2 = ggplot(dt.result.vs.predict.loop,aes(x=predict,fill=result))+geom_bar(position="dodge")+ggtitle(paste(genre,"-",alg, ", Best columns are:" ,
                                                                                                                   columns[bst.i],"," ,columns[bst.j],"with",accuracy,"accuracy"))
        plots[[cnt]] = g2
        cnt = cnt+1
        insert.values = c(genre,alg,acc1,columns[bst.i],columns[bst.j],accuracy)
        summary.plot.dt[counter,] = insert.values
        print(pt.table)
        print(paste("Best accuracy is:",accuracy))
      }
    }
  }
  summary.plot.dt = na.omit(summary.plot.dt)
  write.table(summary.plot.dt, file = "summary.plot.dt.csv", sep = ",", col.names = TRUE,row.names = FALSE)
  print(summary.plot.dt)
  print(paste("Number of runs:",run_times))
  pdf("part2_plots.pdf", width = 22, height = 45)
  multiplot(plotlist = plots, cols = 2)
  dev.off()
}

main = function(){
  set.seed(666)
  dt = fread("movie_metadata.csv")
  dt = unique(dt, by = c("movie_title","title_year"))
  dt = dt[,-c("color","plot_keywords","movie_imdb_link","aspect_ratio")]
  dt = na.omit(dt)
  genres = data.table(unique(strsplit(paste(dt$genres, collapse = '|'), split = "\\|")[[1]]))
  plot.kmeans.and.linear.regression(dt,genres)
  geners.dt.predictions(dt,genres)
}

main()

