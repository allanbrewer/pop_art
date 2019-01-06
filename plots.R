if (!require(tidyverse)) { install.packages("tidyverse") }
if (!require (broom)) {install.packages("broom")}

## Residual Plot

plot1 <- ggplot(trainFinal, aes(x=fit1$fit, y=fit1$res, color = Auction))
plot1 <- plot1 + geom_point() + geom_hline(yintercept=0, linetype="dashed", color = "red")
plot1 <- plot1 + labs(title="Plot 1: Residuals vs Fitted Values" , x="Fitted Values", y = "Residuals") + theme(plot.title = element_text(hjust = 0.5))
plot1

## QQ Plot

qq <- cbind(augment(fit1),f=trainFinal$Auction)
qq = cbind(qq, setNames(qqnorm(qq$.resid, plot.it=FALSE), c("Theoretical", "Sample")))
plot2 <- ggplot(qq, aes(sample = qq$.resid))
plot2 <- plot2 + geom_point(shape=1, size = 1.5, aes(x=Theoretical, y=Sample, colour=f)) + labs(color='Auction')
plot2 <- plot2 + stat_qq_line(colour = "black", linetype = "twodash")
plot2 <- plot2 + labs(title="Plot 2: Normal Q-Q Plot" , x="Theoretical Quantiles", y = "Sample Quantiles") + theme(plot.title = element_text(hjust = 0.5))
plot2

## Residual by Year

plot3 <- ggplot(trainFinal, aes(x=Year, y=fit1$resid, color=Year)) 
plot3 <- plot3 + geom_boxplot() 
plot3 <- plot3 + labs(title="Plot 3: Box plot Residuals by Year" , x="Group", y = "Residuals") + theme(plot.title = element_text(hjust = 0.5), legend.position='none')
plot3

## Index Plot

plot4 <- ggplot(data = index, mapping = aes(x = Year))
#plot4 <- plot4 + geom_point(aes(y = Index), shape = 21) + geom_line(aes(y = Index, colour = "Uncorrected"))
plot4 <- plot4 + geom_point(aes(y = Corrected.Index), shape = 23) + geom_line(aes(y = Corrected.Index, colour = "Art Corrected"))
plot4 <- plot4 + geom_point(aes(y = SP), shape = 22) + geom_line(aes(y = SP, colour = "S&P"))
plot4 <- plot4 + labs(title="Plot 4: Art and S&P Index", y = "Index", x = "Year", colour = "Index Type") + theme(plot.title = element_text(hjust = 0.5))
plot4 <- plot4 + scale_x_continuous(breaks = c(2001:2012))
plot4 <- plot4 + annotate("text", x = 2003, y = 240, label = paste("TWRR Art ", returns[13,2], "%", sep = ""))
plot4 <- plot4 + annotate("text", x = 2003, y = 230, label = paste("TWRR S&P ", returns[13,3], "%", sep = ""))
plot4

## Predicted vs Actual Prices Plots

plot5 <- ggplot(real_pred, aes(x=real, y=pred, color = Place, shape = Origin)) 
plot5 <- plot5 + geom_point() + geom_abline(intercept = 0, slope = 1, linetype="dashed")
plot5 <- plot5 + labs(title="Plot 5: Scatter plot Real vs. Predicted Prices" , x="Log(Real Price)", y = "Log(Predicted Price)") 
plot5 <- plot5 + theme(plot.title = element_text(hjust = 0.5))
plot5 <- plot5 + annotate("text", x = 7.5, y = 13, label = paste("Correlation ", round(cor(real, pred), 2), sep = ""))
plot5

plot6 <- ggplot(data = real_pred, mapping = aes(x = real, color = Place, shape = Origin))
plot6 <- plot6 + geom_point(aes(y = real-pred)) + geom_hline(yintercept=0, linetype="dashed")
plot6 <- plot6 + labs(title="Plot 6: Out of Sample Residual" , x="Log(Real Price)", y = "Residual")
plot6 <- plot6 + theme(plot.title = element_text(hjust = 0.5))
plot6

#plot5 <- ggplot(data = real_pred, mapping = aes(x = 1:length(pred)))
#plot5 <- plot5 + geom_point(aes(y = real, colour = "Actual Price"), shape = 1) + geom_smooth(aes(y = real, colour = "Actual Best Fit"))
#plot5 <- plot5 + geom_point(aes(y = pred, colour = "Predicted Price"), shape = 1) + geom_smooth(aes(y = pred, colour = "Predicted Best Fit"))
#plot5 <- plot5 + labs(title="Plot 5: Real and Predicted Prices by Index" , x="Index", y = "Log(Price)", colour = "Type")
#plot5 <- plot5 + theme(plot.title = element_text(hjust = 0.5))
#plot5

#plot(1:length(real),real,col="green",xlab="Index",ylab="log(Price)",main="Real vs. Predicted Prices" )
#lines(loess.smooth(1:length(real),real),add=TRUE,col="red", lwd = 2)
#points(pred, col="blue")
#lines(loess.smooth(1:length(pred),pred),add=TRUE,col="black", lwd = 2)

## EAD

#Origin
BP2 <- ggplot(train2, aes(x=Origin, y=PriceLN, color=Origin))
BP2 <- BP2 + geom_boxplot()
BP2 <- BP2 + labs(title="Box plot Log Price by Origin of Artist" , y = "Log Price") + theme(plot.title = element_text(hjust = 0.5), legend.position='none')
BP2

#auction
BP3 <- ggplot(train2, aes(x=Auction, y=PriceLN, color=Auction)) 
BP3 <- BP3 + geom_boxplot() 
BP3 <- BP3 + labs(title="Box plot Log Price by Auction House" , y = "Log Price") + theme(plot.title = element_text(hjust = 0.5), legend.position='none')
BP3

#materials
BP4 <- ggplot(train2, aes(x=Material, y=PriceLN, color=Material)) 
BP4 <- BP4 + geom_boxplot() 
BP4 <- BP4 + labs(title="Box plot Log Price by Material of Art Work" , y = "Log Price") + theme(plot.title = element_text(hjust = 0.5), legend.position='none')
BP4

#year
BP5 <- ggplot(train2, aes(x=Year, y=PriceLN, color=Year)) 
BP5 <- BP5 + geom_boxplot() 
BP5 <- BP5 + labs(title="Box plot Log Price by Year of Sale" , y = "Log Price") + theme(plot.title = element_text(hjust = 0.5), legend.position='none', axis.text=element_text(size=5))
BP5

#month
BP6 <- ggplot(train2, aes(x=Month, y=PriceLN, color=Month)) 
BP6 <- BP6 + geom_boxplot()
BP6 <- BP6 + labs(title="Box plot Log Price by Month of Sale" , y = "Log Price") + theme(plot.title = element_text(hjust = 0.5), legend.position='none', axis.text=element_text(size=7))
BP6

#alive
BP7 <- ggplot(train2, aes(x=Alive, y=PriceLN, color=Alive)) 
BP7 <- BP7 + geom_boxplot()
BP7 <- BP7 + labs(title="Box plot Log Price by Vital Status" , y = "Log Price") + theme(plot.title = element_text(hjust = 0.5), legend.position='none')
BP7

#signed
BP8 <- ggplot(train2, aes(x=Signed, y=PriceLN, color=Signed)) 
BP8 <- BP8 + geom_boxplot()
BP8 <- BP8 + labs(title="Box plot Log Price by Markings" , y = "Log Price") + theme(plot.title = element_text(hjust = 0.5), legend.position='none')
BP8

#dated
BP9 <- ggplot(train2, aes(x=Dated, y=PriceLN, color=Dated)) 
BP9 <- BP9 + geom_boxplot()
BP9 <- BP9 + labs(title="Box plot Log Price by Markings" , y = "Log Price") + theme(plot.title = element_text(hjust = 0.5), legend.position='none')
BP9

#area
SP <- ggplot(train2, aes(x=PriceLN, y=AreaLN, color = Auction))
SP <- SP + geom_point()
SP <- SP + labs(title="Log of Price vs Log of Area" , x="Log of Price", y = "Log of Area") + theme(plot.title = element_text(hjust = 0.5))
SP

## Dendograms

plot(hclust(dist(table(dat$Origin))), xlab = "")

plot(hclust(dist(sort(table(dat$Place), decreasing = TRUE)[1:15])), xlab = "")

## Year Plots

\begin{figure}[!ht]
<< year_count, echo=FALSE, tidy = TRUE, warning=FALSE, include = FALSE>>=
png("figures/time.png", res = 75)
plot9 <- ggplot(train2, aes(x=Year))
plot9 <- plot9 + geom_histogram(stat="count", aes(fill = Year))
plot9 <- plot9 + scale_fill_manual(values=wes_palette(n = 12, name="Zissou1", type = "continuous"))
plot9 <- plot9 + labs(title="Artworks sold by Year" , x="Year", y = "Count")
plot9 <- plot9 + theme(plot.title = element_text(hjust = 0.5), title=element_text(size=17), axis.text=element_text(size=13), axis.title=element_text(size=15), legend.text=element_text(size=13), legend.position='none')
plot9
dev.off()
@
\includegraphics[scale = 0.5]{time}
\caption{Histogram of artworks sold each year}
\label{year_count}
\end{figure}

