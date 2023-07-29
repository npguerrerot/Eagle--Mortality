 <- function(hfig){
# REALLYFATTENPLOT - make fonts bold and lines fatter
#
# SYNTAX
# ReallyfattenPlot(hfig) - fatten figure hfig
if (!exist('hfig')){
   hfig <- gcf
}

figure(hfig)

#set(gca,'fontweight','bold')

h <- get(hfig,'children')

for (j in 1:length(h)){
   if (!strcmp(get(h(j),'tag'),'leg}') & !strcmp(get(h(j),'type'),'uicontrol')){
      set(h(j),'fontweight','bold','FontSize',25)
     try
         set(get(h(j),'children'),'linewidth',6)
     catch
         disp(['problem in fattenplot'])
     }
     set(h(j),'linewidth',3)
      set(get(h(j),'ylabel'),'fontweight','bold','FontSize',25)
      set(get(h(j),'zlabel'),'fontweight','bold','FontSize',25)
      set(get(h(j),'title'),'fontweight','bold','FontSize',25)
      set(get(h(j),'xlabel'),'fontweight','bold','FontSize',25)
  } else {
      #its a legend, or a uicontrol ignore
     set(h(j),'fontweight','bold')
  }
}

#now section for ubertitles
h <- findobj(hfig,'tag','UBERTITLE')
set(h,'FontWeight','bold','FontSize',25)
