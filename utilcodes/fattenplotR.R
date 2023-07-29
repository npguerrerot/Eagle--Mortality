 <- function(hfig){
#  FATTENPLOT  "Fatten" all lines and text in a figure.
#
# Syntax:
#  fattenplot - fatten the current figure
#  fattenplot(hfig) - fatten hfig
#
#  James Gerber
#  Ocean Power Technologies, Inc.
#

#[AllInfo,RevisionNo]=GetSVNInfo;

if (!exist('hfig')){
    hfig <- gcf
}

figure(hfig)

#set(gca,'fontweight','bold')

h <- get(hfig,'children')

for (j in 1:length(h)){
    if (!strcmp(get(h(j),'tag'),'leg}') & !strcmp(get(h(j),'type'),'uicontrol')){
    try
        set(h(j),'fontweight','bold')
    }
        try
            hh <- get(h(j),'children')

            for (m in 1:length(hh)){
                try
                    set(hh(m),'linewidth',2)
                }
            }

        catch
            disp('problem in fattenplot')
        }
        try
            set(h(j),'linewidth',1.5)
            set(get(h(j),'ylabel'),'fontweight','bold')
            set(get(h(j),'title'),'fontweight','bold')
            set(get(h(j),'title'),'fontsize',12)
            set(get(h(j),'xlabel'),'fontweight','bold')
            set(get(h(j),'zlabel'),'fontweight','bold')
        }
        } else {
        #its a legend, or a uicontrol ignore
        set(h(j),'fontweight','bold')
    }
}

#now go after patches for contours
try
    hh <- get(h,'children')

    for (j in 1:length(hh)){
        if (strcmp(get(hh(j),'type'),'text')){
            set(hh(j),'fontweight','bold')
            set(hh(j),'fontsize',15)
        }
        if (strcmp(get(hh(j),'type'),'patch')){
            set(hh(j),'linewidth',1.5)
        }

    }
}


#now section for ubertitles
h <- findobj(hfig,'tag','UBERTITLE')
set(h,'FontWeight','bold')
set(h,'FontSize',12)
