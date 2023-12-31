#' @eval get_description('resample')
#' @examples
#' I = resample(
#'     number_of_iterations = 10, 
#'     factor_name = 'Species', 
#'     method = 'split_data',
#'     p_train = 0.8)
#' @export resample
resample = function(
    number_of_iterations=10,
    method='split_data',
    factor_name,
    p_train=0.8,
    collect=NULL,...) {
    out=struct::new_struct('resample',
        number_of_iterations=number_of_iterations,
        method=method,
        factor_name=factor_name,
        p_train=p_train,
        collect=collect,
        ...)
    return(out)
}

.resample<-setClass(
    "resample",
    contains='resampler',
    slots=c(
        number_of_iterations='entity',
        method='enum',
        factor_name='entity',
        p_train='entity',
        results.training='data.frame',
        results.testing='data.frame',
        metric='data.frame',
        metric.train='numeric',
        metric.test='numeric',
        collect='entity',
        collected='entity'
    ),
    prototype = list(name='Data resampling',
        description=paste0('New training sets are generated from the original data ',
            'by selecting samples at random. This can be based on levels in a ',
            'factor or on the whole dataset.'),
        type="resampler",
        result='results.testing',
        number_of_iterations=entity(
            name='Number of iterations',
            description = 'The number of training sets to generate.',
            type=c('numeric','integer'),
            value=100,
            max_length=1
        ),
        method=enum(
            name='Resampling method',
            description = c(
                'split_data' = 'Samples for the training set are selected at random from the full dataset.',
                'stratified_split' = 'Samples for the training set are randomly selected from each level of the chosen factor.',
                'equal_split' = 'Samples for the training set are selected at random from each level of the main factor such that all group sizes are equal.'
            ),
            type=c('character'),
            value='split_data',
            max_length=1,
            allowed=c('split_data','stratified_split','equal_split')
        ),
        p_train=entity(name = 'Proportion in training set',
            description = paste0('The proportion of samples selected for the ',
                'training set.'),
            value = 0.75,
            type='numeric'
        ),
        factor_name=ents$factor_name,
        collect=entity(name = 'Collect output',
            description=paste0('The name of a model output to collect over all ',
                'bootstrap repetitions, in addition to the input metric.'),
            value = NULL,
            max_length = Inf,
            type=c('NULL','character')),
        collected=entity(name='collected output',
            type=c('list'),
            value=list(),max_length=Inf),
        .params=c('number_of_iterations','method','factor_name','p_train','collect'),
        .outputs=c('results.training','results.testing','metric','collected','metric.train','metric.test')
    )
)

#' @export
#' @template run
setMethod(f="run",
    signature=c("resample",'DatasetExperiment','metric'),
    definition=function(I,D,MET) {
        
        # add an indexing column
        D$sample_meta$resample_idx=1:nrow(D)
        
        X=D$data
        y=D$sample_meta[,I$factor_name,drop=FALSE]
        
        n=param_value(I,'number_of_iterations')
        
        all_results_training=data.frame('actual'=rep(y[,1],n),'predicted'=NA,'iteration'=0)
        all_results_testing=data.frame('actual'=rep(y[,1],n),'predicted'=NA,'iteration'=0)
        
        
        tr.metric=numeric(n)
        te.metric=numeric(n)
        
        collected=list()
        
        for (i in 1:n)
        {
            # get the WF
            WF=models(I)
            
            # subsample the data
            if (I$method=='split_data') {
                S = split_data(p_train=I$p_train)
            } else if (I$method=='stratified_split') {
                S = stratified_split(p_train=I$p_train, factor_name=I$factor_name)
            } else if (I$method=='equal_split') {
                S = equal_split(p_train=I$p_train,factor_name=I$factor_name)
            }
            S = model_apply(S,D)
            
            # tables to store results
            Yp=D$sample_meta[[I$factor_name]]
            train_results=data.frame('actual'=Yp,'predicted'=NA,'iteration'=i)
            test_results=data.frame('actual'=Yp,'predicted'=NA,'iteration'=i)
            
            # WF can be a model/model list
            if (is(WF,'model_OR_model_seq'))
            {
                ## training set
                WF=model_train(WF,S$training)
                # predict
                WF=model_predict(WF,S$training)
                p=predicted(WF)
                train_results[S$training$sample_meta$resample_idx,2]=p[,1]
                all_results_training[((nrow(X)*(i-1))+1):(nrow(X)*i),]=train_results
                
                # calculate metric
                MET=calculate(MET,S$training$sample_meta[[I$factor_name]],p)
                tr.metric[i]=value(MET)
                
                # collect from training set
                if (!is.null(I$collect)) {
                    
                    temp_collect=list()
                    for (k in I$collect) {
                        
                        if (is(WF,'model')) {
                            temp_collect=c(temp_collect,list(output_value(WF,k)))
                        } else {
                            # if sequence assume collecting from last index
                            temp_collect=c(temp_collect,list(output_value(WF[length(WF)],k)))
                        }
                    }
                    names(temp_collect)=I$collect
                    collected=c(collected,list(temp_collect))
                }
                
                ## testing set
                # predict
                WF=model_predict(WF,S$testing)
                p=predicted(WF)
                test_results[S$testing$sample_meta$resample_idx,2]=p[,1]
                all_results_testing[((nrow(X)*(i-1))+1):(nrow(X)*i),]=test_results
                
                # calculate metric
                MET=calculate(MET,S$testing$sample_meta[[I$factor_name]],p)
                te.metric[i]=value(MET)
            }
            else
            {
                WF=run(WF,S$training,MET)
                v=output_value(WF,'metric')
                
                if (i==1) {
                    all_results_training=v
                }
                else {
                    all_results_training=rbind(all_results_training,v)
                }
                
                ## real
                WF=run(WF,S$testing,MET)
                w=output_value(WF,'metric')
                if (i==1) {
                    all_results_testing=w
                }
                else {
                    all_results_testing=rbind(all_results_testing,w)
                }
            }
            
        }
        
        results=all_results_training
        k=max(results$iteration)
        ts.metric=numeric(k)
        te.metric=numeric(k)
        for (i in 1:k) {
            # training set
            ts=results[results$iteration==i & !is.na(results$predicted),]
            MET=calculate(MET,ts$actual,factor(ts$predicted,labels=levels(ts$actual),levels=1:length(levels(ts$actual))))
            ts.metric[i]=value(MET)
        }
        
        # test set
        results=all_results_testing
        for (i in 1:k) {
            # testing set
            te=results[results$iteration==i & !is.na(results$predicted),]
            MET=calculate(MET,te$actual,factor(te$predicted,labels=levels(te$actual),levels=1:length(levels(te$actual))))
            te.metric[i]=value(MET)
        }
        out=data.frame('metric'=class(MET)[1],'mean'=mean(te.metric),'sd'=sd(te.metric))
        
        output_value(I,'metric')=out
        output_value(I,'metric.train')=ts.metric
        output_value(I,'metric.test')=te.metric
        
        I$collected=collected
        
        I$results.training=all_results_training
        I$results.testing=all_results_testing
        return(I)
    }
)


#' resample_chart class
#'
#' Plots the results of a resampling.
#' @examples
#' C = resample_chart(style='boxplot')
#' @param style The plot style. One of 'boxplot', 'violin', 'histogram', 'density' or 'scatter'.
#' @param binwidth Binwidth for the "histogram" style. Ignored for all other styles.
#' @param ... additional slots and values passed to struct_class
#' @return struct object
#' @export resample_chart
resample_chart = function(style = 'boxplot',binwidth=0.05,...) {
    out=struct::new_struct('resample_chart',
        style=style,
        binwidth=binwidth,
        ...)
    return(out)
}


.resample_chart<-setClass(
    "resample_chart",
    contains='chart',
    slots=c(style='enum',
        binwidth='numeric'),
    prototype = list(
        name='resample_chart',
        type='boxplot',
        description='A plot of the calculated metric for the model with training and testing labels.',
        .params=c('style','binwidth'),
        style=enum(name='Plot style',
            description=c(
                'boxplot' = 'A boxplot to visualise the performance metrics',
                'violin' = 'A violin plot to visualise the performance metrics',
                'histogram' = 'A histogram of the computed performance metrics',
                'density' = 'A density plot of the computed metrics',
                'scatter' = 'A scatter plot of the computed metrics'),
            type='character',
            value='boxplot',
            allowed=c('boxplot','violin','histogram','scatter','density')
        ),
        binwidth = 0.05
    )
)

#' @export
#' @template chart_plot
setMethod(f="chart_plot",
    signature=c('resample_chart','resample'),
    definition=function(obj,dobj) {
        
        if (is(models(dobj),'iterator')) {
            p=output_value(dobj,'metric.train')
            u=output_value(dobj,'metric.test')
            
            A=data.frame(
                'value'=c(p$mean,u$mean),
                'dataset'=c(rep('Training',nrow(p)),rep('Testing',nrow(u))))
        } else {
            p=dobj$metric.train
            u=dobj$metric.test
            A=data.frame(
                'value'=c(p,u),
                'dataset'=c(rep('Training',length(p)),rep('Testing',length(u))))
        }
        A$dataset=factor(A$dataset,levels=c('Training','Testing'))
        
        plotClass= createClassAndColors(A$dataset)
        
        if (obj$style=='boxplot') {
            out=ggplot(data=A,aes_(x=~dataset,y=~value,color=~dataset)) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
                geom_boxplot() +
                theme(legend.position="none") +xlab('Dataset')
            
        } else if (obj$style=='violin') {
            out=ggplot(data=A,aes_(x=~dataset,y=~value,color=~dataset)) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
                geom_violin(trim=F) +
                theme(legend.position="none") +xlab('Dataset')
            
        } else if (obj$style=='histogram') {
            out=ggplot(data=A,aes_(x=~value,color=~dataset)) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
                geom_freqpoly(binwidth = obj$binwidth)
            
        } else if (obj$style=='scatter') {
            out=ggplot(data=A,aes_(x=1:nrow(A),y=~value,color=~dataset)) +
                geom_point(na.rm=T) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
                theme(axis.title.x=element_blank(),
                    axis.text.x=element_blank(),
                    axis.ticks.x=element_blank())
        } else if (obj$style=='density') {
            out=ggplot(data=A,aes_(x=~value,color=~dataset,fill=~dataset)) +
                scale_color_manual(values=plotClass$manual_colors,name='Dataset') +
                scale_fill_manual(values=plotClass$manual_colors,name='Dataset') +
                geom_density(alpha = 0.3)
        }
        
        out = out + theme_Publication(base_size = 12) 
        
        
        return(out)
    }
)



