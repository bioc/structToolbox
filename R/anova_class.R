#' @eval get_description('ANOVA')
#' @include entity_objects.R zzz.R
#' @import struct
#' @import stats
#' @examples
#' D = iris_DatasetExperiment()
#' M = ANOVA(formula=y~Species)
#' M = model_apply(M,D)
#' @export ANOVA
ANOVA = function(alpha=0.05,mtc='fdr',formula,ss_type='III',...) {
    out=struct::new_struct('ANOVA',
        alpha=alpha,
        mtc=mtc,
        formula=formula,
        ss_type=ss_type,
        ...)
    return(out)
}


.ANOVA<-setClass(
    "ANOVA",
    contains=c('model'),
    slots=c(
        # INPUTS
        alpha='entity',
        mtc='enum',
        formula='entity',
        ss_type='enum',
        # OUTPUTS
        f_statistic='entity',
        p_value='entity',
        significant='entity'
    ),
    prototype = list(name='Analysis of Variance',
        description=paste0(
            "Analysis of Variance (ANOVA) is a univariate method used to ",
            "analyse the difference among ",
            "group means. Multiple test corrected p-values are computed to ",
            "indicate significance for each feature."),
        type="univariate",
        predicted='p_value',
        ontology="OBI:0200201",
        libraries='car',
        .params=c('alpha','mtc','formula','ss_type'),
        .outputs=c('f_statistic','p_value','significant'),

        alpha=ents$alpha,
        mtc=ents$mtc,
        formula=ents$formula,

        ss_type=enum(name='ANOVA sum of squares',
            description=c(
                'I' = 'Type I sum of squares.',
                'II' = 'Type II sum of squares.',
                'III' = 'Type III sum of squares.'
            ),
            value='III',
            type='character',
            allowed=c('I','II','III')
        ),

        f_statistic=ents$f_statistic,
        p_value=ents$p_value,
        significant=ents$significant
    )
)


#' @export
#' @template model_apply
setMethod(f="model_apply",
    signature=c("ANOVA",'DatasetExperiment'),
    definition=function(M,D)
    {
        X=D$data
        var_names=all.vars(M$formula)
        
        # attempt to detect within factors
        within=which(var_names %in% all.names(M$formula)[which('Error'== all.names(M$formula))+2])
        
        if (length(within)>0) {
            var_names_ex=var_names[-within]
        } else {
            var_names_ex=var_names
        }
        
        y=D$sample_meta[var_names[2:length(var_names)]]
        
        # set the contrasts
        O=options('contrasts') # keep the old ones
        options(contrasts = c("contr.sum","contr.poly"))
        
        output=apply(X,2,function(x) {
            temp=y
            temp[[var_names[1]]]=x
            
            # check number levels
            temp2=na.omit(temp)
            s=sapply(var_names_ex[2:length(var_names)],function(x) summary(temp2[[x]]))
            n=nrow(temp2)
            
            # check for alias columns
            dona=FALSE
            if (all(unlist(s)>2)) { # check we have enough levels
                temp3=temp[,var_names_ex] # ignore within factors
                al=alias(M$formula,temp3) # check we have independent columns
                if ('Complete' %in% names(al)) {
                    dona=TRUE
                }
            } else {
                dona=TRUE
            }
            
            # check for enough samples
            temp3=temp
            temp3[[var_names[1]]]=rnorm(nrow(y))
            LM=lm(formula=M$formula,data=temp3)
            p=nrow(summary(LM)$coefficients)
            if (n<(p+1)) {
                dona=TRUE
            }
            
            if (dona) {
                # missing values have probably prevented one or more combinations of levels being present
                # use some fake data to generate the output table then replace all the values with NA
                temp[[var_names[1]]]=rnorm(nrow(y))
                LM=lm(formula=M$formula,data=temp)
                if (M$ss_type=='I') {
                    A=anova(LM)  
                } else {
                    A=car::Anova(LM,type=M$ss_type)
                }
                A[!is.na(A)]=NA
                return(A)
            }
            
            LM=lm(formula=M$formula,data=temp)
            if (M$ss_type=='I') {
                A=anova(LM)  
            } else {
                A=car::Anova(LM,type=M$ss_type)
            }
            return(A)
        })
        
        f_statistic=sapply(output,function(x){
            x$`F value`
        })
        f_statistic=as.data.frame(t(f_statistic))
        colnames(f_statistic)=rownames(output[[1]])
        f_statistic=f_statistic[,colnames(y),drop=FALSE]
        
        
        p_value=sapply(output,function(x){
            x$`Pr(>F)`
        })
        p_value=as.data.frame(t(p_value))
        colnames(p_value)=rownames(output[[1]])
        p_value=p_value[,colnames(y),drop=FALSE]
        
        # fdr correct the p.values
        for (k in 1:ncol(p_value)) {
            p_value[, k]=p.adjust(p_value[ ,k],M$mtc)
        }
        
        # populate the object
        M$f_statistic=f_statistic
        M$p_value=p_value
        M$significant=as.data.frame(p_value<M$alpha)
        
        # reset the contrasts
        options(O)
        
        return(M)
    }
)









