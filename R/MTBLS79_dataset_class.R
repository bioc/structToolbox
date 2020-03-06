#' MTBLS79: Direct infusion mass spectrometry metabolomics dataset: a benchmark for data processing and quality control
#'
#' Direct-infusion mass spectrometry (DIMS) metabolomics is an important approach 
#' for characterising molecular responses of organisms to disease, drugs and the 
#' environment. Increasingly large-scale metabolomics studies are being conducted, 
#' necessitating improvements in both bioanalytical and computational workflows 
#' to maintain data quality. This dataset represents a systematic evaluation of 
#' the reproducibility of a multi-batch DIMS metabolomics study of cardiac tissue 
#' extracts. It comprises of twenty biological samples (cow vs. sheep) that were 
#' analysed repeatedly, in 8 batches across 7 days, together with a concurrent set 
#' of quality control (QC) samples. Data are presented from each step of the workflow 
#' and are available in MetaboLights (https://www.ebi.ac.uk/metabolights/MTBLS79)
#' 
#' @param filtered TRUE to load data with quality control filters already applied, 
#' or FALSE to load the unfiltered data. Default is FALSE. The raw data is available 
#' from (https://www.ebi.ac.uk/metabolights/MTBLS79) and as an R dataset in the 
#' \code{pmp} package, available on Bioconductor.
#' 
#' @export MTBLS79_DatasetExperiment
#' @return DatasetExperiment object
#' @examples
#' D = MTBLS79_DatasetExperiment()
#' summary(D)
MTBLS79_DatasetExperiment=function(filtered=FALSE) {

    if (filtered) {
        M = filter_by_name(mode='include',dimension='variable',names=to_filter)
        M = model_apply(M,MTBLS79_corrected)
        return(predicted(M))
    } else {
        return(MTBLS79_corrected)
    }
}