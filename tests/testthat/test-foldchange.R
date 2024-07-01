

test_that('fold_change unpaired',{
    set.seed('57475')
    
    # data
    D = iris_DatasetExperiment()
    
    # add some missing values
    D$data[1:50,2] = NA
    D$data[1:25,3] = NA
    
    # unpaired
    FF = fold_change(factor_name='Species',method="geometric",control_group='versicolor')
    FF = model_apply(FF,D)
    # check some fold changes
    m=exp(mean(log(na.exclude(D$data[D$sample_meta$Species=='virginica',1])))) / exp(mean(log(na.exclude(D$data[D$sample_meta$Species=='versicolor',1]))))
    expect_equal(FF$fold_change$`virginica/versicolor`[1],m,tolerance=0.00001)
    
    m=exp(mean(log(na.exclude(D$data[D$sample_meta$Species=='setosa',4])))) / exp(mean(log(na.exclude(D$data[D$sample_meta$Species=='versicolor',4]))))
    expect_equal(FF$fold_change$`setosa/versicolor`[4],m,tolerance=0.00001)
    
    m=exp(mean(log(na.exclude(D$data[D$sample_meta$Species=='virginica',3])))) / exp(mean(log(na.exclude(D$data[D$sample_meta$Species=='setosa',3]))))
    expect_equal(FF$fold_change$`virginica/setosa`[3],m,tolerance=0.00001)
    
    # check some NA
    expect_true(is.na(FF$fold_change$`virginica/setosa`[2]))
    expect_true(is.na(FF$fold_change$`setosa/versicolor`[2]))
    
    FF$method = 'median'
    FF = model_apply(FF,D)
    # check some fold changes
    m = median(D$data[D$sample_meta$Species=='virginica',1],na.rm = TRUE) / median(D$data[D$sample_meta$Species=='versicolor',1],na.rm = TRUE)
    expect_equal(FF$fold_change$`virginica/versicolor`[1],m,tolerance=0.00001)
    
    m = median(D$data[D$sample_meta$Species=='setosa',4],na.rm = TRUE) / median(D$data[D$sample_meta$Species=='versicolor',4],na.rm = TRUE)
    expect_equal(FF$fold_change$`setosa/versicolor`[4],m,tolerance=0.00001)
    
    m = median(D$data[D$sample_meta$Species=='virginica',3],na.rm = TRUE) / median(D$data[D$sample_meta$Species=='setosa',3],na.rm = TRUE)
    expect_equal(FF$fold_change$`virginica/setosa`[3],m,tolerance=0.00001)
    
    # check some NA
    expect_true(is.na(FF$fold_change$`virginica/setosa`[2]))
    expect_true(is.na(FF$fold_change$`setosa/versicolor`[2]))
    
    FF$method = 'mean'
    FF = model_apply(FF,D)
    m = mean(D$data[D$sample_meta$Species=='virginica',1],na.rm = TRUE) / mean(D$data[D$sample_meta$Species=='versicolor',1],na.rm = TRUE)
    expect_equal(FF$fold_change$`virginica/versicolor`[1],m,tolerance=0.00001)
    
    m = mean(D$data[D$sample_meta$Species=='setosa',4],na.rm = TRUE) / mean(D$data[D$sample_meta$Species=='versicolor',4],na.rm = TRUE)
    expect_equal(FF$fold_change$`setosa/versicolor`[4],m,tolerance=0.00001)
    
    m = mean(D$data[D$sample_meta$Species=='virginica',3],na.rm = TRUE) / mean(D$data[D$sample_meta$Species=='setosa',3],na.rm = TRUE)
    expect_equal(FF$fold_change$`virginica/setosa`[3],m,tolerance=0.00001)
    
    # check some NA
    expect_true(is.na(FF$fold_change$`virginica/setosa`[2]))
    expect_true(is.na(FF$fold_change$`setosa/versicolor`[2]))
    
    
})

test_that('fold_change paired',{
    set.seed('57475')
    
    # data
    D = iris_DatasetExperiment()
    
    # two groups
    F = filter_smeta(mode='exclude',levels='setosa',factor_name='Species')
    F = model_apply(F,D)
    
    D = predicted(F)
    
    # add column for paired data
    D$sample_meta$sample_id=c(1:50,1:50)
    D$data[1:50,2] = NA
    D$data[1:25,3] = NA
    
    # paired
    FF = fold_change(factor_name='Species',method="geometric",paired=TRUE,sample_name = 'sample_id',control_group = 'versicolor')
    FF = model_apply(FF,D)
    m=exp(mean(log(D$data[D$sample_meta$Species=='virginica',1])-log(D$data[D$sample_meta$Species=='versicolor',1])))
    expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
    expect_true(is.na(FF$fold_change[2,1]))
    
    FF$method = 'median'
    FF = model_apply(FF,D)
    m = median(D$data[D$sample_meta$Species=='virginica',1] / D$data[D$sample_meta$Species=='versicolor',1])
    expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
    expect_true(is.na(FF$fold_change[2,1]))
    
    FF$method = 'mean'
    FF = model_apply(FF,D)
    m = mean(D$data[D$sample_meta$Species=='virginica',1] / D$data[D$sample_meta$Species=='versicolor',1])
    expect_equal(FF$fold_change[1,1],m,tolerance=0.00001)
    expect_true(is.na(FF$fold_change[2,1]))
    
    
})