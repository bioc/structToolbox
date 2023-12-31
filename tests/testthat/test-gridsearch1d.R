# test grid search
test_that('grid_search iterator',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
      factor_name='Species',
      search_values=as.numeric(1:4),
      model_index=2,
      max_min='min')*
    kfold_xval(folds=5,factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$value,0.045,tolerance=0.0005)
})

# test grid search
test_that('grid_search wf',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
    factor_name='Species',
    search_values=as.numeric(1:4),
    model_index=2,
    max_min='min')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  expect_equal(I$metric$value[1],0.04,tolerance=0.005)
})

# test grid search
test_that('grid_search chart',{
  set.seed('57475')
  # DatasetExperiment
  D=iris_DatasetExperiment()
  # iterator
  I = grid_search_1d(param_to_optimise='number_components',
    factor_name='Species',
    search_values=as.numeric(1:4),
    model_index=2,
    max_min='min')*
    kfold_xval(folds=5,factor_name='Species')*
    (mean_centre()+PLSDA(factor_name='Species'))
  # metric
  B=balanced_accuracy()
  # run
  I=run(I,D,B)
  # calculate metric
  C=gs_line()
  gg=chart_plot(C,I)
  expect_true(is(gg,'ggplot'))
})
