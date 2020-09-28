#' Create species distribution models
#'
#' This function runs biomod2 and create species distributions models based on
#' user inputs. As prerequisite BIOMOD_FormatingData() needs to be run as well
#' as environmental layers stack. This function just wraps around biomod2
#' functions BIOMOD_Modeling() and BIOMOD_EnsembleModeling() to produce models
#' with consistent naming.
#'
#' @param data (Biomod formatting object), Biomod formatted data  that is
#' produced as result from BIOMOD_FormattingData() function
#' @param method (character), modelling  method used to create model
#' @param eval_metric_quality_threshold (number) If not NULL, the minimum scores below
#' which models will be excluded of the ensemble-models building
#' @param nb_run_eval (number), number of Evaluation runs
#' @param path_to_bin (character), the link to maxent.jar file (the bin folder in
#' working directory by default)
#'
#' @export
#' @importFrom magrittr "%>%"
#' @import data.table

create_models = function(
  data,
  method,
  eval_metric_quality_threshold = NULL,
  nb_run_eval,
  path_to_bin = "bin/maxent.jar"
) {
  # set magrittr dot variable to NULL
  . = NULL

  model_name = ifelse(length(method) != 1, "EM", method)

  modelling_id = Sys.time() %>%
    format(., "%Y%m%d%H%M") %>%
    paste0(., ".", model_name)

  biomod_options = biomod2::BIOMOD_ModelingOptions(
    MAXENT.Phillips = list(
      path_to_maxent.jar = path_to_bin,
      memory_allocated = 1024,
      background_data_dir = "default",
      maximumbackground = "default",
      maximumiterations = 500,
      visible = TRUE,
      linear = TRUE,
      quadratic = TRUE,
      product = TRUE,
      threshold = TRUE,
      hinge = TRUE,
      lq2lqptthreshold = 80,
      l2lqthreshold = 10,
      hingethreshold = 15,
      beta_threshold = -1,
      beta_categorical = -1,
      beta_lqp = -1,
      beta_hinge = -1,
      betamultiplier = 1,
      defaultprevalence = 0.5
    )
  )

  model_out = biomod2::BIOMOD_Modeling(
    data,
    models = method,
    NbRunEval = nb_run_eval,
    DataSplit = 70,
    Prevalence = 0.5,
    VarImport = 3,
    models.eval.meth = c("TSS","ROC"),
    rescal.all.models = FALSE,
    modeling.id = modelling_id,
    do.full.models = FALSE
  )

  emodel_out = biomod2::BIOMOD_EnsembleModeling(
    modeling.output = model_out,
    chosen.models = "all",
    em.by = "all",
    eval.metric = c("ROC"),
    eval.metric.quality.threshold = eval_metric_quality_threshold,
    models.eval.meth = c("TSS", "ROC"),
    prob.mean = TRUE,
    prob.cv = FALSE,
    prob.ci = FALSE,
    prob.ci.alpha = 0.05,
    prob.median = FALSE,
    committee.averaging = FALSE,
    prob.mean.weight = TRUE,
    prob.mean.weight.decay = "proportional",
    VarImport = 3
  )
}
