.PHONY: plots clean

data/test.csv data/train.csv: sim.R
	@echo "########################################"
	@echo "Generating training and test data..."
	Rscript sim.R

data/outcomes_grid_cf_eo.csv: data/test.csv \
															data/train.csv \
															counterfactual_equalized_odds.py
	@echo "########################################"
	@echo "Testing counterfactual equalized odds..."
	python3 counterfactual_equalized_odds.py

data/outcomes_grid_cf_fairness.csv: data/test.csv \
																	  data/train.csv \
																	  counterfactual_fairness.py
	@echo "########################################"
	@echo "Testing counterfactual fairness..."
	python3 counterfactual_fairness.py

data/outcomes_grid_cf_pred_par.csv: data/test.csv \
																    data/train.csv \
																	  counterfactual_predictive_parity.py
	@echo "########################################"
	@echo "Testing counterfactual predictive parity..."
	python3 counterfactual_predictive_parity.py

data/outcomes_grid_principal_fairness.csv: data/test.csv \
																		 			 data/train.csv \
																		       principal_fairness.py
	@echo "########################################"
	@echo "Testing principal fairness..."
	python3 principal_fairness.py

figures/frontier_cf_eo.pdf \
figures/frontier_cf_fairness.pdf \
figures/frontier_cf_pred_par.pdf \
figures/frontier_principal_fairness.pdf: data/outcomes_grid_cf_eo.csv \
																				 data/outcomes_grid_cf_fairness.csv \
																				 data/outcomes_grid_cf_pred_par.csv \
																				 data/outcomes_grid_principal_fairness.csv \
																				 geometry.R
	@echo "########################################"
	@echo "Generating geometry of fairness plots..."
	Rscript geometry.R

figures/calibration_risk_dist.pdf \
figures/density.pdf \
figures/diabetes.pdf \
figures/diabetes_calibration.pdf: diabetes.R
	@echo "########################################"
	@echo "Generating diabetes plots..."
	Rscript diabetes.R

figures/calibration.pdf \
figures/label_bias.pdf \
figures/pareto_full.pdf \
figures/pareto_sub.pdf \
figures/treatment_effects.pdf: cmc.R
	@echo "########################################"
	@echo "Generating complex medical care plots..."
	Rscript cmc.R

figures/compas.pdf: compas.R
	@echo "########################################"
	@echo "Generating COMPAS plots..."
	Rscript compas.R

plots: figures/frontier_cf_eo.pdf \
			 figures/frontier_cf_fairness.pdf \
			 figures/frontier_cf_pred_par.pdf \
			 figures/frontier_principal_fairness.pdf \
			 figures/calibration_risk_dist.pdf \
			 figures/density.pdf \
			 figures/diabetes.pdf \
			 figures/diabetes_calibration.pdf \
			 figures/calibration.pdf \
			 figures/label_bias.pdf \
			 figures/pareto_full.pdf \
			 figures/pareto_sub.pdf \
			 figures/treatment_effects.pdf \
			 figures/compas.pdf

clean:
	@echo "Cleaning up"
	@rm -f figures/*.pdf
	@rm -f data/*.csv
