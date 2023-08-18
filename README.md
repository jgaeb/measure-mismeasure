# Reproduction Materials for "The Measure and Mismeasure of Fairness"

To reproduce the analyses in our paper:

1. Make sure you have `R` version 4.3.1 and the `groundhog` package installed.
2. Create and activate a python3 virtual environment in the root of the
   repository as follows:
```bash
$ python3 -m venv venv
$ source venv/bin/activate
(venv) $ pip3 install -r requirements.txt
```
3. Ensure you have `make` installed and run the following command from the root
   directory:
```bash
make plots
```

This will perform all of the analyses in the paper and regenerate the plots in
the `figures` directory. The analysis takes around 3 hours on a single M1 core,
~80% of which is generating the counterfactual predictive parity geometry of
fairness plot.
