# Quantified Learner Dynamics

Using quantified learner dynamics to preserve the integrity of learning and knowledge assessment in adaptive retrieval practice.

<img src="flowchart.png" alt="Flowchart" width="400"/>

## Notebooks

- [Fitting the XGBoost model](output/02_fit_model.md)
- [Evaluating the XGBoost model](output/03_evaluate_model.md)

## Usage

Generate keystroke and learning performance features from the response data:
```bash
make features
```

Fit the XGBoost model for each learner:
```bash
make fit
```

Evaluate the performance of fitted models:
```bash
make evaluate
```

Do all of the above:
```bash
make all
```
