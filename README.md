# Quantified Learner Dynamics

Using quantified learner dynamics to preserve the integrity of learning and knowledge assessment in adaptive retrieval practice.

<img src="flowchart.png" alt="Flowchart" width="400"/>

## Paper

This repository accompanies a paper:

van der Velde, M., Krambeer, M., & van Rijn, H. (2025). Preserving the integrity of study behaviour in online retrieval practice using quantified learner dynamics. Proceedings of the 18th International Conference on Educational Data Mining, 680--687. [https://doi.org/10.5281/zenodo.15870147](https://doi.org/10.5281/zenodo.15870147)

Please refer to the paper for a detailed description of the methods and results.


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

## Funding

This project is co-financed by the National Education Lab AI.

<a href="https://www.ru.nl/nolai"><img src="nolai.webp" alt="NOLAI logo" width="100"/></a>
