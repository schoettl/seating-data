Seating data
============

This repository contains data on people's seating behavior in Munich's S-Bahn
(trains of type ET423).

It also contains code for data processing and analysis.

Simulated data
--------------

A second `LOG_EVENT.csv` file is generated by the Vadere simulator.
This file be copied to `data/simulated/` with the following command:

```
./merge-and-save-simulation-output.sh ~/vadere/PersSchoettl/vadere_project/output/train_scenario_2016-09-19_13-48-16.374/
```

Tests
-----

There are a lot of [testthat](https://github.com/hadley/testthat) unit tests.
To run the tests, you can use this command:

```r
library(testthat)
auto_test(code_path=".", test_path="tests")
```
The working directory must be the `scripts/` folder!

You can also run the test from a shell:

```shell
echo "library(testthat); auto_test(code_path=\".\", test_path=\"tests\")" | R --vanilla
```

Before changing any code, you should start the auto test which re-runs
all tests automatically whenever a file changes.

License
-------

PDDL (data) and MIT (code).

See [LICENSE](LICENSE) for more information.
