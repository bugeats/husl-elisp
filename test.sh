#!/bin/bash

# emacs -batch -l ert -l husl.test.el -f ert-run-tests-batch-and-exit
emacs -batch -l ert -l reference.test.el -f ert-run-tests-batch-and-exit
