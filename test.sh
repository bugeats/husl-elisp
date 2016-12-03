#!/bin/bash

emacs -batch -l ert -l husl.test.el -f ert-run-tests-batch-and-exit
