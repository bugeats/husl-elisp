#!/bin/bash

emacs -batch -l ert -l reference.test.el -f ert-run-tests-batch-and-exit
