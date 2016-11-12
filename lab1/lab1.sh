#!/usr/bin/env bash

SBT_ARGS="runMain jsy.student.Lab1 $@"
sbt "$SBT_ARGS"
