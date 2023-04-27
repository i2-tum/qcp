#!/usr/bin/env bash

OLDENV=$(pyenv virtualenvs | grep "^ *\*" | sed -E "s/^ *\* *([^ ]+) +\(.*\)$/\1/")
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
pyenv activate env3.7
PYTHONPATH=lib/rpo python lib/rpo_opt.py
#pyenv activate "$OLDENV"
