#!/usr/bin/env bash
#find ./* -size +100M | cat >> .gitignore
git pull
git add .
git commit -m ${1:-"Luchao's default auto-commitment using gitpull.sh"}
git push

#/bin/bash