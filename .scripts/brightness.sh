#!bin/bash

case $1 in
    up)
        light -A 10
    ;;
    down)
        light -U 10
    ;;
esac
