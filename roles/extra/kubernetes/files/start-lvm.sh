#!/bin/bash

for i in $(lvscan | awk '{print $2}' | sed "s/'//g"); do
    lvchange -ay ${i};
done
