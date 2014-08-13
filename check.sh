#!/bin/bash

echo "1" | ./test
echo "(1)" | ./test
echo "toto" | ./test
echo "True" | ./test
echo "False" | ./test

echo "1 * 2" | ./test
echo "1 / 2" | ./test

echo "1 + 1" | ./test
echo "1 - 1" | ./test

echo "True && False" | ./test
echo "True || False" | ./test
echo "fun x -> x 1" | ./test
echo "fun x -> x" | ./test
echo "let x = 1 in x" | ./test

echo "True  || True"  | ./test
echo "False || True"  | ./test
echo "True  || False" | ./test
echo "False || False" | ./test

echo "True  && True"  | ./test
echo "False && True"  | ./test
echo "True  && False" | ./test
echo "False && False" | ./test
