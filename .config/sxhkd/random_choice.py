#!/usr/bin/python3

import sys
import random


if __name__== "__main__":
    sys.argv.pop(0)
    if sys.argv:
        print(random.choice(sys.argv))
    else:
        raise Exception("No args given")
