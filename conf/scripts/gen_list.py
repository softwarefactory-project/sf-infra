import sys

print(" # ".join(["./" + x.split('/', 1)[1]
                  for x in sys.argv[1:]]))
