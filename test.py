import sys

if len(sys.argv) < 2:
  raise Exception("Missing CLI argument")

# output the contents of 'compress.hf'
with open(sys.argv[1], 'rb') as file:
  byteCount = 0
  while True:
    try:
      byte = file.read(1)
      print('{0:08b}'.format(ord(byte)), end="\t")
      print(byte)
      byteCount += 1
    except:
      break
  file.close()
  print(f"\nByte count: {byteCount}")

with open('file.txt', 'r') as file:
  filecontent = file.read()
  arr = []
  for char in filecontent:
    if char not in arr:
      arr.append(char)
  print(arr, "\n", len(arr))