# program to output the content of 'compress.hf' as binary

with open('compress.hf', 'rb') as file:
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
