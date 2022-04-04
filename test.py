# program to output the content of 'compress.hf' as binary

with open('compress.hf', 'rb') as file:
  while True:
    try:
      byte = file.read(1)
      print('{0:08b}'.format(ord(byte)))
    except:
      break
  file.close()
