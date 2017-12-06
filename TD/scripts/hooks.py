import time

def newJson(json):
  print(op('container1/record')[0])
  if op('container1/record')[0] == 1:
    jsonRecord = op('json_record')
    jsonRecord[0,0] = int(time.time())
    jsonRecord[0,1] = json
    op('json_record_out').par.write.pulse()
  return
