import scripts
import json

def onReceive(dat, rowIndex, message, bytes, peer):
    res = json.loads(json.loads(message))
    print(res)
    if res['tag'] != "Err":
        scripts.apply(json.loads(json.loads(res['contents'])))
    return