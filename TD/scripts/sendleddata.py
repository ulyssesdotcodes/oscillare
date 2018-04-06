dirty = False
buff = [0] * (150 * 3 + 1)
framecount = 0
arduinoop = None
leddataop = None

def onFrameUpdate(frame):
    global framecount
    global arduinoop
    global leddataop

    if arduinoop == None:
        arduinoop = me.fetch('arduino')[1:]

    if leddataop == None:
        leddataop = me.fetch('leddata')[1:]

    framecount += 1
    if framecount % 4  == 0:
        # blist = list(map(lambda v: int(v * 254).to_bytes(1, byteorder='big')[0], op(leddataop)[0].vals))
        # blist.insert(0, 0xff)
        # bs = bytes(blist)
        bs = bytes(map(lambda v: int(v * 254).to_bytes(1, byteorder='big')[0], op(leddataop)[0].vals[:450]))
        # print(len(bs))
        op(arduinoop).sendBytes(0xff)
        op(arduinoop).sendBytes(bs)
    return