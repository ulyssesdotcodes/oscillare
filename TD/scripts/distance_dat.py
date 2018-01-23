# me - this DAT
# scriptOp - the OP which is cooking
#
# press 'Setup Parameters' in the OP to call this function to re-create the parameters.
def onSetupParameters(scriptOp):
	return

# called whenever custom pulse parameter is pushed
def onPulse(par):
	return

def onCook(scriptOp):
  scriptOp.clear()

  target = list(map(lambda c: float(c[0]), scriptOp.inputs[1].cols()))
  maxx = max(map(lambda v: v[0], scriptOp.inputs[0].rows()))
  maxy = max(map(lambda v: v[1], scriptOp.inputs[0].rows()))
  minx = min(map(lambda v: v[0], scriptOp.inputs[0].rows()))
  miny = min(map(lambda v: v[1], scriptOp.inputs[0].rows()))
  valx = float(target[0]) * (maxx - minx) + minx
  valy = float(target[1]) * (maxy - miny) + miny

  for row in scriptOp.inputs[0].rows():
    scriptOp.appendRow([(float(row[0]) - valx)**2 + (float(row[1]) - valy)**2])

  return
