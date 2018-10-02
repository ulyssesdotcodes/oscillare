from dictdiffer import diff, dot_lookup
from collections import deque

from classes import classes


def getClass(opname, default):
  return classes.get(opname, default)

state = {}
diffs = []

def apply(newState):
  global state
  # Step 1: create new nodes
  prevState = state
  state = newState
  ddiff = diff(prevState, state)
  diffs.append(ddiff)

  for diffi in list(reversed(list(ddiff))):
    splits = diffi[1].split('.') if isinstance(diffi[1], str) else diffi[1]
    if diffi[1] == '':
      if diffi[0] == 'add':
        addAll(diffi[2])
      elif diffi[0] == 'remove':
        for key,value in diffi[2]:
          curop = op("/project1/lambda" + key)
          if curop != None:
            curop.destroy()
    elif splits[1] == 'connections':
      concatname = [str(x) for x in diffi[1] if isinstance(x, str)]
      diffip = diffi[1] if isinstance(diffi[1], str) or diffi[1] == '' else ".".join(concatname)
      item = dot_lookup(state, diffip, parent=True)
      curop = op(getName(splits[0]))
      if hasattr(curop, 'inputConnectors'):
        for connector in curop.inputConnectors:
          connector.disconnect()
      for i, conn in enumerate(item['connections']):
        op(getName(conn)).outputConnectors[0].connect(curop.inputConnectors[i])
    elif splits[1] == 'parameters':
      curop = op(getName(splits[0]))
      if diffi[0] == 'add':
        for k,v in diffi[2]:
          addParameter(curop, k, v)
      elif diffi[0] == 'change':
        addParameter(curop, splits[2], diffi[2][1])
      elif diffi[0] == 'remove':
        for param in diffi[2]:
          print("remove")
          print(param[0])
          if(param[0] == 'tx'):
            curop.par.tx = 0
          elif param[0] == 'ty':
            curop.par.ty = 0
          elif param[0] == 'rotate':
            curop.par.rotate = 0
          elif str.startswith(param[0], "name"):
            curop.pars(param[0])[0].val = ""
          par = curop.pars(param[0])[0]
          print("val: " + str(par.val))
          if par.val:
            print("def: " + str(par.default))
            par.val = par.default

    elif splits[1] == 'text':
      op(getName(splits[0])).text = diffi[2][1]

    elif splits[1] == 'commands' and diffi[0] == 'add':
      runCommand(getName(splits[0]), diffi[2][0][1]['command'], diffi[2][0][1]['args'])


def getName(name):
  return "/project1/lambda" + name

def addAll(state):
  connections = []
  queue = deque(state)
  i = 0
  while len(queue) > 0:
    (key, value) = queue.popleft()
    addr = getName(key)
    par = addr[:(addr.rfind('/'))]
    if op(par) != None:
      connections.extend(list(addChange(key, value)))
    else:
      queue.append((key, value))

  for conn in connections:
    if conn[0] == '' or op(getName(conn[0])) == None:
      continue
    if conn[2] == 0:
      for connector in op(conn[1]).inputConnectors:
        connector.disconnect()
    op(getName(conn[0])).outputConnectors[0].connect(op(conn[1]).inputConnectors[conn[2]])
    if op(conn[1]).type == 'feedback' and op(conn[1]).isCHOP:
      print("feedback chop11")
      op(conn[1]).par.reset.pulse(1, frames=2)


def addChange(key, value):
  addr = getName(key)

  newOp = createOp(addr, value['ty'])
  print("Adding op " + value['ty'])

  if 'parameters' in value:
    pars = value['parameters'].items()
    tox = next((x for x in pars if x[0] == 'externaltox'), None)
    if tox != None:
      pars = [x for x in pars if x[0] != 'externaltox']
      pars.insert(0, tox)

    for k,v in pars:
      addParameter(newOp, k, v)

  if 'commands' in value:
    coms = value['commands']
    for comm in coms:
      runCommand(addr, comm['command'], comm['args'])

  if 'text' in value and value['text'] != None:
    newOp.text = value['text']

  if 'connections' in value:
    return ((c, addr, i) for i,c in enumerate(value['connections']))


def createOp(addr, ty):
  clazz = getClass(ty, 'none')
  if clazz == "none":
    print("Couldn't find " + ty)
    return

  name = addr[(addr.rfind('/') + 1):]
  par = addr[:(addr.rfind('/'))]

  if op(addr) != None:
    op(addr).destroy()

  # Special case things that can't have duplicates
  if clazz[1] == 'audiodevicein' or clazz[1] == 'videodevicein' or clazz[1] == 'ndiin' or clazz[1] == 'leapmotion' or clazz[1] == 'cplusplus':
    if op(clazz[1]) == None:
      parent().create(clazz[0], clazz[1])
    if clazz[2] == "CHOP":
      selOp = selectCHOP
      selPar = 'chop'
    elif clazz[2] == "TOP":
      selOp = selectTOP
      selPar = 'top'

    op(par).create(selOp, name)
    op(addr).pars(selPar)[0].val = '/project1/' + clazz[1]
  else:
    op(par).create(clazz[0], name)

  newOp = op(addr)

  # TODO: Figure out a clean way to not special case these
  if clazz[1] == 'out' and clazz[2] == 'SOP':
    newOp.render = True
    newOp.display = True

  if clazz[1] == 'geo':
    op(addr + "/torus1").destroy()

  return newOp

def addParameter(newOp, name, value):
  pars = newOp.pars(name)
  if len(pars) == 0:
    return

  par = pars[0]
  if isfloat(value):
    if par.isMenu:
      par.menuIndex = value
    else:
      par.val = float(value)
  else:
    par.expr = value

  # Special case loading tox as soon as we know source
  if name == "externaltox":
    newOp.par.reinitnet.pulse()
  elif name == 'file' and (newOp.type == "text" or newOp.type == "table"):
    newOp.par.loadonstartpulse.pulse()

def runCommand(newOpName, command, args):
    if command == "pulse":
      newOp = op(newOpName)
      pars = newOp.pars(args[0])
      if len(pars) > 0:
        if isfloat(args[1]):
          pars[0].pulse(float(args[1]), frames=float(args[2]))
        else:
          pars[0].pulse(args[1])
    elif command == "store":
      newOp.store(args[0], args[1])

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False
