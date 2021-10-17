:set -XOverloadedStrings
:set -XFlexibleContexts
:set prompt ""

import Sound.Tidal.Context
import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})

:{
let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    first = streamFirst tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setcps = asap . cps
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1
    d2 = p 2
    d3 = p 3
    d4 = p 4
    d5 = p 5
    d6 = p 6
    d7 = p 7
    d8 = p 8
    d9 = p 9
    d10 = p 10
    d11 = p 11
    d12 = p 12
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
:}

hush = mapM_ ($ silence) [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16]

:{
let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetR tidal
    setB = streamSetB tidal
:}

din = s "din"
ch n = (din #midichan (n-1))
ccScale = (*127)
cc n val = control (ccScale val) #io n where io n = (midicmd "control" #ctlNum n)
cc' c n val = control (ccScale val) #io n c where io n c = (din #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC c n val = once $ control (val) #io n c where io n c = (din #midicmd "control" #midichan (c-1) #ctlNum (n))
setCC' c n val = control (val) #io n c where io n c = (din #midicmd "control" #midichan (c-1) #ctlNum (n))
midiScale n = 0.9 + n*0.03
lfo wave lo hi = segment 64 $ range lo hi wave
lfo' wave lo hi = segment 64 $ rangex (s lo) (s hi) $ wave where s n | n > 0 = n | n <= 0 = 0.001
ped = cc 64
vel = pF "amp"
humanise n = vel $ (range (-0.09 * n) (0.09 * n) $ rand)
patToList n pat = fmap value $ queryArc pat (Arc 0 n)
pullBy = (<~)
pushBy = (~>)
(|=) = (#)
resetCycles = streamResetCycles tidal
master = 0.99
runWith f = resetCycles >> f

out = 4
bar b1 b2 p = ((b1+2)*4, (b2+3)*4, p)
phrase = bar
rh = phrase
runSeq = (0, 1, silence)
midiClock out = bar 0 out $ midicmd "midiClock*24" #din
initSync = bar 0 0 $ midicmd "stop" #din
startSync = bar 1 1 $ midicmd "start" #din
stopSync out = bar (out+1) (out+1) $ midicmd "stop" #din
inKey k b p = note (slow b $ k p)
sync out = [midiClock out, initSync, startSync, stopSync out]
bpm t = cps (t/60)
meter t m s = cps (t/((m/s)*60))

n \\\ s = toScale $ fromIntegral . (+ i n) . toInteger <$> s

hemidemisemiquaver = 1/64
demisemiquaver = 1/32
semiquaver = 1/16
quaver = 1/8
crotchet = 1/4
minim = 1/2

:{

mMaj :: Num a => [a]
mMaj = [0,2,4,5,7,9,11]

mMin :: Num a => [a]
mMin = [0,2,3,5,7,9,11]

hMin :: Num a => [a]
hMaj = [0,2,3,5,7,8,11]

hMaj :: Num a => [a]
hMin = [0,2,4,5,7,8,11]

mode m key pat = key (pat |+ m)
transpose st key pat = (key pat) |+ st

prog p = midicmd "program" #progNum p

cc'' n val = control val #io n where io n = (midicmd "control" #ctlNum n)

user = stack[cc'' 0 80, 0.01 ~> cc'' 32 0]
presetA = stack[cc'' 0 81, 0.01 ~> cc'' 32 0]
presetB = stack[cc'' 0 81, 0.01 ~> cc'' 32 1]
presetC = stack[cc'' 0 81, 0.01 ~> cc'' 32 2]
presetD = stack[cc'' 0 81, 0.01 ~> cc'' 32 3]
presetE = stack[cc'' 0 81, 0.01 ~> cc'' 32 4]
sessionA = stack[cc'' 0 84, 0.01 ~> cc'' 32 0]
sessionB = stack[cc'' 0 84, 0.01 ~> cc'' 32 1]
orchestralA = stack[cc'' 0 84, 0.01 ~> cc'' 32 2]
orchestralB = stack[cc'' 0 84, 0.01 ~> cc'' 32 3]

jvProgram b p = stack[silence
  ,b
  ,0.02 ~> prog (p-1)
  ]

program prog bank = stack [
  midicmd "program" #progNum prog,
  pushBy 0.001 $ control bank #midicmd "control" #ctlNum 0
  ]

:}

:set prompt "tidal> "
:set prompt-cont ""
