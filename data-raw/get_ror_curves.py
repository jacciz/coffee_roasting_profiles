import sys
import os
import jsonpickle
import json
import string
import csv
from pandas import json_normalize
import pandas as pd
import numpy
from scipy.signal import savgol_filter
#numpy.float64
#pd.DataFrame
with open('C:/Users/jacci/Documents/DS 710/coffee_roasting_profiles/data-raw/saved/Haiti--2021-02-09-20-15-17.json') as f:
  data = json.load(f)
# Output: {'name': 'Bob', 'languages': ['English', 'Fench']}
#print(data)
filename = 'C:/Users/jacci/Documents/DS 710/coffee_roasting_profiles/data-raw/saved/Haiti--2021-02-09-20-15-17.json'

def import_games_json(filename = filename): #import raw tweets and outputs a df with selected columns
    with open(filename, 'r', encoding='utf-8') as tweet_data:
      games_tweet = [json.loads(line) for line in tweet_data.readlines()]
      game_df = json_normalize(games_tweet)
    game_df_selected = game_df[['timex', 'temp1', 'temp2']]
    return game_df_selected
# print(import_games_json()["temp2"][0])
# print(len(data["timex"]))
# game_tweets = import_games_json()
# game_tweets['timex']
# timex = game_tweets['timex']
# t1 = game_tweets['temp1']
# t2 = game_tweets['temp2']

tuple(import_games_json()["temp2"][0])



# mylist = []
# for x in range(0,len(import_games_json()["temp2"][0])):
#     mylist.append(import_games_json()["temp2"][x])
# timex = tuple(mylist)
# print (timex)
# 
# 
# mylist = []
# for x in range(0,len(data["timex"])):
#     mylist.append(data['timex'][x])
# timex = tuple(mylist)
# #print (mytuple)
# 
# mylist = []
# for x in range(0,455):
#     mylist.append(data['temp1'][x])
# t1 = tuple(mylist)
# 
# mylist = []
# for x in range(0,455):
#     mylist.append(data['temp2'][x])
# t2 = tuple(mylist)
#print(pd.DataFrame(game_tweets['temp1']))
DROPidx = len(data["timex"])
timex = tuple(import_games_json()["timex"][0])
t1 = tuple(import_games_json()["temp1"][0])
t2 = tuple(import_games_json()["temp2"][0])

    # computes the RoR over the time and temperature arrays tx and temp via polynoms of degree 1 at index i using a window of wsize
    # the window size wsize needs to be at least 2 (two succeeding readings)
def polyRoR(tx,temp,wsize,i):
    if i == 0: # we duplicate the first possible RoR value instead of returning a 0
        i = 1
    if i>0 and i<len(tx) and i<len(temp):
        with warnings.catch_warnings():
            warnings.simplefilter('ignore')
            left_index = max(0,i-wsize)
            LS_fit = numpy.polynomial.polynomial.polyfit(tx[left_index:i+1],temp[left_index:i+1], 1)
            return LS_fit[1]*60.
    else:
        return 0
def arrayRoR(tx,temp,wsize):
    res = (temp[wsize:] - temp[:-wsize]) / ((tx[wsize:] - tx[:-wsize])/60.)
        # length compensation done downstream, no necessary here!
    return res
# smoothes a list of values 'y' at taken at times indicated by the numbers in list 'x'
# 'flat', 'hanning', 'hamming', 'bartlett', 'blackman'
# 'flat' results in moving average
# window_len should be odd
# based on http://wiki.scipy.org/Cookbook/SignalSmooth
def smooth(x, y, window_len=15, window='hanning'):
    try:
        if len(x) == len(y) and len(x) > 1:
            if window_len > 2:
                # smooth curves
                #s = numpy.r_[2*x[0]-y[window_len:1:-1],y,2*y[-1]-y[-1:-window_len:-1]]
                #s=numpy.r_[y[window_len-1:0:-1],y,y[-2:-window_len-1:-1]]
                #s = y
                s=numpy.r_[y[window_len-1:0:-1],y,y[-1:-window_len:-1]]
                if window == 'flat': #moving average
                    w = numpy.ones(window_len,'d')
                else:
                    w = eval('numpy.'+window+'(window_len)')
                try:
                    ys = numpy.convolve(w/w.sum(),s,mode='valid')
                except:
                    return y
                hwl = int((window_len/2))
                res = ys[hwl:-hwl]
                if len(res)+1 == len(y) and len(res) > 0:
                    try:
                        return ys[hwl-1:-hwl]
                    except:
                        return y
                elif len(res) != len(y):
                    return y
                else:
                    return res
            else:
                return y
        else:
            return y
    except Exception as ex:
#            import traceback
#            traceback.print_exc(file=sys.stdout)
        #_, _, exc_tb = sys.exc_info()
        #adderror((QApplication.translate("Error Message","Exception:",None) + " smooth() {0}").format(str(ex)),exc_tb.tb_lineno)
        return x

def smooth_list(a, b, window_len = 7, window='hanning',decay_weights=None,decay_smoothing=False,fromIndex=-1,toIndex=0,re_sample=True,back_sample=True,a_lin=None):  # default 'hanning'
    window_len = 7
    decay_smoothing= None
    decay_weights=None
    filterDropOuts = True # Smooth Spikes
    if len(a) > 1 and len(a) == len(b) and (filterDropOuts or window_len>2):
        #pylint: disable=E1103
        # 1. truncate
        if fromIndex > -1: # if fromIndex is set, replace prefix up to fromIndex by None
            if toIndex==0: # no limit
                toIndex=len(a)
        else: # smooth list on full length
            fromIndex = 0
            toIndex = len(a)
        a = numpy.array(a,dtype='float64')[fromIndex:toIndex]
        b = numpy.array(b,dtype='float64')[fromIndex:toIndex]
        # 2. re-sample
        if re_sample:
            if a_lin is None or len(a_lin) != len(a):
                a_mod = numpy.linspace(a[0],a[-1],len(a))
            else:
                a_mod = a_lin
            b = numpy.interp(a_mod, a, b) # resample data in a to linear spaced time
        else:
            a_mod = a
        # 3. filter spikes
        if filterDropOuts:
            try:
                b = self.medfilt(numpy.array(b),5)  # k=3 seems not to catch all spikes in all cases; k must be odd!
## scipy alternative which performs equal
#                    from scipy.signal import medfilt as scipy_medfilt
#                    b = scipy_medfilt(numpy.array(b),5)
                res = b
            except:
                pass
        # 4. smooth data
        if window_len > 2:
            if decay_smoothing:
                # decay smoothing
                if decay_weights is None:
                    decay_weights = numpy.arange(1,window_len+1)
                else:
                    window_len = len(decay_weights)
                # invariant: window_len = len(decay_weights)
                if decay_weights.sum() == 0:
                    res = b
                else:
                    res = []
                    # ignore -1 readings in averaging and ensure a good ramp
                    for i in range(len(b)):
                        seq = b[max(0,i-window_len + 1):i+1]
                        # we need to surpress -1 drop out values from this
                        seq = list(filter(lambda item: item != -1,seq))
                        w = decay_weights[max(0,window_len-len(seq)):]  # preCond: len(decay_weights)=window_len and len(seq) <= window_len; postCond: len(w)=len(seq)
                        if len(w) == 0:
                            res.append(b[i]) # we don't average if there is are no weights (e.g. if the original seq did only contain -1 values and got empty)
                        else:
                            res.append(numpy.average(seq,weights=w)) # works only if len(seq) = len(w)
                    # postCond: len(res) = len(b)
            else:
                # optimal smoothing (the default)
                win_len = max(0,window_len)
                if win_len != 1: # at the lowest level we turn smoothing completely off
                    res = smooth(a_mod,b,win_len,window)
                else:
                    res = b
        # 4. sample back
        if re_sample and back_sample:
            res = numpy.interp(a, a_mod, res) # re-sampled back to orginal timestamps
        return numpy.concatenate(([None]*(fromIndex),res.tolist(),[None]*(len(a)-toIndex))).tolist()
    else:
        return b
      
# CHANGE CHARGE AND DROP ID
    # computes the RoR deltas and returns the smoothed versions for both temperature channels
    # if t1 or t2 is not given (None), its RoR signal is not computed and None is returned instead
    # timex_lin: a linear spaced version of timex
def recomputeDeltas(timex=timex,CHARGEidx = 1,DROPidx = DROPidx,t1 = t1,t2=t2,optimalSmoothing=False,timex_lin=None,deltaETsamples=None,deltaBTsamples=None):
    polyfitRoRcalc = False # RIGHT? should be false, Look at artisan.settings
    # RoR display limits - (RoR FILTER PART)
    # user configurable RoR limits (only applied if flag is True; applied before TP during recording as well as full redraw)
    RoRlimitFlag = True
    RoRlimit = 65
    RoRlimitm = 0
# system fixed RoR limits (only applied if flag is True; usually higher than the user configurable once and always applied)
    maxRoRlimit = 170
    deltaBTsamples = int(max(1,10 / 1))  #int(max(1,self.deltaBTspan / interval))
    deltaETsamples = int(max(1,1 / 1))
    deltaETfilter = 3#1
    deltaBTfilter = 21#10
    #print(len(numpy.array(timex)))
    try:
        tx_roast = numpy.array(timex)
        lt = len(tx_roast)
        if CHARGEidx > -1:
            roast_start_idx = CHARGEidx
        else:
            roast_start_idx = 0
        if DROPidx > 0:
            roast_end_idx = DROPidx
        else:
            roast_end_idx = lt
        if deltaBTsamples is None:
            dsBT = max(2,(deltaBTsamples)) # now as in sample()
        else:
            dsBT = deltaBTsamples
        if deltaETsamples is None:
            dsET = max(2,(deltaETsamples)) # now as in sample()
        else:
            dsET = deltaETsamples
        if timex_lin is not None:
            if len(timex_lin) == len(timex):
                timex_lin = numpy.array(timex_lin)
            else:
                timex_lin = None
        if t1 is not None:
            with numpy.errstate(divide='ignore'):
                nt1 = numpy.array([0 if x is None else x for x in t1]) # ERROR None Type object not scriptable! t==None on ON
                if optimalSmoothing and polyfitRoRcalc:
                    # optimal RoR computation using polynoms with out timeshift
                    if dsET % 2 == 0:
                        dsETs = dsET+1 # the savgol_filter expectes odd window length
                    else:
                        dsETs = dsET
                    if len(nt1) > dsETs:
                        try:
                            # nt1 is not linearized yet:
                            if timex_lin is None or len(timex_lin) != len(nt1):
                                lin1 = numpy.linspace(timex[0],timex[-1],len(timex))
                            else:
                                lin1 = timex_lin
                            if lin1 is None:
                                nt1_lin = timex # we just run on the non-linear timex in this case
                            else:
                                nt1_lin = numpy.interp(lin1, tx_roast, nt1) # resample data in nt1 to linear spaced time
                            dist = (lin1[-1] - lin1[0]) / (len(lin1) - 1)
                            z1 = savgol_filter(nt1_lin, dsETs, 1, deriv=1,delta=dsET)
                            z1 = z1 * (60./dist) * dsETs
                        except:
                            # a numpy/OpenBLAS polyfit bug can cause polyfit to throw an execption "SVD did not converge in Linear Least Squares" on Windows Windows 10 update 2004
                            # https://github.com/numpy/numpy/issues/16744
                            # original version just picking the corner values:
                            z1 =  arrayRoR(tx_roast,nt1,dsET)
                    else:
                        # in this case we use the standard algo
                        try:
                            # variant using incremental polyfit RoR computation
                            z1 = [ polyRoR(tx_roast,nt1,dsET,i) for i in range(len(nt1))]
                        except:
                            # a numpy/OpenBLAS polyfit bug can cause polyfit to throw an execption "SVD did not converge in Linear Least Squares" on Windows Windows 10 update 2004
                            # https://github.com/numpy/numpy/issues/16744
                            # original version just picking the corner values:
                            z1 =  arrayRoR(tx_roast,nt1,dsET)
                else:
                    if polyfitRoRcalc:
                        try:
                            # variant using incremental polyfit RoR computation
                            z1 = [polyRoR(tx_roast,nt1,dsET,i) for i in range(len(nt1))]
                        except:
                            # a numpy/OpenBLAS polyfit bug can cause polyfit to throw an execption "SVD did not converge in Linear Least Squares" on Windows Windows 10 update 2004
                            # https://github.com/numpy/numpy/issues/16744
                            # original version just picking the corner values:
                            z1 =  arrayRoR(tx_roast,nt1,dsET)
                    else:
                        z1 =  arrayRoR(tx_roast,nt1,dsET)
            ld1 = len(z1)
            # make lists equal in length
            if lt > ld1:
                z1 = numpy.append([z1[0] if ld1 else 0.]*(lt - ld1),z1)
            # apply smybolic formula
            #if DeltaETfunction is not None and len(DeltaETfunction):
            #    z1 =  apply_symbolic_delta_formula(DeltaETfunction,z1,timex,RTsname="R1")
            # apply smoothing
            if optimalSmoothing:
                user_filter = deltaETfilter
            else:
                user_filter = int(round( deltaETfilter/2.))
            delta1 =  smooth_list(tx_roast,z1,window_len=user_filter,decay_smoothing=(not optimalSmoothing),a_lin=timex_lin)
            delta1 = delta1[roast_start_idx:roast_end_idx]

            # add None for parts before and after CHARGE/DROP
            delta1 = numpy.concatenate(([None]*(roast_start_idx),delta1,[None]*(lt-roast_end_idx))) # ERROR: all input arrays must have the same number of dimensions
            # filter out values beyond the delta limits to cut out the part after DROP and before CHARGE

            if RoRlimitFlag:
                # remove values beyond the RoRlimit
                delta1 = [d if d is not None and (max(-maxRoRlimit,RoRlimitm) < d < min(maxRoRlimit,RoRlimit)) else None for d in delta1]
            if isinstance(delta1, (numpy.ndarray, numpy.generic)):
                delta1 = delta1.tolist()
        else:
            delta1 = None
        if t2 is not None:
            with numpy.errstate(divide='ignore'):
                nt2 = numpy.array([0 if x is None else x for x in t2])
                if optimalSmoothing and polyfitRoRcalc:
                    # optimal RoR computation using polynoms with out timeshift
                    if dsBT % 2 == 0:
                        dsBTs = dsBT+1 # the savgol_filter expectes odd window length
                    else:
                        dsBTs = dsBT
                    if len(nt2) > dsBTs:
                        try:
                            # nt2 is not linearized yet:
                            if timex_lin is None or len(timex_lin) != len(nt2):
                                lin2 = numpy.linspace(timex[0],timex[-1],len(timex))
                            else:
                                lin2 = timex_lin
                            if lin2 is None:
                                nt2_lin = timex # we just run on the non-linear timex in this case
                            else:
                                nt2_lin = numpy.interp(lin2, tx_roast, nt2) # resample data in nt2 to linear spaced time
                            dist = (lin2[-1] - lin2[0]) / (len(lin2) - 1)
                            z2 = savgol_filter(nt2_lin, dsBTs, 1, deriv=1,delta=dsBTs)
                            z2 = z2 * (60./dist) * dsBTs
                            print(z2)
                        except:
                            # a numpy/OpenBLAS polyfit bug can cause polyfit to throw an execption "SVD did not converge in Linear Least Squares" on Windows Windows 10 update 2004
                            # https://github.com/numpy/numpy/issues/16744
                            # original version just picking the corner values
                            z2 =  arrayRoR(tx_roast,nt2,dsBT)
                    else:
                        # in this case we use the standard algo
                        try:
                            z2 = [polyRoR(tx_roast,nt2,dsBT,i) for i in range(len(nt2))]
                        except:
                            # a numpy/OpenBLAS polyfit bug can cause polyfit to throw an execption "SVD did not converge in Linear Least Squares" on Windows Windows 10 update 2004
                            # https://github.com/numpy/numpy/issues/16744
                            # original version just picking the corner values
                            z2 =  arrayRoR(tx_roast,nt2,dsBT)
                else:
                    if polyfitRoRcalc:
                        try:
                            # variant using incremental polyfit RoR computation
                            z2 = [ polyRoR(tx_roast,nt2,dsBT,i) for i in range(len(nt2))]
                        except:
                            # a numpy/OpenBLAS polyfit bug can cause polyfit to throw an execption "SVD did not converge in Linear Least Squares" on Windows Windows 10 update 2004
                            # https://github.com/numpy/numpy/issues/16744
                            # original version just picking the corner values
                            z2 =  arrayRoR(tx_roast,nt2,dsBT)
                    else:
                        z2 =  arrayRoR(tx_roast,nt2,dsBT)

            ld2 = len(z2)
            # make lists equal in length
            if lt > ld2:
                z2 = numpy.append([z2[0] if ld2 else 0.]*(lt - ld2),z2)
            # apply smybolic formula
            #if aw.qmc.DeltaBTfunction is not None and len(aw.qmc.DeltaBTfunction):
            #    z2 =  apply_symbolic_delta_formula(aw.qmc.DeltaBTfunction,z2,timex,RTsname="R2")
            # apply smoothing
            if optimalSmoothing:
                user_filter =  deltaBTfilter
            else:
                user_filter = int(round( deltaBTfilter/2.))
            delta2 =  smooth_list(tx_roast,z2,window_len=user_filter,decay_smoothing=(not optimalSmoothing),a_lin=timex_lin)
            delta2 = delta2[roast_start_idx:roast_end_idx]
            # add None for parts before and after CHARGE/DROP
            delta2 = numpy.concatenate(([None]*(roast_start_idx),delta2,[None]*(lt-roast_end_idx)))
            # filter out values beyond the delta limits to cut out the part after DROP and before CHARGE
            if RoRlimitFlag:
                # remove values beyond the RoRlimit
                delta2 = [d if d is not None and (max(-maxRoRlimit,RoRlimitm) < d < min(maxRoRlimit,RoRlimit)) else None for d in delta2]
            if isinstance(delta2, (numpy.ndarray, numpy.generic)):
                delta2 = delta2.tolist()
        else:
            delta2 = None

        return delta1, delta2
    except Exception as e:
#            import traceback
#            traceback.print_exc(file=sys.stdout)
        #_, _, exc_tb = sys.exc_info()
        #aw.qmc.adderror((QApplication.translate("Error Message","Exception:",None) + " recomputeDeltas() {0}").format(str(e)),exc_tb.tb_lineno)
        return [0]*len(timex),[0]*len(timex)

print(recomputeDeltas())
df = recomputeDeltas()

with open('C:/Users/jacci/Documents/DS 710/test_haiti.csv', 'w') as f:
    writer = csv.writer(f , lineterminator='\n')
    for tup in df:
        writer.writerow(tup)
