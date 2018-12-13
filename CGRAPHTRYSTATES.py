import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import matplotlib.axes as ax
import sys, time, math
import serial
#import matplotlib.pyplot.axvline


xsize=100

ser = serial.Serial(
 port='COM11',
 baudrate=115200,
 parity=serial.PARITY_NONE,
 stopbits=serial.STOPBITS_TWO,
 bytesize=serial.EIGHTBITS
)
ser.isOpen()   

   
def data_gen():

    labelt = plt.title('Reflow Soldering Profile', fontsize=40)
    labelt.set_color("red")
    
    labely = plt.ylabel('Temperature', fontsize=30)
    labely.set_color("red")
    
    labelx = plt.xlabel('Time', fontsize=30)
    labelx.set_color("red")
    
    plt.legend()
    
    ax.set_axis_bgcolor('black')
    ax.set_axis_bgcolor((0,0,0))#change the backgroudnof the graph
    
    t2=0
    t3=0
    t4=0
    tnew=0
    check=0
    check1=0
    check3=0
    check4=0
    
    t = data_gen.t
    while True:
       t+=1
       val = float(ser.readline())
       yield t,val
       if check3==0:
           if val <= 150: #state1
               line.set_color("yellow")
               check3=1
       else:
           if val > 150:
               if check == 0:
                   t2 = t+60
                   check=1
               if t < t2:#state2
                   line.set_color("blue")
               else:
                   if t > t2:
                       if val <=220:#state3
                           if check4==0:
                               line.set_color("green")
                               check4=1
                       else:
                           if val > 220:
                               if check1 == 0:
                                   t3 = t +45
                                   check1=1
                               if t < t3:#state4
                                   line.set_color("white")
                               else:
                                   if t>t3:
                                       if val >=60:#state5
                                           line.set_color("red")
                                       else:
                                           line.set_color("yellow")    
                                                 
           
    yield t, val

def run(data):
    # update the data
    t,y = data
    if t>-1:
        xdata.append(t)
        ydata.append(y)
        #if t>xsize: # Scroll to the left.
           # ax.set_xlim(t-xsize, t)
        line.set_data(xdata, ydata)

    return line,

def on_close_figure(event):
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], linestyle='-.', lw=7, color = 'red')
ax.set_ylim(0, 250)
ax.set_xlim(0, 400)
ax.grid()
xdata, ydata = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
plt.show()

