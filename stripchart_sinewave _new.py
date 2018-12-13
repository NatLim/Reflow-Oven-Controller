import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import matplotlib.axes as ax
import sys, time, math
import serial

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

    plt.title('Reflow Soldering Profile')
    plt.ylabel('Temperature')
    plt.xlabel('Time')
    plt.legend()
    
    ax.set_axis_bgcolor('black')
    ax.set_axis_bgcolor((0,1,0))#change the backgroudnof the graph
    
    

    
    t = data_gen.t
    while True:
       t+=1
       val = ser.readline()
       
      
                    
                       
       yield t, val
    print(val)


def run(data):
    # update the data
    t,y = data
    if t>-1:
        xdata.append(t)
        ydata.append(y)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(t-xsize, t)
        line.set_data(xdata, ydata)

    return line,

def on_close_figure(event):
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], lw=2)
ax.set_ylim(0, 250)
ax.set_xlim(0, xsize)
ax.grid()
xdata, ydata = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
plt.show()
