using System;

namespace Ext.Tween
{
	public delegate void TweenDelegate(int newX, int newY);
	
	///<summary>
	///This class is implemented so that from any other class it can be called, and by passing the correct parameters
	///from any object, it will animate the object passed. Parameters required are:
	///(sender[do not modify], timer1[do not modify and must exist], X Destination, Y Destination, Animation Type, Duration) or as noted below.
	///From the remote class add this to the object's eventHandler:
	///TweenLibrary.startTweenEvent(sender, timer1, 300, randint(), "easeinoutcubic", 20); :-mm
	///</summary>
	public class TweenLibrary
	{
		private int counter = 0;
		private int timeStart;
		private int timeDest;
		private string animType;
		
		private float t, d, b, c;

		private int[] Arr_startPos = new int[]{0,0};

		private int destX, destY;
		private int curX, curY;
		
		private System.ComponentModel.IContainer components;
		private System.Windows.Forms.Timer _timer;
		private TweenDelegate _tweenDelegate;
		
		///<summary>
		///this method kicks off the process
		///</summary>
		public void startTweenEvent(TweenDelegate tweenDelegate, int srcX, int srcY, int destX, int destY, string _animType, int _timeInterval)
		{
			//inits the parameters for the tween process
			this._tweenDelegate = tweenDelegate;
			this.counter = 0;
			this.timeStart = counter;
			this.timeDest = _timeInterval;
			this.animType = _animType;
			this.Arr_startPos[0] = srcX;
			this.Arr_startPos[1] = srcY;
			this.destX = destX;
			this.destY = destY;

			this.components  = new System.ComponentModel.Container();
			this._timer  = new System.Windows.Forms.Timer(this.components);
			this._timer.Interval = 1;
			this._timer.Tick += new System.EventHandler(this.timer_Tick);
			
			//resets the timer and finally starts it
			_timer.Stop();
			_timer.Enabled = false;
			_timer.Enabled = true;
		}

		///<summary>
		///This is the method that gets called every tick interval
		///</summary>
		public void timer_Tick(object sender, System.EventArgs e)
		{
			if (curX == destX && curY == destY) {
				_timer.Stop();
				_timer.Enabled = false;
			} else {
				curX = tween(0);
				curY = tween(1);
				
				if (_tweenDelegate != null) _tweenDelegate(curX, curY);
				
				counter++;
			}
		}

		///<summary>
		///This method returns a value from the tween formula.
		///</summary>
		private int tween(int prop)
		{
			t = (float)counter - timeStart;
			b = (float)Arr_startPos[prop];
			
			if (prop == 0) {
				c = (float)destX - Arr_startPos[prop];
			} else {
				c = (float)destY - Arr_startPos[prop];
			}
			
			d = (float)timeDest - timeStart;

			return getFormula(animType, t, b, d, c);
		}

		///<summary>
		///this method selects which formula to pick and then returns a number for the tween position of the pictureBox
		///</summary>
		private int getFormula(string animType, float t, float b, float d, float c)
		{
			//adjust formula to selected algoritm from combobox
			switch (animType) {
				case "linear":
					// simple linear tweening - no easing
					return (int)(c*t/d+b);

				case "easeinquad":
					// quadratic (t^2) easing in - accelerating from zero velocity
					return (int)(c*(t/=d)*t + b);
					
				case "easeoutquad":
					// quadratic (t^2) easing out - decelerating to zero velocity
					return (int)(-c*(t=t/d)*(t-2)+b);
					
				case "easeinoutquad":
					// quadratic easing in/out - acceleration until halfway, then deceleration
					if ((t/=d/2)<1) return (int)(c/2*t*t+b); else return (int)(-c/2*((--t)*(t-2)-1)+b);
					
				case "easeincubic":
					// cubic easing in - accelerating from zero velocity
					return (int)(c*(t/=d)*t*t + b);

				case "easeoutcubic":
					// cubic easing in - accelerating from zero velocity
					return (int)(c*((t=t/d-1)*t*t + 1) + b);
					
				case "easeinoutcubic":
					// cubic easing in - accelerating from zero velocity
					if ((t/=d/2) < 1)return (int)(c/2*t*t*t+b);else return (int)(c/2*((t-=2)*t*t + 2)+b);

				case "easeinquart":
					// quartic easing in - accelerating from zero velocity
					return (int)(c*(t/=d)*t*t*t + b);

				case "easeinexpo":
					// exponential (2^t) easing in - accelerating from zero velocity
					if (t==0) return (int)b; else return (int)(c*Math.Pow(2,(10*(t/d-1)))+b);
					
				case "easeoutexpo":
					// exponential (2^t) easing out - decelerating to zero velocity
					if (t==d) return (int)(b+c); else return (int)(c * (-Math.Pow(2,-10*t/d)+1)+b);

				default:
					return 0;
			}
		}
	}
}