/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.ComponentModel;
using System.Windows.Forms;

namespace GKCommon
{
    public enum TweenAnimation {
        Linear,
        EaseInQuad, EaseOutQuad, EaseInOutQuad,
        EaseInCubic, EaseOutCubic, EaseInOutCubic,
        EaseInQuart, EaseInExpo, EaseOutExpo
    }

    public delegate void TweenDelegate(int newX, int newY);

    ///<summary>
    /// This class is implemented so that from any other class it can be called, and by passing the correct parameters
    /// from any object, it will animate the object passed. Parameters required are:
    /// (sender[do not modify], timer1[do not modify and must exist], X Destination, Y Destination, Animation Type, Duration) or as noted below.
    /// From the remote class add this to the object's eventHandler:
    /// TweenLibrary.startTweenEvent(sender, timer1, 300, randint(), "easeinoutcubic", 20); :-mm
    ///</summary>
    public sealed class TweenLibrary : BaseObject
    {
        private bool busy;
        private int counter;
        private int timeStart;
        private int timeDest;
        private int fDestX, fDestY;
        private int curX, curY;

        private TweenAnimation fAnimation;
        private IContainer fComponents;
        private readonly float[] fStartPos;
        private Timer fTimer;
        private TweenDelegate fTweenDelegate;

        public TweenLibrary()
        {
            this.busy = false;
            this.counter = 0;
            this.fStartPos = new float[] { 0, 0 };
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (fTimer != null) fTimer.Dispose();
                if (fComponents != null) fComponents.Dispose();
            }
            base.Dispose(disposing);
        }

        ///<summary>
        /// This method kicks off the process.
        ///</summary>
        public void StartTween(TweenDelegate tweenDelegate, int srcX, int srcY, int destX, int destY, TweenAnimation animType, int timeInterval)
        {
            if (busy) return;
            busy = true;

            this.fTweenDelegate = tweenDelegate;
            this.counter = 0;
            this.timeStart = counter;
            this.timeDest = timeInterval;
            this.fAnimation = animType;
            this.fStartPos[0] = srcX;
            this.fStartPos[1] = srcY;
            this.curX = srcX;
            this.curY = srcY;
            this.fDestX = destX;
            this.fDestY = destY;

            this.fComponents = new Container();
            this.fTimer = new Timer(this.fComponents);
            this.fTimer.Interval = 1;
            this.fTimer.Tick += this.timer_Tick;
            this.fTimer.Stop();
            this.fTimer.Start();
        }

        ///<summary>
        /// This is the method that gets called every tick interval.
        ///</summary>
        private void timer_Tick(object sender, EventArgs e)
        {
            if (curX == fDestX && curY == fDestY) {
                this.fTimer.Stop();
                this.fTimer.Enabled = false;
                
                busy = false;
            } else {
                curX = this.Tween(0);
                curY = this.Tween(1);

                if (this.fTweenDelegate != null) this.fTweenDelegate(curX, curY);
                
                counter++;
            }
        }

        ///<summary>
        /// This method returns a value from the tween formula.
        ///</summary>
        private int Tween(int prop)
        {
            float c;
            if (prop == 0) {
                c = fDestX - fStartPos[prop];
            } else {
                c = fDestY - fStartPos[prop];
            }

            float t = (float)counter - timeStart;
            float d = (float)timeDest - timeStart;

            return this.GetFormula(t, fStartPos[prop], d, c);
        }

        ///<summary>
        /// This method selects which formula to pick and then returns a number for the tween position of the pictureBox.
        ///</summary>
        private int GetFormula(float t, float b, float d, float c)
        {
            // adjust formula to selected algoritm from combobox
            switch (fAnimation) {
                case TweenAnimation.Linear:
                    // simple linear tweening - no easing
                    return (int)(c*t/d+b);

                case TweenAnimation.EaseInQuad:
                    // quadratic (t^2) easing in - accelerating from zero velocity
                    return (int)(c*(t/=d)*t + b);
                    
                case TweenAnimation.EaseOutQuad:
                    // quadratic (t^2) easing out - decelerating to zero velocity
                    return (int)(-c*(t=t/d)*(t-2)+b);
                    
                case TweenAnimation.EaseInOutQuad:
                    // quadratic easing in/out - acceleration until halfway, then deceleration
                    if ((t/=d/2)<1) return (int)(c/2*t*t+b);
                    return (int)(-c/2*((--t)*(t-2)-1)+b);

                case TweenAnimation.EaseInCubic:
                    // cubic easing in - accelerating from zero velocity
                    return (int)(c*(t/=d)*t*t + b);

                case TweenAnimation.EaseOutCubic:
                    // cubic easing in - accelerating from zero velocity
                    return (int)(c*((t=t/d-1)*t*t + 1) + b);
                    
                case TweenAnimation.EaseInOutCubic:
                    // cubic easing in - accelerating from zero velocity
                    if ((t/=d/2) < 1) return (int)(c/2*t*t*t+b);
                    return (int)(c/2*((t-=2)*t*t + 2)+b);

                case TweenAnimation.EaseInQuart:
                    // quartic easing in - accelerating from zero velocity
                    return (int)(c*(t/=d)*t*t*t + b);

                case TweenAnimation.EaseInExpo:
                    // exponential (2^t) easing in - accelerating from zero velocity
                    if (t==0) return (int)b;
                    return (int)(c*Math.Pow(2,(10*(t/d-1)))+b);

                case TweenAnimation.EaseOutExpo:
                    // exponential (2^t) easing out - decelerating to zero velocity
                    if (t==d) return (int)(b+c);
                    return (int)(c * (-Math.Pow(2,-10*t/d)+1)+b);

                default:
                    return 0;
            }
        }
    }
}
