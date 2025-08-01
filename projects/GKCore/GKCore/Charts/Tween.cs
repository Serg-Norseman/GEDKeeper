/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using BSLib;

namespace GKCore.Charts
{
    public enum TweenAnimation
    {
        Linear,
        EaseInQuad, EaseOutQuad, EaseInOutQuad,
        EaseInCubic, EaseOutCubic, EaseInOutCubic,
        EaseInQuart, EaseInExpo, EaseOutExpo
    }

    public delegate void TweenDelegate(int newX, int newY);

    ///<summary>
    /// This class is implemented so that from any other class it can be called,
    /// and by passing the correct parameters from any object, it will animate
    /// the object passed.
    ///</summary>
    public sealed class TweenLibrary : BaseObject
    {
        private readonly ITimer fTimer;

        private TweenAnimation fAnimation;
        private bool fBusy;
        private int fCounter;
        private int fCurX, fCurY;
        private int fDestX, fDestY;
        private float fStartX, fStartY;
        private int fTimeStart;
        private int fTimeDest;
        private TweenDelegate fTweenDelegate;

        public bool Busy
        {
            get { return fBusy; }
        }

        public TweenLibrary()
        {
            fBusy = false;
            fCounter = 0;
            fStartX = 0.0f;
            fStartY = 0.0f;
            fTimer = AppHost.Instance.CreateTimer(10.0d, timer_Tick);

            StopTween();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fTimer != null) fTimer.Dispose();
            }
            base.Dispose(disposing);
        }

        ///<summary>
        /// This method kicks off the process.
        ///</summary>
        public void StartTween(TweenDelegate tweenDelegate, int srcX, int srcY, int destX, int destY, TweenAnimation animType, int timeInterval)
        {
            if (tweenDelegate == null) return;

            if (fBusy) StopTween();
            fBusy = true;

            fTweenDelegate = tweenDelegate;
            fCounter = 0;
            fTimeStart = fCounter;
            fTimeDest = timeInterval;
            fAnimation = animType;
            fStartX = srcX;
            fStartY = srcY;
            fCurX = srcX;
            fCurY = srcY;
            fDestX = destX;
            fDestY = destY;

            fTimer.Start();
        }

        public void StopTween()
        {
            fTimer.Stop();
            fBusy = false;
        }

        ///<summary>
        /// This is the method that gets called every tick interval.
        ///</summary>
        private void timer_Tick(object sender, EventArgs e)
        {
            if (fCurX == fDestX && fCurY == fDestY) {
                StopTween();
            } else {
                float t = fCounter - fTimeStart;
                float d = fTimeDest - fTimeStart;

                fCurX = GetFormula(t, fStartX, d, fDestX - fStartX);
                fCurY = GetFormula(t, fStartY, d, fDestY - fStartY);

                fTweenDelegate(fCurX, fCurY);
                fCounter++;
            }
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
