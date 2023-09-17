/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using GKCore.Interfaces;

namespace GKUI.Platform
{
    /// <summary>
    /// Xamarin-specific UI timer.
    /// </summary>
    public sealed class XFUITimer : BaseObject, ITimer
    {
        //private readonly UITimer fInnerTimer;
        private readonly EventHandler fElapsedHandler;

        private bool fEnabled;
        private double fInterval;

        public bool Enabled
        {
            get { return fEnabled; }
            set {
                if (value) {
                    //fInnerTimer.Start();
                } else {
                    //fInnerTimer.Stop();
                }
                fEnabled = value;
            }
        }

        /// <summary>
        /// Gets or sets the interval, in milliseconds.
        /// </summary>
        public double Interval
        {
            get { return fInterval; }
            set { fInterval = value; }
        }

        public XFUITimer(double msInterval, EventHandler elapsedHandler)
        {
            fElapsedHandler = elapsedHandler;

            /*fInnerTimer = new UITimer();
            fInnerTimer.Interval = msInterval / 1000;
            fInnerTimer.Elapsed += ElapsedEventHandler;*/

            //Device.StartTimer(TimeSpan interval, Func<bool> callback);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //fInnerTimer.Dispose();
            }
            base.Dispose(disposing);
        }

        private void ElapsedEventHandler(object sender, EventArgs e)
        {
            fElapsedHandler(sender, e);
        }

        public void Start()
        {
            //fInnerTimer.Start();
        }

        public void Stop()
        {
            //fInnerTimer.Stop();
        }
    }
}
