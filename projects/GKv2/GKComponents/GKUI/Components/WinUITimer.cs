/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using System.Timers;
using BSLib;
using GKCore.Interfaces;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class WinUITimer : BaseObject, ITimer
    {
        private readonly Timer fInnerTimer;
        private readonly EventHandler fElapsedHandler;

        public bool Enabled
        {
            get { return fInnerTimer.Enabled; }
            set {
                if (value) {
                    fInnerTimer.Start();
                } else {
                    fInnerTimer.Stop();
                }
            }
        }

        /// <summary>
        /// Gets or sets the interval, in milliseconds.
        /// </summary>
        public double Interval
        {
            get { return fInnerTimer.Interval; }
            set { fInnerTimer.Interval = value; }
        }

        public WinUITimer(double msInterval, EventHandler elapsedHandler)
        {
            fElapsedHandler = elapsedHandler;

            fInnerTimer = new Timer();
            fInnerTimer.Interval = msInterval;
            fInnerTimer.Elapsed += ElapsedEventHandler;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fInnerTimer.Dispose();
            }
            base.Dispose(disposing);
        }

        private void ElapsedEventHandler(object sender, ElapsedEventArgs e)
        {
            fElapsedHandler(sender, e);
        }

        public void Start()
        {
            fInnerTimer.Start();
        }

        public void Stop()
        {
            fInnerTimer.Stop();
        }
    }
}
