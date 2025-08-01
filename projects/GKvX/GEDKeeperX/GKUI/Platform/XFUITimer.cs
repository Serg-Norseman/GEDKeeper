/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using Xamarin.Forms;

namespace GKUI.Platform
{
    /// <summary>
    /// Xamarin-specific UI timer.
    /// </summary>
    public sealed class XFUITimer : BaseObject, ITimer
    {
        private readonly EventHandler fElapsedHandler;
        private bool fEnabled;
        private double fInterval;

        public bool Enabled
        {
            get { return fEnabled; }
            set {
                if (value) {
                    Start();
                } else {
                    Stop();
                }
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
            fInterval = msInterval;
            fElapsedHandler = elapsedHandler;
            fEnabled = false;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                Stop();
            }
            base.Dispose(disposing);
        }

        private bool ElapsedEventHandler()
        {
            fElapsedHandler(this, new EventArgs());
            return fEnabled;
        }

        public void Start()
        {
            fEnabled = true;
            Device.StartTimer(TimeSpan.FromMilliseconds(fInterval), ElapsedEventHandler);
        }

        public void Stop()
        {
            fEnabled = false;
        }
    }
}
