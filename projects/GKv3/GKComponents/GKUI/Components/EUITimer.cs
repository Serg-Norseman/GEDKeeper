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
using BSLib;
using Eto.Forms;
using GKCore.Interfaces;

namespace GKUI.Components
{
    /// <summary>
    /// Eto-specific UI timer.
    /// </summary>
    public class EUITimer : BaseObject, ITimer
    {
        private readonly UITimer fInnerTimer;
        private readonly EventHandler fElapsedHandler;

        public bool Enabled
        {
            get { return fInnerTimer.Started; }
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
            get { return fInnerTimer.Interval * 1000; }
            set { fInnerTimer.Interval = value / 1000; }
        }

        public EUITimer(double msInterval, EventHandler elapsedHandler)
        {
            fElapsedHandler = elapsedHandler;

            fInnerTimer = new UITimer();
            fInnerTimer.Interval = msInterval / 1000;
            fInnerTimer.Elapsed += ElapsedEventHandler;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fInnerTimer.Dispose();
            }
            base.Dispose(disposing);
        }

        private void ElapsedEventHandler(object sender, EventArgs e)
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
