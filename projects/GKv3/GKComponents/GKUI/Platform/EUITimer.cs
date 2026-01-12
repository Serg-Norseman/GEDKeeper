/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using Eto.Forms;
using GKCore;

namespace GKUI.Platform
{
    /// <summary>
    /// Eto-specific UI timer.
    /// </summary>
    public sealed class EUITimer : BaseObject, ITimer
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
