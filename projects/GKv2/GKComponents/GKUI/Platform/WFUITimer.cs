/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Windows.Forms;
using BSLib;
using GKCore;

namespace GKUI.Platform
{
    /// <summary>
    /// WinForms-specific UI timer.
    /// </summary>
    public sealed class WFUITimer : BaseObject, ITimer
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
            set { fInnerTimer.Interval = (int)value; }
        }

        public WFUITimer(double msInterval, EventHandler elapsedHandler)
        {
            fElapsedHandler = elapsedHandler;

            fInnerTimer = new Timer();
            fInnerTimer.Interval = (int)msInterval;
            fInnerTimer.Tick += ElapsedEventHandler;
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
