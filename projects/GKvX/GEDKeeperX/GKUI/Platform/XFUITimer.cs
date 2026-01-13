/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
