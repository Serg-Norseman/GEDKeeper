/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;
using stTimer = System.Threading.Timer;

namespace GKUI.Platform;

public sealed class TGTimer : ITimer
{
    private readonly stTimer fTimer;
    private bool fEnabled;
    private int fInterval;

    public double Interval
    {
        get { return fInterval; }
        set { }
    }

    public bool Enabled
    {
        get { return fEnabled; }
        set { fEnabled = value; }
    }

    public TGTimer(double msInterval, EventHandler elapsedHandler)
    {
        fInterval = (int)msInterval;
        fTimer = new stTimer((object state) => {
            if (fEnabled && elapsedHandler != null) {
                elapsedHandler(this, new EventArgs());
            }
        }, null, 0, fInterval);
    }

    public void Dispose()
    {
        if (fTimer != null)
            fTimer.Dispose();
    }

    public void Start()
    {
        fEnabled = true;
    }

    public void Stop()
    {
        fEnabled = false;
    }
}
