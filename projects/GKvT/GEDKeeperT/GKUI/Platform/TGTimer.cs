/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GKCore;

namespace GKUI.Platform;

public sealed class TGTimer : ITimer
{
    public double Interval
    {
        get { return 0; }
        set { }
    }

    public bool Enabled
    {
        get { return false; }
        set { }
    }

    public TGTimer(double msInterval, EventHandler elapsedHandler)
    {
    }

    public void Dispose()
    {
    }

    public void Start()
    {
    }

    public void Stop()
    {
    }
}
