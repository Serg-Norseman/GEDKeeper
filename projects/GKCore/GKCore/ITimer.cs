/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GKCore
{
    /// <summary>
    /// An interface for cross-platform timers provided by the framework to the application core.
    /// </summary>
    public interface ITimer : IDisposable
    {
        /// <summary>
        /// Gets or sets the interval, in milliseconds.
        /// </summary>
        double Interval
        {
            get;
            set;
        }

        bool Enabled
        {
            get; set;
        }

        void Start();
        void Stop();
    }
}
