/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GKCore.Logging
{
    /// <summary>
    /// Levels in order of increasing priority: ALL, DEBUG, INFO, WARN, ERROR, FATAL, OFF
    /// </summary>
    public interface ILogger
    {
        void WriteDebug(string msg);
        void WriteDebug(string str, params object[] args);

        void WriteInfo(string msg);
        void WriteInfo(string str, params object[] args);

        void WriteWarn(string msg);
        void WriteWarn(string str, params object[] args);

        void WriteError(string msg);
        void WriteError(string msg, Exception ex);

        // unused
        void WriteNumError(int num, Exception ex);
    }
}
