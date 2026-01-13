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
    public sealed class LogManager
    {
        private static LogManager fLogManager;

        private LogManager(string logFileName, string logLevel)
        {
            try {
                Log4NetHelper.Init(logFileName, logLevel);
            } catch (Exception e) {
                Log4NetHelper.Init(@".\fatal.log", "ERROR");
                var l = new Log4NetHelper(GetType().ToString());
                l.WriteError("Error while initializing the logger", e);
            }
        }

        public static ILogger GetLogger(string logFileName, string logLevel, string loggerName)
        {
            if (fLogManager == null) {
                fLogManager = new LogManager(logFileName, logLevel);
            }
            return new Log4NetHelper(loggerName);
        }
    }
}
