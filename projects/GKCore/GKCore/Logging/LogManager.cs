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
