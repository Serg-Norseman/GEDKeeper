/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Diagnostics;
using System.IO;
using System.Text;

namespace GKCore
{
    public static class Logger
    {
        private static readonly object fLock = new object();
        private static string fLogFilename;
        private static readonly string EventLogSource = "Application";

        public static void LogInit(string fileName)
        {
            fLogFilename = fileName;
        }

        public static void LogWrite(string msg)
        {
            try {
                lock (fLock) {
                    using (StreamWriter log = new StreamWriter(fLogFilename, true, Encoding.UTF8))
                    {
                        log.WriteLine("[" + DateTime.Now.ToString() + "] -> " + msg);
                        log.Flush();
                        log.Close();
                    }
                }
            } catch(Exception ex){
                try
                {
                    string exceptionInfo = "Loggin message into file log failed, see more info:\n" + BuildExceptionMessage(ex, "");
                    if (EventLog.Exists(EventLogSource))
                        EventLog.WriteEntry(EventLogSource, exceptionInfo, EventLogEntryType.Error, 1, 1);
                }
                catch
                {
                    //if we log some information about application state and it is not Exception
                    //we can pass this exception if we fail on writing message and cannot write info to Windows Application log
                }
            }
        }
        /// <summary>
        /// Writing exception information into text log
        /// </summary>
        /// <param name="e"></param>
        public static void LogException(Exception e)
        {
            //Adding a new line to avoid separating first line of exception and other lines, because in LogWrite we logging time of message on the left of the message text
            string exceptionMessage = "\n" + BuildExceptionMessage(e, "    ");
            LogWrite(exceptionMessage);
        }
        /// <summary>
        /// Building full exception information with inner exceptions and stack traces
        /// </summary>
        /// <param name="e">Current exception</param>
        /// <param name="baseTag">Current line indent to user-frendly exception visualization</param>
        /// <returns>Full exception information string </returns>
        private static string BuildExceptionMessage(Exception e, string baseTag)
        {
            if (e == null)
                return string.Empty;

            string message = baseTag + e.Message;
            string tag = baseTag + "    ";
            if (e.Data != null)
            {
                message += "\n" + tag + "Exception data:";
                foreach (object key in e.Data.Keys)
                    message += "\n" + tag + e.Data[key];
            }
            message += "\n" + tag + "StackTrace:";
            message += "\n" + tag + e.StackTrace;            
            if (e.InnerException != null)
                message += BuildExceptionMessage(e.InnerException, tag);
            return message;
        }
    }
}
