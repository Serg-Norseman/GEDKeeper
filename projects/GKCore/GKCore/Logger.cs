/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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
using System.IO;
using System.Text;

namespace GKCore
{
    public static class Logger
    {
        private static readonly object fLock = new object();
        private static string fLogFilename;

        public static void Init(string fileName)
        {
            fLogFilename = fileName;
        }

        public static void WriteInfo(string msg)
        {
            try {
                lock (fLock) {
                    using (StreamWriter log = new StreamWriter(fLogFilename, true, Encoding.UTF8)) {
                        log.WriteLine("[" + DateTime.Now.ToString() + "] -> " + msg);
                        log.Flush();
                        log.Close();
                    }
                }
            } catch (Exception ex) {
            }
        }

        /// <summary>
        /// Writing exception information into text log.
        /// </summary>
        public static void WriteError(string msg)
        {
            WriteInfo(msg);
        }

        /// <summary>
        /// Writing exception information into text log.
        /// </summary>
        /// <param name="msg"></param>
        /// <param name="ex"></param>
        public static void WriteError(string msg, Exception ex)
        {
            string exceptionMessage = "\n" + msg + ": " + BuildExceptionMessage(ex, "    ");
            WriteInfo(exceptionMessage);
        }

        /// <summary>
        /// Building full exception information with inner exceptions and stack traces. 
        /// Author: Maxim Yugov (aka Akeloya).
        /// </summary>
        /// <param name="e">Current exception</param>
        /// <param name="baseIndent">Current line indent to user-frendly exception visualization</param>
        /// <returns>Full exception information string </returns>
        private static string BuildExceptionMessage(Exception e, string baseIndent)
        {
            if (e == null)
                return string.Empty;

            string message = baseIndent + e.Message;
            string indent = baseIndent + "    ";
            if (e.Data != null) {
                message += "\n" + indent + "Exception data:";
                foreach (object key in e.Data.Keys)
                    message += "\n" + indent + e.Data[key];
            }
            message += "\n" + indent + "StackTrace:";
            message += "\n" + indent + (e.StackTrace == null ? string.Empty : e.StackTrace.Replace("\n", "\n" + indent));
            message += BuildExceptionMessage(e.InnerException, indent);
            return message;
        }
    }
}
