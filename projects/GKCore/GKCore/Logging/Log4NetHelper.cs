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
using System.IO;
using System.Reflection;
using System.Xml;
using log4net;
using log4net.Config;

namespace GKCore.Logging
{
    [Serializable]
    internal class Log4NetHelper : ILogger
    {
        private readonly string fContext;
        private readonly ILog fLog;

        public Log4NetHelper()
        {
        }

        public Log4NetHelper(string loggerName)
        {
            fLog = log4net.LogManager.GetLogger(loggerName);
        }

        public Log4NetHelper(string loggerName, string context)
        {
            fLog = log4net.LogManager.GetLogger(loggerName);
            fContext = context;
        }

        public static void Configure()
        {
            XmlConfigurator.Configure();
        }

        public static void Init(string fileName, string logLevel)
        {
            var s = "<?xml version=\"1.0\"?>" + "<log4net>";
            s = s + "<appender name=\"Logs\" type=\"log4net.Appender.RollingFileAppender\">";
            //s = s + "<bufferSize value=\"1\"/>";
            s = s + "<file value=\"" + fileName.Replace("\\", "\\\\") + "\"/>";
            s = s + "<appendToFile value=\"true\"/>";
            s = s + "<rollingStyle value=\"Size\"/>";
            s = s + "<maximumFileSize value=\"100MB\"/>";
            s = s + "<maxSizeRollBackups value=\"10\"/>";
            s = s + "<lockingModel type=\"log4net.Appender.FileAppender+MinimalLock\"/>";
            s = s + "<layout type=\"log4net.Layout.PatternLayout\">";
            s = s + "<conversionPattern value=\"%date[%-2thread][%-5level][%logger] %message%newline\"/>";
            s = s + "</layout>";
            s = s + "</appender>";
            s = s + "<root>";
            s = s + "<level value=\"" + logLevel + "\"/>";
            s = s + "<appender-ref ref=\"Logs\"/>";
            s = s + "</root>";
            s = s + "</log4net>";

            TextReader tr = new StringReader(s);
            var xmlDoc = new XmlDocument();
            xmlDoc.Load(tr);
            XmlConfigurator.Configure((XmlElement)xmlDoc.SelectSingleNode("log4net"));
        }

        public static void InitConfiguration(string configFileName)
        {
            XmlConfigurator.Configure(new FileInfo(configFileName));
        }

        public static void InitConfiguration(string configFileName, string logFileName)
        {
            var j = new XmlDocument();
            j.Load(configFileName);

            var n = j.SelectSingleNode("configuration/log4net/appender/file");
            if (n != null) {
                if (n.Attributes != null) {
                    var s = n.Attributes["value"].Value;
                    if (s.IndexOf("xxx") > -1) {
                        n.Attributes["value"].Value = s.Replace("xxx", logFileName);
                    } else {
                        var jj = s.LastIndexOf('\\');
                        var hh = s.Substring(0, jj + 1);
                        n.Attributes["value"].Value = hh + logFileName + ".log";
                    }
                }
            }
            XmlConfigurator.Configure((XmlElement)j.SelectSingleNode("configuration/log4net"));
        }

        public void WriteDebug(string msg)
        {
            fLog.Debug(string.IsNullOrEmpty(fContext) ? msg : string.Format("[{0}] {1}", fContext, msg));
        }

        public void WriteDebug(string str, params object[] args)
        {
            WriteDebug(string.Format(str, args));
        }

        public void WriteInfo(string msg)
        {
            fLog.Info(string.IsNullOrEmpty(fContext) ? msg : string.Format("[{0}] {1}", fContext, msg));
        }

        public void WriteInfo(string str, params object[] args)
        {
            WriteInfo(string.Format(str, args));
        }

        public void WriteError(string msg)
        {
            fLog.Error(string.IsNullOrEmpty(fContext) ? msg : string.Format("[{0}] {1}", fContext, msg));
        }

        public void WriteError(string msg, Exception ex)
        {
            fLog.Error(string.IsNullOrEmpty(fContext) ? msg : string.Format("[{0}] {1}", fContext, msg), ex);
        }

        public void WriteWarn(string msg)
        {
            fLog.Warn(string.IsNullOrEmpty(fContext) ? msg : string.Format("[{0}] {1}", fContext, msg));
        }

        public void WriteWarn(string str, params object[] args)
        {
            WriteInfo(string.Format(str, args));
        }


        public void WriteNumError(int num, Exception ex)
        {
            var err = ex as ReflectionTypeLoadException;
            if (err != null) {
                foreach (var item in err.LoaderExceptions) {
                    fLog.Error(item.Message);
                }
            }
            fLog.Error("#" + num + " " + ex.Message);
            fLog.Error(ex.StackTrace);
        }
    }
}
