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
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using System.Xml;

using GKCommon;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public static class UpdateMan
    {
        private const string UPDATE_URL = "https://sourceforge.net/projects/gedkeeper/files/gk_version.xml";

        public static Version GetLastVersion(out string url)
        {
            Version newVersion = null;
            url = "";

            XmlTextReader reader = null;
            try
            {
                reader = new XmlTextReader(UPDATE_URL);
                reader.MoveToContent();

                string nodeName = "";
                if ((reader.NodeType == XmlNodeType.Element) && (reader.Name == "gedkeeper"))
                {
                    while (reader.Read())
                    {
                        if (reader.NodeType == XmlNodeType.Element)
                            nodeName = reader.Name;
                        else
                        {
                            if ((reader.NodeType == XmlNodeType.Text) && (reader.HasValue))
                            {
                                switch (nodeName)
                                {
                                    case "version":
                                        newVersion = new Version(reader.Value);
                                        break;

                                    case "url":
                                        url = reader.Value;
                                        break;
                                }
                            }
                        }
                    }
                }
            }
            catch (Exception)
            {
            }
            finally
            {
                if (reader != null) reader.Close();
            }

            return newVersion;
        }

        private static void WorkerMethod()
        {
            try {
                Version curVersion = Assembly.GetExecutingAssembly().GetName().Version;

                string url;
                Version newVersion = GetLastVersion(out url);

                if (curVersion.CompareTo(newVersion) < 0)
                {
                    string question = "You've got version {0} of GEDKeeper. Would you like to update to the latest version {1}?";

                    #if !CI_MODE
                    if (DialogResult.Yes == GKUtils.ShowQuestion(string.Format(question, curVersion, newVersion)))
                    {
                        Process.Start(url);
                    }
                    #endif
                }
            }
            catch (Exception ex)
            {
                Logger.LogWrite("UpdateMan.WorkerMethod(): " + ex.Message);
            }
        }

        public static void CheckUpdate()
        {
            try
            {
                #if __MonoCS__
                DesktopType desktopType = SysUtils.GetDesktopType();
                if (desktopType == DesktopType.Unity) {
                    // In Ubuntu 1604 LTS (Unity desktop), this method leads to a
                    // complete crash of the program at the level of X11,
                    // but in the same version of Ubuntu and Xfce, everything is fine
                    Logger.LogWrite("UpdateMan.CheckUpdate(): is not supported for Unity");
                    return;
                }
                #endif

                Thread worker = new Thread(WorkerMethod);
                worker.SetApartmentState(ApartmentState.STA);
                worker.IsBackground = true;
                worker.Start();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("UpdateMan.CheckUpdate(): " + ex.Message);
            }
        }
    }
}
