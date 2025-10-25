﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

#pragma warning disable CA1416

using System;
using System.Net.Http;
using System.Threading.Tasks;
using System.Xml;
using GKCore.Locales;

namespace GKCore
{
#if OS_LINUX
using GKCore.Utilities;
#endif

    public sealed class DistributedPackage
    {
        public Version Version { get; set; }
        public string URL { get; set; }

        public DistributedPackage(Version version, string url)
        {
            Version = version;
            URL = url;
        }
    }


    /// <summary>
    /// Update check manager.
    /// </summary>
    public static class UpdateMan
    {
        private static async Task<DistributedPackage> GetLastPackage()
        {
            var result = new DistributedPackage(null, "");

            try {
                GKUtils.InitSecurityProtocol();

                using (var webRequest = new HttpClient())
                using (var stream = await webRequest.GetStreamAsync(new Uri(GKData.UpdateURL)))
                using (var reader = new XmlTextReader(stream)) {
                    reader.MoveToContent();

                    string nodeName = "";
                    if ((reader.NodeType == XmlNodeType.Element) && (reader.Name == "gedkeeper")) {
                        while (reader.Read()) {
                            if (reader.NodeType == XmlNodeType.Element)
                                nodeName = reader.Name;
                            else {
                                if ((reader.NodeType == XmlNodeType.Text) && (reader.HasValue)) {
                                    switch (nodeName) {
                                        case "version":
                                            result.Version = new Version(reader.Value);
                                            break;

                                        case "url":
                                            result.URL = reader.Value;
                                            break;
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("UpdateMan.GetLastPackage()", ex);
            }

            return result;
        }

        public static async void CheckUpdate()
        {
            try {
                Version curVersion = AppHost.GetAppVersion();
                if (curVersion == null) return;

                var distrVersion = await GetLastPackage();

                var newVersion = distrVersion.Version;
                if (newVersion != null && curVersion.CompareTo(newVersion) < 0) {
#if !CI_MODE
                    string question = LangMan.LS(LSID.UpdateToLatestVersion, curVersion, newVersion);
                    if (await AppHost.StdDialogs.ShowQuestion(question)) {
                        GKUtils.LoadExtFile(distrVersion.URL);
                    }
#endif
                }
            } catch (Exception ex) {
                Logger.WriteError("UpdateMan.CheckUpdate()", ex);
            }
        }
    }
}
