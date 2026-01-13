/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
