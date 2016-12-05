/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Collections.Specialized;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Reflection;
using System.Xml;

using Externals;
using GKCommon;

namespace GKUpdater
{
    public delegate void UpdateMessageHandler(object sender, UpdateEventArgs uea);

    public class UpdateEventArgs : EventArgs
    {
        public readonly string Message;
        public readonly int Progress;

        public UpdateEventArgs(string message, int progress)
        {
            this.Message = message;
            this.Progress = progress;
        }
    }

    public sealed class Controller
    {
        public const string MainApp = "GEDKeeper2.exe";
        public const int Interval = 1; // now it's minutes
        public const string Url = "file:///d:/update.xml"; // TODO: url to site for update.xml
        public const int Timeout = 5000;
        public const string OptionsFileName = "GKUpdater.ini";
        public const string ProductUrl = "";


        private static Controller fInstance;
        private StringDictionary fApplicationUpdates;
        private StringDictionary fPluginUpdates;
        private StringDictionary fExtensionUpdates;
        private readonly string fUpdatesPath;

        public event UpdateMessageHandler DownloadProgress;
        public event UpdateMessageHandler UpdateMessage;

        public static Controller Instance
        {
            get
            {
                if (fInstance == null)
                    fInstance = new Controller();
                return fInstance;
            }
        }

        public bool NeedsUpdate
        {
            get
            {
                if (fApplicationUpdates != null && fApplicationUpdates.Count > 0)
                    return true;
                if (fPluginUpdates != null && fPluginUpdates.Count > 0)
                    return true;
                if (fExtensionUpdates != null && fExtensionUpdates.Count > 0)
                    return true;
                return false;
            }
        }


        private Controller()
        {
            Logger.LogInit(Path.Combine(GetAppPath(), "GKUpdater.log"));

            WebRequest.DefaultWebProxy = WebProxy.GetDefaultProxy();
            WebRequest.DefaultWebProxy.Credentials = CredentialCache.DefaultCredentials;

            // TODO: Temp or AppData?
            this.fUpdatesPath = Path.Combine(GetAppPath(), "updates");
        }

        private void OnUpdateMessage(UpdateEventArgs uea)
        {
            if (UpdateMessage != null)
                UpdateMessage(this, uea);
        }

        private void OnDownloadProgress(UpdateEventArgs uea)
        {
            if (DownloadProgress != null)
                DownloadProgress(this, uea);
        }

        public void CheckForUpdates(string url)
        {
            OnUpdateMessage(new UpdateEventArgs("Loading source of updates...", 0));

            string xml = this.InternalDownload(url);
            if (xml.Length > 0)
            {
                XmlDocument doc = new XmlDocument();
                try
                {
                    doc.LoadXml(xml);
                    XmlNode updateInfo = doc.SelectSingleNode("/UpdateInfo");

                    if (updateInfo != null) {
                        fApplicationUpdates = CheckApplicationUpdates(doc, "/UpdateInfo/Application");
                        fPluginUpdates = CheckApplicationUpdates(doc, "/UpdateInfo/Plugin");
                        fExtensionUpdates = CheckExtensions(doc, "/UpdateInfo/Extension");
                    }
                }
                catch (XmlException ex)
                {
                    Logger.LogWrite("Controller.CheckForUpdates(): " + ex.ToString());
                }
            }
        }

        private StringDictionary CheckApplicationUpdates(XmlDocument doc, string xpath)
        {
            StringDictionary updates = new StringDictionary();

            foreach (XmlNode plugin in doc.SelectNodes(xpath))
            {
                try
                {
                    string sName = plugin.SelectSingleNode("FileName").InnerText;
                    string checkFilename = Path.Combine(GetAppPath(), sName);

                    string newVersion = plugin.SelectSingleNode("Version").InnerText;

                    if (File.Exists(checkFilename))
                    {
                        OnUpdateMessage(new UpdateEventArgs(sName, 0));

                        FileVersionInfo info = FileVersionInfo.GetVersionInfo(checkFilename);
                        Version current = new Version(info.FileMajorPart, info.FileMinorPart, info.FileBuildPart, info.FilePrivatePart);
                        if (current < new Version(newVersion))
                        {
                            updates.Add(plugin.SelectSingleNode("Url").InnerText, sName);
                        }
                    }
                }
                catch (Exception ex)
                {
                    Logger.LogWrite("Controller.CheckApplicationUpdates(): " + ex.ToString());
                }
            }

            return updates;
        }

        private StringDictionary CheckExtensions(XmlDocument doc, string xpath)
        {
            StringDictionary updates = new StringDictionary();

            foreach (XmlNode plugin in doc.SelectNodes(xpath))
            {
                try
                {
                    string sName = plugin.SelectSingleNode("FileName").InnerText;
                    string checkFilename = Path.Combine(GetAppPath(), sName);

                    if (File.Exists(checkFilename))
                    {
                        updates.Add(plugin.SelectSingleNode("Url").InnerText, sName);
                    }
                }
                catch (Exception ex)
                {
                    Logger.LogWrite("Controller.CheckExtensions(): " + ex.ToString());
                }
            }

            return updates;
        }

        public bool Update()
        {
            if (!Directory.Exists(this.fUpdatesPath))
                Directory.CreateDirectory(this.fUpdatesPath);

            foreach (string key in fApplicationUpdates.Keys)
                if (!DownloadFile(key))
                    return false;

            foreach (string key in fPluginUpdates.Keys)
                if (!DownloadFile(key))
                    return false;

            foreach (string key in fExtensionUpdates.Keys)
                if (!DownloadFile(key))
                    return false;

            string[] files = Directory.GetFiles(this.fUpdatesPath);
            foreach (string source in files)
            {
                string target = Path.GetFileName(source);
                if (File.Exists(target))
                    File.Delete(target); // TODO: Backup und Rollback
                File.Move(source, target);
                File.Delete(source);
            }

            return true;
        }

        private bool DownloadFile(string url)
        {
            string localName = Path.Combine(this.fUpdatesPath, Path.GetFileName(url));

            int size = this.InternalDownload(url, localName);

            if (size <= 0) return false;

            if (string.Compare(Path.GetExtension(localName), ".zip", true) == 0)
            {
                ZipStorer zip = ZipStorer.Open(localName, FileAccess.Read);
                try
                {
                    zip.ExtractFiles(this.fUpdatesPath);
                    zip.Close();

                    File.Delete(localName);
                }
                catch (Exception ex)
                {
                    Logger.LogWrite("Controller.DownloadFile(): " + ex.ToString());
                    return false;
                }
            }

            return true;
        }

        #region Internal download's routines

        private string InternalDownload(string url)
        {
            string txt = string.Empty;
            WebResponse response = null;
            WebRequest  request = WebRequest.Create(url);
            try
            {
                request.Timeout = Controller.Timeout;
                request.Credentials = CredentialCache.DefaultCredentials;

                response = request.GetResponse();
                Stream stream = response.GetResponseStream();
                StreamReader sr = new StreamReader(stream);

                txt = sr.ReadToEnd();

                sr.Close();
                stream.Close();
            }
            catch (WebException ex)
            {
                Logger.LogWrite("Controller.InternalDownload.1(): " + ex.ToString());
            }
            finally
            {
                if (response != null) response.Close();
            }

            return txt;
        }

        private int InternalDownload(string url, string local)
        {
            OnDownloadProgress(new UpdateEventArgs(url, 0));

            long fileSize = GetFileSize(url);
            if (fileSize == 0) return 0;

            WebRequest request = WebRequest.Create(url);
            request.Credentials = CredentialCache.DefaultCredentials;
            request.Timeout = 5000;

            WebResponse response = null;
            Stream stmIn = null, stmOut = null;
            long localSize = 0;

            try
            {
                response = request.GetResponse();
                stmIn = response.GetResponseStream();
                stmOut = File.Create(local);
                byte[] buffer = new byte[1024];
                int bytesRead = 0;
                do
                {
                    bytesRead = stmIn.Read(buffer, 0, 1024);
                    stmOut.Write(buffer, 0, bytesRead);
                    localSize += bytesRead;

                    float progress = (localSize * 100.0f)/ (float)fileSize;
                    OnDownloadProgress(new UpdateEventArgs(url, Math.Min((int)progress, 100)));
                } while (bytesRead > 0);
            }
            catch (WebException ex)
            {
                Logger.LogWrite("Controller.InternalDownload.2(): " + ex.ToString());
            }
            finally
            {
                if (stmIn != null) stmIn.Close();
                if (response != null) response.Close();
                if (stmOut != null) {
                    stmOut.Flush();
                    stmOut.Close();
                }
            }

            return (int)localSize;
        }

        private long GetFileSize(string url)
        {
            long fileSize = 0;

            WebResponse response = null;
            try
            {
                WebRequest request = WebRequest.Create(url);
                request.Credentials = CredentialCache.DefaultCredentials;
                request.Timeout = 5000;
                response = request.GetResponse();
                fileSize = response.ContentLength;
            }
            catch (WebException ex)
            {
                Logger.LogWrite("Controller.GetFileSize(): " + ex.ToString());
            }
            finally
            {
                if (response != null) response.Close();
            }

            return fileSize;
        }

        #endregion

        #region Main application's specifics

        private static string GetAppPath()
        {
            Module[] mods = Assembly.GetExecutingAssembly().GetModules();
            string fn = mods[0].FullyQualifiedName;
            return Path.GetDirectoryName(fn) + Path.DirectorySeparatorChar;
        }

        public static void StartMainApp()
        {
            string mainApp = Path.Combine(GetAppPath(), Controller.MainApp);

            if (!string.IsNullOrEmpty(mainApp) && File.Exists(mainApp))
                Process.Start(mainApp);
        }

        public static DateTime LoadLastCheck()
        {
            string iniName = Path.Combine(GetAppPath(), Controller.OptionsFileName);

            try
            {
                IniFile ini = new IniFile(iniName);
                try
                {
                    return ini.ReadDateTime("Common", "LastCheck", DateTime.FromBinary(0));
                } finally {
                    ini.Dispose();
                }
            } catch (Exception ex) {
                Logger.LogWrite("Controller.LoadLastCheck(): " + ex.Message);
                return DateTime.FromBinary(0);
            }
        }

        public static void SaveLastCheck(DateTime lastCheck)
        {
            string iniName = Path.Combine(GetAppPath(), Controller.OptionsFileName);

            try {
                IniFile ini = new IniFile(iniName);

                try
                {
                    ini.WriteDateTime("Common", "LastCheck", lastCheck);
                }
                finally
                {
                    ini.Dispose();
                }
            } catch (Exception ex) {
                Logger.LogWrite("Controller.SaveLastCheck(): " + ex.Message);
            }
        }

        #endregion
    }
}
