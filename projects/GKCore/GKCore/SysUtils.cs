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
using System.Reflection;
using System.Text;

namespace GKCore
{
    #if !__MonoCS__
    using GKCore.MapiMail;
    #endif

    public enum DesktopType
    {
        None = 0,
        Windows,
        Gnome,
        Kde,
        Unity,
        Lxde,
        Xfce,
        Mate,
        Cinnamon,
        Pantheon
    }

    public static class SysUtils
    {
        #region FileSystem helpers

        public static bool IsUnicodeEncoding(Encoding encoding)
        {
            return (encoding == Encoding.Unicode || encoding == Encoding.UTF7 || encoding == Encoding.UTF8 || encoding == Encoding.UTF32);
        }

        #endregion

        #region Network functions

        public static bool IsNetworkAvailable()
        {
            return System.Net.NetworkInformation.NetworkInterface.GetIsNetworkAvailable();
        }

        public static void SendMail(string address, string subject, string body, string attach)
        {
            if (!File.Exists(attach)) return;

            try
            {
                #if __MonoCS__

                const string mailto = "'{0}' --subject '{1}' --body '{2}' --attach {3}";
                string args = string.Format(mailto, address, subject, body, attach);

                var proc = new System.Diagnostics.Process();
                proc.EnableRaisingEvents = false;
                proc.StartInfo.FileName = "xdg-email";
                proc.StartInfo.Arguments = args;
                proc.Start();

                #else

                MapiMailMessage message = new MapiMailMessage(subject, body);
                message.Recipients.Add(address);
                message.Files.Add(attach);
                message.ShowDialog();

                #endif
            }
            catch (Exception ex)
            {
                Logger.LogWrite("SysUtils.SendMail(): " + ex.Message);
            }
        }

        #endregion

        #region Assembly helpers

        public static T GetAssemblyAttribute<T>(Assembly assembly) where T : Attribute
        {
            if (assembly == null)
                throw new ArgumentNullException("assembly");

            object[] attributes = assembly.GetCustomAttributes(typeof(T), false);
            T result;
            if (attributes == null || attributes.Length == 0) {
                result = null;
            } else {
                result = attributes[0] as T;
            }
            return result;
        }

        #endregion

        #region Cross-platform helpers

        public static string GetMonoVersion()
        {
            string uVersion = "";
            try {
                Type t = Type.GetType("Mono.Runtime");
                if (t != null) {
                    MethodInfo mi = t.GetMethod("GetDisplayName",
                                        BindingFlags.NonPublic | BindingFlags.Static);
                    if (mi != null) {
                        uVersion = (mi.Invoke(null, null) as string);
                    }
                }
            } catch {
                // dummy
            }

            return uVersion;
        }

        private static bool? fIsUnix = null;

        public static bool IsUnix()
        {
            if (fIsUnix.HasValue) return fIsUnix.Value;

            PlatformID p = GetPlatformID();

            // Mono defines Unix as 128 in early .NET versions
            fIsUnix = ((p == PlatformID.Unix) || (p == PlatformID.MacOSX) || ((int)p == 128));

            return fIsUnix.Value;
        }

        private static PlatformID? fPlatformID = null;

        public static PlatformID GetPlatformID()
        {
            if (fPlatformID.HasValue) return fPlatformID.Value;

            fPlatformID = Environment.OSVersion.Platform;

            return fPlatformID.Value;
        }

        public static DesktopType GetDesktopType()
        {
            DesktopType deskType = DesktopType.None;

            if (!IsUnix()) {
                deskType = DesktopType.Windows;
            } else {
                try
                {
                    string strXdg = (Environment.GetEnvironmentVariable(
                        "XDG_CURRENT_DESKTOP") ?? string.Empty).Trim();
                    string strGdm = (Environment.GetEnvironmentVariable(
                        "GDMSESSION") ?? string.Empty).Trim();
                    StringComparison sc = StringComparison.OrdinalIgnoreCase;

                    if (strXdg.Equals("Unity", sc))
                        deskType = DesktopType.Unity;
                    else if (strXdg.Equals("LXDE", sc))
                        deskType = DesktopType.Lxde;
                    else if (strXdg.Equals("XFCE", sc))
                        deskType = DesktopType.Xfce;
                    else if (strXdg.Equals("MATE", sc))
                        deskType = DesktopType.Mate;
                    else if (strXdg.Equals("X-Cinnamon", sc))
                        deskType = DesktopType.Cinnamon;
                    else if (strXdg.Equals("Pantheon", sc)) // Elementary OS
                        deskType = DesktopType.Pantheon;
                    else if (strXdg.Equals("KDE", sc) || // Mint 16
                             strGdm.Equals("kde-plasma", sc)) // Ubuntu 12.04
                        deskType = DesktopType.Kde;
                    else if (strXdg.Equals("GNOME", sc))
                    {
                        if (strGdm.Equals("cinnamon", sc)) // Mint 13
                            deskType = DesktopType.Cinnamon;
                        else deskType = DesktopType.Gnome;
                    }
                } catch (Exception) {
                    Debug.Assert(false);
                }
            }

            return deskType;
        }

        #endregion
    }
}
