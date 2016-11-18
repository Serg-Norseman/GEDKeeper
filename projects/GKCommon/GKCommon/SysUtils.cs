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
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Text.RegularExpressions;
using System.Windows.Forms;

namespace GKCommon
{
    #if !__MonoCS__
    using Externals.MapiMail;
    #endif

    [SecurityCritical, SuppressUnmanagedCodeSecurity]
    public static class SysUtils
    {
        public static bool IsSetBit(uint val, int pos)
        {
            return (val & (1 << pos)) != 0;
        }

        #region Math helpers

        public static long Trunc(double value)
        {
            return (long)Math.Truncate(value);
        }

        public static double SafeDiv(double dividend, double divisor)
        {
            return (divisor == (double)0f) ? 0.0 : (dividend / divisor);
        }

        #endregion

        #region Date functions

        public static uint DaysBetween(DateTime now, DateTime then)
        {
            TimeSpan span = ((now < then) ? then - now : now - then);
            return (uint) (span.Days);
        }

        private static readonly ushort[][] MONTH_DAYS = new ushort[][]
        {
            new ushort[] { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
            new ushort[] { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
        };

        public static ushort DaysInAMonth(ushort year, ushort month)
        {
            return MONTH_DAYS[(month == 2 && DateTime.IsLeapYear(year)) ? 1 : 0][month - 1];
        }

        #endregion

        #region FileSystem helpers

        public static string GetFileExtension(string fileName)
        {
            string extension = Path.GetExtension(fileName);
            return string.IsNullOrEmpty(extension) ? string.Empty : extension.ToLowerInvariant();
        }

        public static void LoadExtFile(string fileName)
        {
            if (File.Exists(fileName))
            {
                #if __MonoCS__
                Process.Start(new ProcessStartInfo("file://"+fileName) { UseShellExecute = true });
                #else
                Process.Start(fileName);
                #endif
            }
        }

        #endregion

        #region KeyLayout functions

        public static int GetKeyLayout()
        {
            #if __MonoCS__
            // There is a bug in Mono: does not work this CurrentInputLanguage
            return CultureInfo.CurrentUICulture.KeyboardLayoutId;
            #else
            InputLanguage currentLang = InputLanguage.CurrentInputLanguage;
            return currentLang.Culture.KeyboardLayoutId;
            #endif
        }

        public static void SetKeyLayout(int layout)
        {
            try {
                CultureInfo cultureInfo = new CultureInfo(layout);
                InputLanguage currentLang = InputLanguage.FromCulture(cultureInfo);
                InputLanguage.CurrentInputLanguage = currentLang;
            } catch (Exception ex) {
                Logger.LogWrite("GKUtils.SetKeyLayout(): " + ex.Message);
            }
        }

        #endregion

        #region Network functions

        public static void SendMail(string address, string subject, string body, string attach)
        {
            if (File.Exists(attach)) {
                #if __MonoCS__

                string mailto = string.Format("mailto:{0}?Subject={1}&Body={2}&Attach={3}", address, subject, body, "" + attach + "");
                Process.Start(mailto);

                #else

                MapiMailMessage message = new MapiMailMessage(subject, body);
                message.Recipients.Add(address);
                message.Files.Add(attach);
                message.ShowDialog();

                #endif
            }
        }

        public static bool IsNetworkAvailable()
        {
            return System.Net.NetworkInformation.NetworkInterface.GetIsNetworkAvailable();
        }

        #endregion

        #region Assembly helpers

        public static T GetAssemblyAttribute<T>(Assembly assembly) where T : Attribute
        {
            if (assembly == null)
                throw new ArgumentNullException("assembly");

            object[] attributes = assembly.GetCustomAttributes(typeof(T), false);
            if (attributes == null || attributes.Length == 0)
                return null;
            return SingleOrDefault(OfTypeIterator((T[])attributes));
        }

        public static void GetAssemblyVersion(Assembly assembly, out string copyright, out string version)
        {
            copyright = "";
            version = "";

            object[] attributes = assembly.GetCustomAttributes(typeof(AssemblyCopyrightAttribute), false);
            if (attributes.Length != 0) copyright = ((AssemblyCopyrightAttribute)attributes[0]).Copyright;

            version = assembly.GetName().Version.ToString();
        }

        #endregion

        #region Native WinAPI

        public const uint WM_USER 			= 0x0400;
        public const uint WM_KEEPMODELESS 	= WM_USER + 111;

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool PostMessage(IntPtr hWnd, uint msg, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool EnableWindow(IntPtr hWnd, [MarshalAs(UnmanagedType.Bool)]bool bEnable);

        #endregion

        #region Cross-platform helpers

        private static ulong? fMonoVersion = null;

        public static ulong GetMonoVersion()
        {
            if (fMonoVersion.HasValue) return fMonoVersion.Value;

            ulong uVersion = 0;
            try
            {
                Type t = Type.GetType("Mono.Runtime");
                if (t != null)
                {
                    MethodInfo mi = t.GetMethod("GetDisplayName",
                                                BindingFlags.NonPublic | BindingFlags.Static);
                    if (mi != null)
                    {
                        string strName = (mi.Invoke(null, null) as string);
                        if (!string.IsNullOrEmpty(strName))
                        {
                            Match m = Regex.Match(strName, "\\d+(\\.\\d+)+");
                            if (m.Success)
                                uVersion = ParseVersion(m.Value);
                            else { Debug.Assert(false); }
                        }
                        else { Debug.Assert(false); }
                    }
                    else { Debug.Assert(false); }
                }
            }
            catch (Exception) { Debug.Assert(false); }

            fMonoVersion = uVersion;
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

        private static readonly char[] VersionSep = new char[] { '.', ',' };

        public static ulong ParseVersion(string strVersion)
        {
            if (strVersion == null) { Debug.Assert(false); return 0; }

            string[] vVer = strVersion.Split(VersionSep);
            if ((vVer == null) || (vVer.Length == 0)) { Debug.Assert(false); return 0; }

            ushort uPart;
            ushort.TryParse(vVer[0].Trim(), out uPart);
            ulong uVer = ((ulong)uPart << 48);

            if (vVer.Length >= 2)
            {
                ushort.TryParse(vVer[1].Trim(), out uPart);
                uVer |= ((ulong)uPart << 32);
            }

            if (vVer.Length >= 3)
            {
                ushort.TryParse(vVer[2].Trim(), out uPart);
                uVer |= ((ulong)uPart << 16);
            }

            if (vVer.Length >= 4)
            {
                ushort.TryParse(vVer[3].Trim(), out uPart);
                uVer |= (ulong)uPart;
            }

            return uVer;
        }

        #endregion

        #region Linq-pieces

        public static IEnumerable<TResult> OfTypeIterator<TResult>(IEnumerable<TResult> source)
        {
            foreach (object current in source)
            {
                if (current is TResult)
                {
                    yield return (TResult)((object)current);
                }
            }
            yield break;
        }

        public static TSource SingleOrDefault<TSource>(IEnumerable<TSource> source)
        {
            if (source == null)
                throw new ArgumentNullException("source");

            IList<TSource> list = source as IList<TSource>;

            if (list != null)
            {
                switch (list.Count)
                {
                    case 0:
                        return default(TSource);
                    case 1:
                        return list[0];
                }
            }
            else
            {
                using (IEnumerator<TSource> enumerator = source.GetEnumerator())
                {
                    if (!enumerator.MoveNext())
                    {
                        TSource result = default(TSource);
                        return result;
                    }
                    TSource current = enumerator.Current;
                    if (!enumerator.MoveNext())
                    {
                        TSource result = current;
                        return result;
                    }
                }
            }

            throw new Exception("MoreThanOneElement");
        }

        public static T FirstOrDefault<T>(IList<T> list)
        {
            if (list == null)
                throw new ArgumentNullException("list");

            return (list.Count > 0) ? list[0] : default(T);
        }

        public static T LastOrDefault<T>(IList<T> list)
        {
            if (list == null)
                throw new ArgumentNullException("list");

            int count = list.Count;
            return (count > 0) ? list[count - 1] : default(T);
        }

        #endregion
    }
}
