/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using BSLib;

namespace GKCore
{
#if OS_MSWIN
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

    public enum OSType
    {
        Unknown = 0,
        Linux,
        MacOS,
        Windows95,
        Windows98,
        WindowsMe,
        WindowsNT351,
        WindowsNT40,
        Windows2000,
        WindowsXP,
        WindowsServer2003,
        WindowsVista,
        //WindowsServer2008,
        Windows7,
        //WindowsServer2008R2,
        Windows8,
        Windows81,
        Windows10,
        Windows11,
    }

    public static class SysUtils
    {
        #region FileSystem helpers

        public static bool IsUnicodeEncoding(Encoding encoding)
        {
            return (encoding != null) && (encoding == Encoding.Unicode || encoding == Encoding.UTF7 || encoding == Encoding.UTF8 || encoding == Encoding.UTF32);
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

            try {
#if !OS_MSWIN
                const string mailto = "{0} --subject \"{1}\" --body \"{2}\" --attach {3}";
                attach = string.Join("", "\"", attach, "\"");
                string args = string.Format(mailto, address, subject, body, attach);

                var proc = new System.Diagnostics.Process();
                proc.EnableRaisingEvents = false;
                proc.StartInfo.FileName = "xdg-email";
                proc.StartInfo.Arguments = args;
                proc.Start();
#else
                MapiMailMessage message = new MapiMailMessage(subject, body);
                if (!string.IsNullOrEmpty(address)) {
                    message.Recipients.Add(address);
                }
                message.Files.Add(attach);
                message.ShowDialog();
#endif
            } catch (Exception ex) {
                Logger.WriteError("SysUtils.SendMail()", ex);
            }
        }

        #endregion

        #region Runtime helpers

        public static Assembly GetExecutingAssembly()
        {
            Assembly asm = Assembly.GetEntryAssembly();
            if (asm == null) {
                asm = Assembly.GetExecutingAssembly();
            }
            return asm;
        }

        public static bool ImplementsInterface(Type type, Type ifaceType)
        {
            Type[] intf = type.GetInterfaces();
            for (int i = 0; i < intf.Length; i++) {
                if (intf[i] == ifaceType) {
                    return true;
                }
            }
            return false;
        }

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
                try {
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
                    else if (strXdg.Equals("GNOME", sc)) {
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

        public static OSType GetOSType()
        {
            var result = OSType.Unknown;

#if OS_LINUX
            result = OSType.Linux;
#endif

#if OS_MACOS
            result = OSType.MacOS;
#endif

#if OS_MSWIN
            OperatingSystem osVersion = Environment.OSVersion;
            int majorVersion = osVersion.Version.Major;
            int minorVersion = osVersion.Version.Minor;

            switch (osVersion.Platform) {
                case PlatformID.Win32Windows: {
                        if (majorVersion == 4) {
                            switch (minorVersion) {
                                case 0:
                                    result = OSType.Windows95;
                                    break;
                                case 10:
                                    result = OSType.Windows98;
                                    break;
                                case 90:
                                    result = OSType.WindowsMe;
                                    break;
                            }
                        }
                        break;
                    }

                case PlatformID.Win32NT: {
                        switch (majorVersion) {
                            case 3:
                                result = OSType.WindowsNT351;
                                break;
                            case 4:
                                result = OSType.WindowsNT40;
                                break;
                            case 5:
                                switch (minorVersion) {
                                    case 0:
                                        result = OSType.Windows2000;
                                        break;
                                    case 1:
                                        result = OSType.WindowsXP;
                                        break;
                                    case 2:
                                        result = OSType.WindowsServer2003;
                                        break;
                                }
                                break;
                            case 6:
                                switch (minorVersion) {
                                    case 0:
                                        result = OSType.WindowsVista;
                                        break;
                                    case 1:
                                        result = OSType.Windows7;
                                        break;
                                    case 2:
                                        result = OSType.Windows8;
                                        break;
                                    case 3:
                                        result = OSType.Windows81;
                                        break;
                                }
                                break;
                            case 10:
                                if (osVersion.Version.Build >= 22000) {
                                    result = OSType.Windows11;
                                } else {
                                    result = OSType.Windows10;
                                }
                                break;
                        }
                        break;
                    }
            }
#endif

            return result;
        }

        #endregion

        public static string StripHTML(string source)
        {
            return source == null ? string.Empty : Regex.Replace(source, "<.*?>", string.Empty);
        }

        public static void Shuffle<T>(this T[] array)
        {
            var rng = new Random();

            int n = array.Length;
            while (n > 1) {
                int k = rng.Next(n--);
                T temp = array[n];
                array[n] = array[k];
                array[k] = temp;
            }
        }

        [MethodImpl(MethodImplOptions.NoOptimization | MethodImplOptions.NoInlining)]
        public static void DoNotInline(object obj)
        {
        }

        /// <summary>
        /// Returns a new string in which all occurrences of a specified string in the current instance are replaced with another 
        /// specified string according the type of search to use for the specified string.
        /// </summary>
        /// <param name="str">The string performing the replace method.</param>
        /// <param name="oldValue">The string to be replaced.</param>
        /// <param name="newValue">The string replace all occurrences of <paramref name="oldValue"/>. 
        /// If value is equal to <c>null</c>, than all occurrences of <paramref name="oldValue"/> will be removed from the <paramref name="str"/>.</param>
        /// <param name="comparisonType">One of the enumeration values that specifies the rules for the search.</param>
        /// <returns>A string that is equivalent to the current string except that all instances of <paramref name="oldValue"/> are replaced with <paramref name="newValue"/>. 
        /// If <paramref name="oldValue"/> is not found in the current instance, the method returns the current instance unchanged.</returns>
        [DebuggerStepThrough]
        public static string Replace(this string str, string oldValue, string newValue, StringComparison comparisonType)
        {
            // Check inputs.
            if (str == null) {
                // Same as original .NET C# string.Replace behavior.
                throw new ArgumentNullException("str");
            }
            if (str.Length == 0) {
                // Same as original .NET C# string.Replace behavior.
                return str;
            }
            if (oldValue == null) {
                // Same as original .NET C# string.Replace behavior.
                throw new ArgumentNullException("oldValue");
            }
            if (oldValue.Length == 0) {
                // Same as original .NET C# string.Replace behavior.
                throw new ArgumentException("String cannot be of zero length.");
            }

            //if (oldValue.Equals(newValue, comparisonType))
            //{
            //This condition has no sense
            //It will prevent method from replacesing: "Example", "ExAmPlE", "EXAMPLE" to "example"
            //return str;
            //}

            // Prepare string builder for storing the processed string.
            // Note: StringBuilder has a better performance than String by 30-40%.
            var result = new StringBuilder(str.Length);

            // Analyze the replacement: replace or remove.
            bool isReplacementNullOrEmpty = string.IsNullOrEmpty(newValue);

            // Replace all values.
            const int valueNotFound = -1;
            int foundAt;
            int startSearchFromIndex = 0;
            while ((foundAt = str.IndexOf(oldValue, startSearchFromIndex, comparisonType)) != valueNotFound) {
                // Append all characters until the found replacement.
                int charsUntilReplacment = foundAt - startSearchFromIndex;
                bool isNothingToAppend = charsUntilReplacment == 0;
                if (!isNothingToAppend) {
                    result.Append(str, startSearchFromIndex, charsUntilReplacment);
                }

                // Process the replacement.
                if (!isReplacementNullOrEmpty) {
                    result.Append(newValue);
                }

                // Prepare start index for the next search.
                // This needed to prevent infinite loop, otherwise method always start search 
                // from the start of the string. For example: if an oldValue == "EXAMPLE", newValue == "example"
                // and comparisonType == "any ignore case" will conquer to replacing:
                // "EXAMPLE" to "example" to "example" to "example" … infinite loop.
                startSearchFromIndex = foundAt + oldValue.Length;
                if (startSearchFromIndex == str.Length) {
                    // It is end of the input string: no more space for the next search.
                    // The input string ends with a value that has already been replaced. 
                    // Therefore, the string builder with the result is complete and no further action is required.
                    return result.ToString();
                }
            }

            // Append the last part to the result.
            int charsUntilStringEnd = str.Length - startSearchFromIndex;
            result.Append(str, startSearchFromIndex, charsUntilStringEnd);

            return result.ToString();
        }

        // Converts a string of the form #RRGGBB to a Color instance.
        // Used when retrieving colours from the config.
        public static int ParseColor(string s)
        {
            if (string.IsNullOrEmpty(s)) {
                return 0;
            }

            int r = 0, g = 0, b = 0;

            if (s[0] != '#') {
                s = '#' + s;
            }

            switch (s.Length) {
                case 4:
                    s = s.Substring(1);
                    goto case 3;
                case 3:
                    r = int.Parse(s.Substring(0, 1), NumberStyles.HexNumber);
                    g = int.Parse(s.Substring(1, 1), NumberStyles.HexNumber);
                    b = int.Parse(s.Substring(2, 1), NumberStyles.HexNumber);
                    break;
                case 7:
                    s = s.Substring(1);
                    goto case 6;
                case 6:
                    r = int.Parse(s.Substring(0, 2), NumberStyles.HexNumber);
                    g = int.Parse(s.Substring(2, 2), NumberStyles.HexNumber);
                    b = int.Parse(s.Substring(4, 2), NumberStyles.HexNumber);
                    break;
            }

            return GfxHelper.MakeArgb(255, r, g, b);
        }

        public static string StringifyColor(int argb)
        {
            //string.Format("{0:X2}{1:X2}{2:X2}", c.R, c.G, c.B);
            argb &= 0xFFFFFF;
            string result = argb.ToString("X6");
            return result;
        }

        public static string GetToken(string str, string sepChars, int tokenNum)
        {
            if (string.IsNullOrEmpty(str) || string.IsNullOrEmpty(sepChars)) {
                return string.Empty;
            }

            if (sepChars.IndexOf(str[str.Length - 1]) < 0) {
                str += sepChars[0];
            }

            int tok_num = -1;
            string tok = "";

            for (int i = 0; i < str.Length; i++) {
                if (sepChars.IndexOf(str[i]) >= 0) {
                    tok_num++;
                    if (tok_num == tokenNum) {
                        return tok;
                    }
                    tok = string.Empty;
                } else {
                    tok += (str[i]);
                }

            }
            return string.Empty;
        }

        public static string GetWordForm(string ls, int selector)
        {
            try {
                string result;

                string word = GetToken(ls, "[]", 0);
                string sfx = GetToken(ls, "[]", 1);

                if (string.IsNullOrEmpty(sfx)) {
                    result = word;
                } else {
                    string sf = GetToken(sfx, "|", selector);
                    if (string.IsNullOrEmpty(sf)) {
                        result = word;
                    } else {
                        if (sf[0] != '-') {
                            result = sf;
                        } else {
                            sf = sf.Substring(1);
                            result = word + sf;
                        }
                    }
                }

                return result;
            } catch (Exception ex) {
                Logger.WriteError("SysUtils.GetWordForm(): " + ex.Message);
                throw ex;
            }
        }
    }
}
