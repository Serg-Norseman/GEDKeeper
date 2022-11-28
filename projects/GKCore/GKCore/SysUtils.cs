/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Runtime.CompilerServices;
using System.Text;

namespace GKCore
{
    #if !MONO
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

            try
            {
                #if MONO

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
                Logger.WriteError("SysUtils.SendMail()", ex);
            }
        }

        #endregion

        #region Runtime helpers

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

        public static string StripHTML(string source)
        {
            char[] array = new char[source.Length];
            int arrayIndex = 0;
            bool inside = false;
            for (int i = 0; i < source.Length; i++) {
                char let = source[i];
                if (let == '<') {
                    inside = true;
                    continue;
                }
                if (let == '>') {
                    inside = false;
                    continue;
                }
                if (!inside) {
                    array[arrayIndex] = let;
                    arrayIndex++;
                }
            }
            return new string(array, 0, arrayIndex);
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
    }
}
