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
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

namespace GKCommon
{
    #if PCL
    public interface ICloneable
    {
        object Clone();
    }
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
        static SysUtils()
        {
            InitCRC32();
            InitRome();
        }

        public static bool IsSetBit(int val, int pos)
        {
            return (val & (1 << pos)) != 0;
        }

        public static bool IsDigit(char chr)
        {
            return chr >= '0' && chr <= '9';
        }

        public static bool IsDigits(string str)
        {
            bool res = false;

            if (!string.IsNullOrEmpty(str))
            {
                int I;
                for (I = 1; I <= str.Length; I++)
                {
                    char c = str[I - 1];
                    if (c < '0' || c >= ':')
                    {
                        break;
                    }
                }
                res = (I > str.Length);
            }

            return res;
        }

        public static int IndexOf<T>(T[] array, T value)
        {
            for (int i = 0; i < array.Length; i++)
            {
                if (array[i].Equals(value))
                {
                    return i;
                }
            }
            return -1;
        }

        #region Math helpers

        public static long Trunc(double value)
        {
            return (long)Math.Truncate(value);
        }

        public static double SafeDiv(double dividend, double divisor)
        {
            return (divisor == 0.0d) ? 0.0d : (dividend / divisor);
        }

        public static double DegreesToRadians(double degrees)
        {
            return degrees * (Math.PI / 180);
        }

        public static double RadiansToDegrees(double radians)
        {
            return radians * 180 / Math.PI;
        }

        #endregion

        #region Date functions

        public static int DaysBetween(DateTime now, DateTime then)
        {
            TimeSpan span = then - now;
            return span.Days;
        }

        private static readonly byte[][] MONTH_DAYS = new byte[][]
        {
            new byte[] { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
            new byte[] { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
        };

        public static byte DaysInAMonth(short year, byte month)
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
            #if !CI_MODE
            if (File.Exists(fileName)) {
                Process.Start(new ProcessStartInfo("file://"+fileName) { UseShellExecute = true });
            } else {
                Process.Start(fileName);
            }
            #endif
        }

        public static bool IsRemovableDrive(string filePath)
        {
            string pathRoot = Path.GetPathRoot(filePath);
            var driveInfo = new DriveInfo(pathRoot);
            return (driveInfo.DriveType == DriveType.Removable);
        }

        // Replaces backslashes with slashes
        public static string NormalizeFilename(string fileName)
        {
            string filename = fileName.Replace('\\', '/');
            return filename;
        }

        public static byte[] ReadByteArray(Stream input)
        {
            byte[] buffer = new byte[16*1024];
            using (MemoryStream ms = new MemoryStream())
            {
                int read;
                while ((read = input.Read(buffer, 0, buffer.Length)) > 0)
                {
                    ms.Write(buffer, 0, read);
                }
                return ms.ToArray();
            }
        }

        public static StreamReader OpenStreamReader(Stream src, Encoding defaultEncoding)
        {
            Encoding encodingSource = defaultEncoding;
            bool detectEncoding = false;

            if (src.CanSeek)
            {
                byte[] bPreamble = new byte[4];
                int iReaded = src.Read(bPreamble, 0, 4);

                if (iReaded >= 3 && bPreamble[0] == 0xEF && bPreamble[1] == 0xBB && bPreamble[2] == 0xBF) // utf-8
                    encodingSource = Encoding.UTF8;
                else if (iReaded == 4 && bPreamble[0] == 0x00 && bPreamble[1] == 0x00 && bPreamble[2] == 0xFE && bPreamble[3] == 0xFF) // utf-32 EB
                {
                    encodingSource = Encoding.GetEncoding("utf-32"); // is a EL codepage, but the StreamReader should switch to EB
                    detectEncoding = true;
                }
                else if (iReaded == 4 && bPreamble[0] == 0xFF && bPreamble[1] == 0xFE && bPreamble[2] == 0x00 && bPreamble[3] == 0x00) // utf-32 EL
                    encodingSource = Encoding.GetEncoding("utf-32");
                else if (iReaded >= 2 && bPreamble[0] == 0xFE && bPreamble[1] == 0xFF) // utf-16 EB
                    encodingSource = Encoding.BigEndianUnicode;
                else if (iReaded >= 2 && bPreamble[0] == 0xFF && bPreamble[1] == 0xFE) // utf-16 EL
                    encodingSource = Encoding.Unicode;

                src.Seek(-iReaded, SeekOrigin.Current);
            }
            else
                detectEncoding = true;

            return new StreamReader(src, encodingSource, detectEncoding);
        }

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
            try
            {
                Type t = Type.GetType("Mono.Runtime");
                if (t != null)
                {
                    MethodInfo mi = t.GetMethod("GetDisplayName",
                                                BindingFlags.NonPublic | BindingFlags.Static);
                    if (mi != null)
                    {
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

        #region Linq-pieces

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

        #region Convert helpers

        private static int[] RN_N;
        private static string[] RN_S;

        private static void InitRome()
        {
            RN_N = new int[] { 1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000 };
            RN_S = new string[] { "I", "IV", "V", "IX", "X", "XL", "L", "XC", "C", "CD", "D", "CM", "M" };
        }

        public static string GetRome(int num)
        {
            string rome = "";
            int T = 12;

            if (num > 0)
            {
                while (true)
                {
                    int rn = RN_N[T];

                    if (num >= rn) {
                        while (num >= rn) {
                            num -= rn;
                            rome += RN_S[T];
                        }

                        if (num <= 0) break;
                    } else {
                        T -= 1;
                    }
                }
            }
            return rome;
        }

        public static NumberFormatInfo CreateDefaultNumberFormat()
        {
            var result = new NumberFormatInfo();
            result.NumberDecimalSeparator = ".";
            result.NumberGroupSeparator = "";
            return result;
        }

        public static int ParseInt(string str, int Default)
        {
            int res;
            if (!int.TryParse(str, out res)) res = Default;
            return res;
        }

        public static double ParseFloat(string str, double defaultValue, bool checkSeparator = false)
        {
            if (string.IsNullOrEmpty(str)) return defaultValue;

            string decSep;
            if (checkSeparator) {
                decSep = (str.Contains(",") ? "," : ".");
            } else {
                decSep = ".";
            }

            NumberFormatInfo formatInfo = (NumberFormatInfo)Thread.CurrentThread.CurrentCulture.NumberFormat.Clone();
            formatInfo.NumberDecimalSeparator = decSep;
            formatInfo.NumberGroupSeparator = " ";

            double value;
            double result;
            if (double.TryParse(str, NumberStyles.Float, formatInfo, out value)) {
                result = value;
            } else {
                result = defaultValue;
            }
            return result;
        }

        public static string AdjustNum(int val, int up)
        {
            StringBuilder res = new StringBuilder(up);
            res.Append(val.ToString());
            while (res.Length < up) res.Insert(0, "0");
            return res.ToString();
        }

        public static string NormalizeName(string s)
        {
            if (string.IsNullOrEmpty(s)) return "";

            StringBuilder stb = new StringBuilder(s.Trim().ToLowerInvariant());
            stb[0] = char.ToUpperInvariant(stb[0]);
            return stb.ToString();
        }

        #endregion

        #region CRC32

        private const uint DEFAULT_POLYNOMIAL = 0xedb88320u;
        private static uint[] CCITT32_TABLE;

        private static void InitCRC32()
        {
            CCITT32_TABLE = new uint[256];

            for (uint i = 0; i < 256; i++)
            {
                uint val = i;

                for (uint j = 0; j < 8; j++)
                    if ((val & 1) == 1)
                        val = (val >> 1) ^ DEFAULT_POLYNOMIAL;
                    else
                        val = val >> 1;

                CCITT32_TABLE[i] = val;
            }
        }

        public static uint CrcBytes(byte[] data)
        {
            uint crc = 0u;
            if (data != null && data.Length != 0)
            {
                for (int i = 0; i < data.Length; i++)
                {
                    byte c = data[i];
                    crc = ((crc >> 8 & 16777215u) ^ CCITT32_TABLE[(int)((crc ^ c) & 255u)]);
                }
            }
            return crc;
        }

        public static uint CrcStr(string str)
        {
            uint crc = 0u;
            if (!string.IsNullOrEmpty(str))
            {
                byte[] data = Encoding.Unicode.GetBytes(str);
                crc = CrcBytes(data);
            }
            return crc;
        }

        #endregion

        #region Encoding

        public static string EncodeUID(byte[] binaryKey)
        {
            StringBuilder result = new StringBuilder(36);
            byte checkA = 0;
            byte checkB = 0;

            int num = binaryKey.Length;
            for (int i = 0; i < num; i++)
            {
                byte val = binaryKey[i];
                checkA = unchecked((byte)(checkA + (uint)val));
                checkB = unchecked((byte)(checkB + (uint)checkA));
                result.Append(val.ToString("X2"));
            }

            result.Append(checkA.ToString("X2"));
            result.Append(checkB.ToString("X2"));

            return result.ToString();
        }

        public static string GetRectUID(int x1, int y1, int x2, int y2)
        {
            byte[] bx1 = BitConverter.GetBytes((ushort)x1);
            byte[] by1 = BitConverter.GetBytes((ushort)y1);
            byte[] bx2 = BitConverter.GetBytes((ushort)x2);
            byte[] by2 = BitConverter.GetBytes((ushort)y2);

            byte[] buffer = new byte[8];
            Buffer.BlockCopy(bx1, 0, buffer, 0, 2);
            Buffer.BlockCopy(by1, 0, buffer, 2, 2);
            Buffer.BlockCopy(bx2, 0, buffer, 4, 2);
            Buffer.BlockCopy(by2, 0, buffer, 6, 2);

            return SysUtils.EncodeUID(buffer);
        }

        #endregion

        #region Graphics functions

        public static float ZoomToFit(float imgWidth, float imgHeight,
                                      float requireWidth, float requireHeight)
        {
            if (imgWidth == 0.0f || imgHeight == 0.0f) return 1.0f;

            float aspectRatio;

            if (imgWidth > imgHeight) {
                aspectRatio = requireWidth / imgWidth;

                if (requireHeight < imgHeight * aspectRatio) {
                    aspectRatio = requireHeight / imgHeight;
                }
            } else {
                aspectRatio = requireHeight / imgHeight;

                if (requireWidth < imgWidth * aspectRatio) {
                    aspectRatio = requireWidth / imgWidth;
                }
            }

            return aspectRatio;
        }

        #endregion

        #region Match functions

        public static Regex InitMaskRegex(string mask)
        {
            Regex result = null;

            if (!string.IsNullOrEmpty(mask))
            {
                string regexStr = "";
                int curPos = 0;
                int len = mask.Length;

                while (curPos < len)
                {
                    int I = mask.IndexOfAny("*?".ToCharArray(), curPos);
                    if (I < curPos) break;
                    if (I > curPos) {
                        string part = mask.Substring(curPos, I - curPos);
                        regexStr += Regex.Escape(part);
                    }

                    char c = mask[I];
                    switch (c) {
                        case '*':
                            regexStr += ".*";
                            break;
                        case '?':
                            regexStr += ".";
                            break;
                    }

                    curPos = I + 1;
                }

                if (curPos < len) {
                    string part = mask.Substring(curPos, len - curPos);
                    regexStr += Regex.Escape(part);
                }

                result = new Regex(regexStr, RegexOptions.IgnoreCase);
            }

            return result;
        }

        public static bool MatchesRegex(string str, Regex regex)
        {
            return (regex != null) && regex.IsMatch(str);
        }

        public static bool MatchesMask(string str, string mask)
        {
            Regex regex = InitMaskRegex(mask);
            return MatchesRegex(str, regex);
        }

        #endregion
    }
}
