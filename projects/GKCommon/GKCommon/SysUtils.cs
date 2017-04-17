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
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Text;
using System.Threading;
using System.Windows.Forms;

namespace GKCommon
{
    #if !__MonoCS__
    using Externals.MapiMail;
    #endif

    #region Native WinAPI

    [SecurityCritical, SuppressUnmanagedCodeSecurity]
    public static class NativeMethods
    {
        public const uint WM_USER = 0x0400;
        public const uint WM_KEEPMODELESS = WM_USER + 111;

        #if !__MonoCS__

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool PostMessage(IntPtr hWnd, uint msg, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool EnableWindow(IntPtr hWnd, [MarshalAs(UnmanagedType.Bool)]bool bEnable);

        #endif
    }

    #endregion

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
            if (!File.Exists(attach)) return;

            #if __MonoCS__

            try
            {
                const string mailto = "'{0}' --subject '{1}' --body '{2}' --attach {3}";
                string args = string.Format(mailto, address, subject, body, attach);

                var proc = new System.Diagnostics.Process();
                proc.EnableRaisingEvents = false;
                proc.StartInfo.FileName = "xdg-email";
                proc.StartInfo.Arguments = args;
                proc.Start();
            }
            catch (Exception ex)
            {
                Logger.LogWrite("SysUtils.SendMail(): " + ex.Message);
            }

            #else

            MapiMailMessage message = new MapiMailMessage(subject, body);
            message.Recipients.Add(address);
            message.Files.Add(attach);
            message.ShowDialog();

            #endif
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
            }
            catch { }

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

        #region Reflection helpers

        private static PropertyInfo GetPropertyInfo(Type type, string propertyName)
        {
            PropertyInfo propInfo;
            do {
                propInfo = type.GetProperty(propertyName, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
                type = type.BaseType;
            } while (propInfo == null && type != null);
            return propInfo;
        }

        public static object GetPropertyValue(object obj, string propertyName)
        {
            if (obj == null)
                throw new ArgumentNullException("obj");

            Type objType = obj.GetType();
            PropertyInfo propInfo = GetPropertyInfo(objType, propertyName);
            if (propInfo == null)
                throw new ArgumentOutOfRangeException("propertyName",
                                                      string.Format("Couldn't find property {0} in type {1}", propertyName, objType.FullName));
            return propInfo.GetValue(obj, null);
        }

        public static void SetPropertyValue(object obj, string propertyName, object val)
        {
            if (obj == null)
                throw new ArgumentNullException("obj");

            Type objType = obj.GetType();
            PropertyInfo propInfo = GetPropertyInfo(objType, propertyName);
            if (propInfo == null)
                throw new ArgumentOutOfRangeException("propertyName",
                                                      string.Format("Couldn't find property {0} in type {1}", propertyName, objType.FullName));
            propInfo.SetValue(obj, val, null);
        }


        public static object GetFieldValue(object obj, string fieldName)
        {
            if (obj == null)
                throw new ArgumentNullException("obj");

            Type objType = obj.GetType();
            FieldInfo fieldInfo = objType.GetField(fieldName, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

            if (fieldInfo == null)
                throw new ArgumentOutOfRangeException("fieldName",
                                                      string.Format("Couldn't find field {0} in type {1}", fieldName, objType.FullName));

            return fieldInfo.GetValue(obj);
        }


        private const BindingFlags AllBindings = BindingFlags.IgnoreCase | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;

        private static FieldInfo FindFieldInfo(Type t, string fieldName)
        {
            foreach (FieldInfo fi in t.GetFields(AllBindings))
            {
                if (fi.Name == fieldName)
                {
                    return fi;
                }
            }

            return t.BaseType != null ? FindFieldInfo(t.BaseType, fieldName) : null;
        }

        public static void RemoveControlStdEventHandlers(Control ctl, string privateEventObj)
        {
            if (ctl == null)
                throw new ArgumentNullException("ctl");

            FieldInfo f1 = FindFieldInfo(ctl.GetType(), privateEventObj);
            if (f1 == null) return;

            object obj = f1.GetValue(ctl);

            PropertyInfo pi = ctl.GetType().GetProperty("Events", BindingFlags.NonPublic | BindingFlags.Instance);
            if (pi == null) return;

            EventHandlerList list = (EventHandlerList)pi.GetValue(ctl, null);
            list.RemoveHandler(obj, list[obj]);
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

        public static int ParseInt(string str, int Default)
        {
            int res;
            if (!int.TryParse(str, out res)) res = Default;
            return res;
        }

        public static double ParseFloat(string str, double Default, bool checkSeparator = false)
        {
            if (string.IsNullOrEmpty(str)) return Default;

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
                result = Default;
            }
            return result;
        }

        public static double ParseFloat(string value, double defaultValue)
        {
            double result;
            // Try parsing in the current culture
            if (!double.TryParse(value, System.Globalization.NumberStyles.Any, CultureInfo.CurrentCulture, out result) &&
                // Then in neutral language
                !double.TryParse(value, System.Globalization.NumberStyles.Any, CultureInfo.InvariantCulture, out result))
            {
                result = defaultValue;
            }
            return result;
        }

        public static string AdjustNum(int val, int up)
        {
            StringBuilder res = new StringBuilder(up);
            res.Append(val.ToString());
            while (res.Length < up) res.Insert(0, '0');
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

        #region Sort helpers

        public static void QuickSort<T>(IList<T> list, Comparison<T> comparer)
        {
            if (list == null)
                throw new ArgumentNullException("list");

            if (list.Count > 1)
                QuickSort(list, comparer, 0, list.Count - 1);
        }

        private static void QuickSort<T>(IList<T> list, Comparison<T> comparer, int left, int right)
        {
            int I;
            do
            {
                I = left;
                int J = right;
                T itm = list[(int)((uint)(left + right) >> 1)];
                while (true)
                {
                    if (comparer(list[I], itm) >= 0)
                    {
                        while (comparer(list[J], itm) > 0) J--;

                        if (I <= J)
                        {
                            T tmp = list[I];
                            list[I] = list[J];
                            list[J] = tmp;

                            I++;
                            J--;
                        }

                        if (I > J)
                        {
                            break;
                        }
                    }
                    else
                    {
                        I++;
                    }
                }
                if (left < J) QuickSort(list, comparer, left, J);
                left = I;
            }
            while (I < right);
        }

        public static void MergeSort<T>(IList<T> list, Comparison<T> comparer)
        {
            if (list == null)
                throw new ArgumentNullException("list");

            if (list.Count > 1)
                MergeSort(list, new T[list.Count], 0, list.Count - 1, comparer);
        }

        private static void MergeSort<T>(IList<T> list, T[] tmp, int left, int right, Comparison<T> comparer)
        {
            if (left >= right) return;

            int mid = (left + right) >> 1;
            MergeSort(list, tmp, left, mid, comparer);
            MergeSort(list, tmp, mid + 1, right, comparer);

            int i = left, j = mid + 1, k = left;

            while (i <= mid && j <= right)
            {
                if (comparer(list[i], list[j]) < 0)
                {
                    tmp[k++] = list[i++];
                }
                else
                {
                    tmp[k++] = list[j++];
                }
            }
            while (i <= mid) tmp[k++] = list[i++];
            while (j <= right) tmp[k++] = list[j++];
            for (i = left; i <= right; ++i) list[i] = tmp[i];
        }

        #endregion

        #region Color transformations

        public static Color Darker(Color color, float fraction)
        {
            float factor = (1.0f - fraction);

            int rgb = color.ToArgb();
            int red = (rgb >> 16) & 0xFF;
            int green = (rgb >> 8) & 0xFF;
            int blue = (rgb >> 0) & 0xFF;
            //int alpha = (rgb >> 24) & 0xFF;

            red = (int) (red * factor);
            green = (int) (green * factor);
            blue = (int) (blue * factor);

            red = (red < 0) ? 0 : red;
            green = (green < 0) ? 0 : green;
            blue = (blue < 0) ? 0 : blue;

            return Color.FromArgb(red, green, blue);
        }

        public static Color Lighter(Color color, float fraction)
        {
            float factor = (1.0f + fraction);
            
            int rgb = color.ToArgb();
            int red = (rgb >> 16) & 0xFF;
            int green = (rgb >> 8) & 0xFF;
            int blue = (rgb >> 0) & 0xFF;
            //int alpha = (rgb >> 24) & 0xFF;

            red = (int) (red * factor);
            green = (int) (green * factor);
            blue = (int) (blue * factor);

            if (red < 0) {
                red = 0;
            } else if (red > 255) {
                red = 255;
            }
            if (green < 0) {
                green = 0;
            } else if (green > 255) {
                green = 255;
            }
            if (blue < 0) {
                blue = 0;
            } else if (blue > 255) {
                blue = 255;
            }

            //int alpha = color.getAlpha();

            return Color.FromArgb(red, green, blue);
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

        public static GraphicsPath CreateRectangle(float x, float y, float width, float height)
        {
            float xw = x + width;
            float yh = y + height;

            GraphicsPath p = new GraphicsPath();
            p.StartFigure();

            p.AddLine(x, y, xw, y); // Top Edge
            p.AddLine(xw, y, xw, yh); // Right Edge
            p.AddLine(xw, yh, x, yh); // Bottom Edge
            p.AddLine(x, yh, x, y); // Left Edge

            p.CloseFigure();
            return p;
        }

        public static GraphicsPath CreateRoundedRectangle(float x, float y, float width, float height, float radius)
        {
            float xw = x + width;
            float yh = y + height;
            float xwr = xw - radius;
            float yhr = yh - radius;
            float xr = x + radius;
            float yr = y + radius;
            float r2 = radius * 2;
            float xwr2 = xw - r2;
            float yhr2 = yh - r2;

            GraphicsPath p = new GraphicsPath();
            p.StartFigure();

            p.AddArc(x, y, r2, r2, 180, 90); // Top Left Corner
            p.AddLine(xr, y, xwr, y); // Top Edge
            p.AddArc(xwr2, y, r2, r2, 270, 90); // Top Right Corner
            p.AddLine(xw, yr, xw, yhr); // Right Edge
            p.AddArc(xwr2, yhr2, r2, r2, 0, 90); // Bottom Right Corner
            p.AddLine(xwr, yh, xr, yh); // Bottom Edge
            p.AddArc(x, yhr2, r2, r2, 90, 90); // Bottom Left Corner
            p.AddLine(x, yhr, x, yr); // Left Edge

            p.CloseFigure();
            return p;
        }

        public static void DrawPathWithFuzzyLine(Graphics gfx, GraphicsPath path, Color baseColor, int maxOpacity, int width, int opaqueWidth)
        {
            if (gfx == null || path == null) return;

            int numSteps = width - opaqueWidth + 1; // Number of pens we will use
            float delta = (float)maxOpacity / numSteps / numSteps; // Change in alpha between pens
            float alpha = delta; // Initial alpha

            for (int thickness = width; thickness >= opaqueWidth; thickness--)
            {
                Color color = Color.FromArgb((int)alpha, baseColor.R, baseColor.G, baseColor.B);

                using (Pen pen = new Pen(color, thickness))
                {
                    pen.EndCap = LineCap.Round;
                    pen.StartCap = LineCap.Round;
                    gfx.DrawPath(pen, path);
                }

                alpha += delta;
            }
        }

        public enum Direction {
            Up,
            Right,
            Down,
            Left
        }

        public static void DrawTriangle(Graphics gfx, Rectangle rect, Direction direction)
        {
            int halfWidth = rect.Width / 2;
            int halfHeight = rect.Height / 2;
            Point p0 = Point.Empty;
            Point p1 = Point.Empty;
            Point p2 = Point.Empty;

            switch (direction)
            {
                case Direction.Up:
                    p0 = new Point(rect.Left + halfWidth, rect.Top);
                    p1 = new Point(rect.Left, rect.Bottom);
                    p2 = new Point(rect.Right, rect.Bottom);
                    break;
                case Direction.Down:
                    p0 = new Point(rect.Left + halfWidth, rect.Bottom);
                    p1 = new Point(rect.Left, rect.Top);
                    p2 = new Point(rect.Right, rect.Top);
                    break;
                case Direction.Left:
                    p0 = new Point(rect.Left, rect.Top + halfHeight);
                    p1 = new Point(rect.Right, rect.Top);
                    p2 = new Point(rect.Right, rect.Bottom);
                    break;
                case Direction.Right:
                    p0 = new Point(rect.Right, rect.Top + halfHeight);
                    p1 = new Point(rect.Left, rect.Bottom);
                    p2 = new Point(rect.Left, rect.Top);
                    break;
            }

            gfx.FillPolygon(Brushes.Black, new Point[] { p0, p1, p2 });
        }

        #endregion
    }
}
