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
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security;
using System.Windows.Forms;

using GKCommon;

namespace GKCore
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

        public static long Trunc(double value)
        {
            return (long)Math.Truncate(value);
        }

        public static double SafeDiv(double dividend, double divisor)
        {
            return (divisor == (double)0f) ? 0.0 : (dividend / divisor);
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

        public static void SendMail(string address, string subject, string body, string attach)
        {
            if (File.Exists(attach)) {
                #if __MonoCS__

                string mailto = string.Format("mailto:{0}?Subject={1}&Body={2}&Attach={3}", address, subject, body, "" + attach + ""); // Attach, Attachment
                Process.Start(mailto);

                #else

                MapiMailMessage message = new MapiMailMessage(subject, body);
                message.Recipients.Add(GKData.APP_MAIL);
                message.Files.Add(attach);
                message.ShowDialog();

                #endif
            }
        }

        public static bool IsNetworkAvailable()
        {
            return System.Net.NetworkInformation.NetworkInterface.GetIsNetworkAvailable();
        }

        public static T GetAssemblyAttribute<T>(Assembly assembly) where T : Attribute
        {
            if (assembly == null)
            {
                throw new ArgumentNullException("assembly");
            }

            object[] attributes = assembly.GetCustomAttributes(typeof(T), false);
            if (attributes == null || attributes.Length == 0)
                return null;
            return LinqHelper.SingleOrDefault(LinqHelper.OfTypeIterator((T[])attributes));
        }

        public static void GetAssemblyVersion(out string copyright, out string version)
        {
            copyright = "";
            version = "";

            Assembly assembly = Assembly.GetExecutingAssembly();

            object[] attributes = assembly.GetCustomAttributes(typeof(AssemblyCopyrightAttribute), false);
            if (attributes.Length != 0) copyright = ((AssemblyCopyrightAttribute)attributes[0]).Copyright;

            version = assembly.GetName().Version.ToString();
        }

        public const uint WM_USER 			= 0x0400;
        public const uint WM_KEEPMODELESS 	= WM_USER + 111;

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool PostMessage(IntPtr hWnd, uint msg, IntPtr wParam, IntPtr lParam);

        [DllImport("user32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        public static extern bool EnableWindow(IntPtr hWnd, [MarshalAs(UnmanagedType.Bool)]bool bEnable);
    }
}
