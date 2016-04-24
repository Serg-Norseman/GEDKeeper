/*
  KeePass Password Safe - The Open-Source Password Manager
  Copyright (C) 2003-2016 Dominik Reichl <dominik.reichl@t-online.de>

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Windows.Forms;

namespace GKCommon
{
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

    [Flags]
    public enum AppRunFlags
    {
        None = 0,
        GetStdOutput = 1,
        WaitForExit = 2,

        // https://sourceforge.net/p/keepass/patches/84/
        /// <summary>
        /// This flag prevents any handles being garbage-collected
        /// before the started process has terminated, without
        /// blocking the current thread.
        /// </summary>
        GCKeepAlive = 4,

        // https://sourceforge.net/p/keepass/patches/85/
        DoEvents = 8,
        DisableForms = 16
    }

    public static class SysInfo
    {
        private static ulong? m_ouMonoVersion = null;
        public static ulong MonoVersion
        {
            get
            {
                if(m_ouMonoVersion.HasValue) return m_ouMonoVersion.Value;

                ulong uVersion = 0;
                try
                {
                    Type t = Type.GetType("Mono.Runtime");
                    if(t != null)
                    {
                        MethodInfo mi = t.GetMethod("GetDisplayName",
                                                    BindingFlags.NonPublic | BindingFlags.Static);
                        if(mi != null)
                        {
                            string strName = (mi.Invoke(null, null) as string);
                            if(!string.IsNullOrEmpty(strName))
                            {
                                Match m = Regex.Match(strName, "\\d+(\\.\\d+)+");
                                if(m.Success)
                                    uVersion = SysInfo.ParseVersion(m.Value);
                                else { Debug.Assert(false); }
                            }
                            else { Debug.Assert(false); }
                        }
                        else { Debug.Assert(false); }
                    }
                }
                catch(Exception) { Debug.Assert(false); }

                m_ouMonoVersion = uVersion;
                return uVersion;
            }
        }

        private static bool? m_bIsUnix = null;
        public static bool IsUnix()
        {
            if (m_bIsUnix.HasValue) return m_bIsUnix.Value;

            PlatformID p = GetPlatformID();

            // Mono defines Unix as 128 in early .NET versions
            m_bIsUnix = ((p == PlatformID.Unix) || (p == PlatformID.MacOSX) || ((int)p == 128));

            return m_bIsUnix.Value;
        }

        private static PlatformID? m_platID = null;
        public static PlatformID GetPlatformID()
        {
            if (m_platID.HasValue) return m_platID.Value;

            m_platID = Environment.OSVersion.Platform;

            // Mono returns PlatformID.Unix on Mac OS X, workaround this
            if (m_platID.Value == PlatformID.Unix)
            {
                if ((RunConsoleApp("uname", null) ?? string.Empty).Trim().Equals("Darwin", StringComparison.OrdinalIgnoreCase))
                    m_platID = PlatformID.MacOSX;
            }

            return m_platID.Value;
        }

        public static string RunConsoleApp(string strAppPath, string strParams)
        {
            return RunConsoleApp(strAppPath, strParams, null);
        }

        public static string RunConsoleApp(string strAppPath, string strParams,
                                           string strStdInput)
        {
            return RunConsoleApp(strAppPath, strParams, strStdInput,
                                 (AppRunFlags.GetStdOutput | AppRunFlags.WaitForExit));
        }

        private delegate string RunProcessDelegate();

        public static string RunConsoleApp(string strAppPath, string strParams,
                                           string strStdInput, AppRunFlags f)
        {
            if(strAppPath == null) throw new ArgumentNullException("strAppPath");
            if(strAppPath.Length == 0) throw new ArgumentException("strAppPath");

            bool bStdOut = ((f & AppRunFlags.GetStdOutput) != AppRunFlags.None);

            RunProcessDelegate fnRun = delegate()
            {
                try
                {
                    ProcessStartInfo psi = new ProcessStartInfo();

                    psi.CreateNoWindow = true;
                    psi.FileName = strAppPath;
                    psi.WindowStyle = ProcessWindowStyle.Hidden;
                    psi.UseShellExecute = false;
                    psi.RedirectStandardOutput = bStdOut;

                    if(strStdInput != null) psi.RedirectStandardInput = true;

                    if(!string.IsNullOrEmpty(strParams)) psi.Arguments = strParams;

                    Process p = Process.Start(psi);

                    if(strStdInput != null)
                    {
                        EnsureNoBom(p.StandardInput);

                        p.StandardInput.Write(strStdInput);
                        p.StandardInput.Close();
                    }

                    string strOutput = string.Empty;
                    if(bStdOut) strOutput = p.StandardOutput.ReadToEnd();

                    if((f & AppRunFlags.WaitForExit) != AppRunFlags.None)
                        p.WaitForExit();
                    else if((f & AppRunFlags.GCKeepAlive) != AppRunFlags.None)
                    {
                        Thread th = new Thread(delegate()
                                               {
                                                   try { p.WaitForExit(); }
                                                   catch(Exception) { Debug.Assert(false); }
                                               });
                        th.Start();
                    }

                    return strOutput;
                }
                catch(Exception) { Debug.Assert(false); }

                return null;
            };

            if((f & AppRunFlags.DoEvents) != AppRunFlags.None)
            {
                List<Form> lDisabledForms = new List<Form>();
                if((f & AppRunFlags.DisableForms) != AppRunFlags.None)
                {
                    foreach(Form form in Application.OpenForms)
                    {
                        if(!form.Enabled) continue;

                        lDisabledForms.Add(form);
                        form.Enabled = false;
                    }
                }

                IAsyncResult ar = fnRun.BeginInvoke(null, null);

                while(!ar.AsyncWaitHandle.WaitOne(0))
                {
                    Application.DoEvents();
                    Thread.Sleep(2);
                }

                string strRet = fnRun.EndInvoke(ar);

                for(int i = lDisabledForms.Count - 1; i >= 0; --i)
                    lDisabledForms[i].Enabled = true;

                return strRet;
            }

            return fnRun();
        }

        private static void EnsureNoBom(StreamWriter sw)
        {
            if (sw == null) { Debug.Assert(false); return; }
            //if(!MonoWorkarounds.IsRequired(1219)) return;

            try
            {
                Encoding enc = sw.Encoding;
                if(enc == null) { Debug.Assert(false); return; }
                byte[] pbBom = enc.GetPreamble();
                if((pbBom == null) || (pbBom.Length == 0)) return;

                // For Mono >= 4.0 (using Microsoft's reference source)
                try
                {
                    FieldInfo fi = typeof(StreamWriter).GetField("haveWrittenPreamble",
                                                                 BindingFlags.Instance | BindingFlags.NonPublic);
                    if(fi != null)
                    {
                        fi.SetValue(sw, true);
                        return;
                    }
                }
                catch(Exception) { Debug.Assert(false); }

                // For Mono < 4.0
                FieldInfo fiPD = typeof(StreamWriter).GetField("preamble_done",
                                                               BindingFlags.Instance | BindingFlags.NonPublic);
                if(fiPD != null) fiPD.SetValue(sw, true);
                else { Debug.Assert(false); }
            }
            catch(Exception) { Debug.Assert(false); }
        }

        private static DesktopType? m_tDesktop = null;
        public static DesktopType GetDesktopType()
        {
            if (!m_tDesktop.HasValue)
            {
                DesktopType t = DesktopType.None;
                if (!IsUnix()) t = DesktopType.Windows;
                else
                {
                    try
                    {
                        string strXdg = (Environment.GetEnvironmentVariable("XDG_CURRENT_DESKTOP") ?? string.Empty).Trim();
                        string strGdm = (Environment.GetEnvironmentVariable("GDMSESSION") ?? string.Empty).Trim();
                        StringComparison sc = StringComparison.OrdinalIgnoreCase;

                        if(strXdg.Equals("Unity", sc))
                            t = DesktopType.Unity;
                        else if(strXdg.Equals("LXDE", sc))
                            t = DesktopType.Lxde;
                        else if(strXdg.Equals("XFCE", sc))
                            t = DesktopType.Xfce;
                        else if(strXdg.Equals("MATE", sc))
                            t = DesktopType.Mate;
                        else if(strXdg.Equals("X-Cinnamon", sc))
                            t = DesktopType.Cinnamon;
                        else if(strXdg.Equals("Pantheon", sc)) // Elementary OS
                            t = DesktopType.Pantheon;
                        else if(strXdg.Equals("KDE", sc) || // Mint 16
                                strGdm.Equals("kde-plasma", sc)) // Ubuntu 12.04
                            t = DesktopType.Kde;
                        else if(strXdg.Equals("GNOME", sc))
                        {
                            if(strGdm.Equals("cinnamon", sc)) // Mint 13
                                t = DesktopType.Cinnamon;
                            else t = DesktopType.Gnome;
                        }
                    }
                    catch(Exception) { Debug.Assert(false); }
                }

                m_tDesktop = t;
            }

            return m_tDesktop.Value;
        }

        private static readonly char[] m_vVersionSep = new char[]{ '.', ',' };
        public static ulong ParseVersion(string strVersion)
        {
            if (strVersion == null) { Debug.Assert(false); return 0; }

            string[] vVer = strVersion.Split(m_vVersionSep);
            if((vVer == null) || (vVer.Length == 0)) { Debug.Assert(false); return 0; }

            ushort uPart;
            SysInfo.TryParseUShort(vVer[0].Trim(), out uPart);
            ulong uVer = ((ulong)uPart << 48);

            if(vVer.Length >= 2)
            {
                SysInfo.TryParseUShort(vVer[1].Trim(), out uPart);
                uVer |= ((ulong)uPart << 32);
            }

            if(vVer.Length >= 3)
            {
                SysInfo.TryParseUShort(vVer[2].Trim(), out uPart);
                uVer |= ((ulong)uPart << 16);
            }

            if(vVer.Length >= 4)
            {
                SysInfo.TryParseUShort(vVer[3].Trim(), out uPart);
                uVer |= (ulong)uPart;
            }

            return uVer;
        }

        public static bool TryParseUShort(string str, out ushort u)
        {
            try {
                u = ushort.Parse(str);
                return true;
            } catch (Exception) {
                u = 0;
                return false;
            }
        }
    }
}
