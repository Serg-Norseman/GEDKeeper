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

#if __MonoCS__
#define OLDMETHOD
#endif

using System;
using System.Drawing;
using System.Windows.Forms;

using GKCommon;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public class UIManager
    {
        public static ExtRect GetFormRect(Form form)
        {
            if (form == null) return ExtRect.CreateEmpty();

            #if OLDMETHOD

            int x = form.Left;
            int y = form.Top;
            int w = form.Width;
            int h = form.Height;
            // You must not expect user has a top window located on the primary
            // monitor.
            Screen scr = Screen.PrimaryScreen;
            int mw = scr.WorkingArea.Width;
            int mh = scr.WorkingArea.Height;
            // If a top window ain't on the primary monitor, `x` and `y` may be
            // negative numbers.
            if (x < 0) x = 0;
            if (y < 0) y = 0;
            if (w > mw) w = mw;
            if (h > mh) h = mh;
            return ExtRect.Create(x, y, x + w - 1, y + h - 1);

            #else

            // 2016-09-30 Ruslan Garipov <brigadir15@gmail.com>
            // GK doesn't check size and position of the `form` here anymore.
            // `GetFormRect` is called **before** closing the application, but
            // there's no guarantees that user won't change a monitor settings
            // after that. GK should restrict position of a top window on the
            // application startup. And I'm still not sured GK should constrain
            // a window size (either top one or child).
            // FIXME: If `Control::Left`, `Control::Top`, `Control::Width` and
            // `Control::Height` return physical values (device depended), code
            // here or code that uses the result of `GetFormRect` must convert
            // them to logical values (device independed) before storing it as
            // the application settings. Had GK been a native Windows
            // application, it had to do that. But since it's a .NET application
            // I don't know is it a true.
            return ExtRect.Create(form.Left, form.Top,
                                  form.Left + form.Width - 1, form.Top + form.Height - 1);

            #endif
        }

        public static void RestoreFormRect(Form form, ExtRect rt, FormWindowState winState)
        {
            // check for new and empty struct
            if (form != null && !rt.IsEmpty())
            {
                if (winState != FormWindowState.Minimized) {
                    form.Left = rt.Left;
                    form.Top = rt.Top;
                    form.Width = rt.GetWidth();
                    form.Height = rt.GetHeight();

                    form.WindowState = winState;
                } else {
                    form.WindowState = FormWindowState.Maximized;
                }
            }
        }

        public static void NormalizeFormRect(ref ExtRect winRect)
        {
            #if !OLDMETHOD

            //------------------------------------------------------------------
            // 2016-09-30 Ruslan Garipov <brigadir15@gmail.com>
            // Restrict position and size of the main window.
            // FIXME: DPI-aware code still required here. See also
            // implementation of `GKCore::Options::GlobalOptions::SaveToFile`
            // and `GKCore::GKUtils::GetFormRect` members.
            //------------------------------------------------------------------
            IntPtr user32 = IntPtr.Zero;

            NativeWindowsWorld.RECT rect;
            rect.left = winRect.Left;
            rect.top = winRect.Top;
            rect.right = winRect.Right;
            rect.bottom = winRect.Bottom;

            IntPtr monitor = NativeWindowsWorld.MonitorFromRect(ref user32,
                                                                ref rect, NativeWindowsWorld.MONITOR_DEFAULTTONEAREST);
            if (IntPtr.Zero != monitor)
            {
                NativeWindowsWorld.MONITORINFOEX mi =
                    new NativeWindowsWorld.MONITORINFOEX();
                mi.Init();

                if (0 != NativeWindowsWorld.GetMonitorInfoW(ref user32, monitor,
                                                            ref mi))
                {
                    int width = winRect.GetWidth();
                    int height = winRect.GetHeight();

                    // Besides disallowing to the main window to have its right
                    // and bottom borders overhanged entire virtual workspace,
                    // combined from all available monitors, this code also
                    // does not allow to have this window "between" two
                    // monitors. This may be UNWANTED BEHAVIOR.
                    winRect.Left = Math.Max(mi.work.left, Math.Min(
                        mi.work.right - width, winRect.Left));
                    winRect.Top = Math.Max(mi.work.top, Math.Min(
                        mi.work.bottom - height, winRect.Top));
                    winRect.Right = winRect.Left + width - 1;
                    winRect.Bottom = winRect.Top + height - 1;
                }
            }
            if (IntPtr.Zero != user32)
            {
                NativeWindowsWorld.FreeLibrary(user32);
            }

            #endif
        }

        public static void CenterFormByParent(Form form, IntPtr parent)
        {
            if (form == null) return;

            #if OLDMETHOD

            form.StartPosition = FormStartPosition.CenterScreen;

            #else

            form.StartPosition = FormStartPosition.Manual;
            // Center the new window on a monitor, where the parent window
            // is located.
            IntPtr user32 = IntPtr.Zero;
            IntPtr monitor = NativeWindowsWorld.MonitorFromWindow(
                ref user32, parent,
                NativeWindowsWorld.MONITOR_DEFAULTTONEAREST);
            if (IntPtr.Zero != monitor)
            {
                NativeWindowsWorld.MONITORINFOEX mi =
                    new NativeWindowsWorld.MONITORINFOEX();
                mi.Init();
                if (0 != NativeWindowsWorld.GetMonitorInfoW(ref user32,
                                                            monitor, ref mi))
                {
                    // Yes, here I calculate width and height of a window
                    // using Windows API approach:
                    // dim = coord2 - coord1
                    // This approach differs from GK's one.
                    form.Left = mi.work.left +
                        ((mi.work.right - mi.work.left - form.Width) >> 1);
                    form.Top = mi.work.top +
                        ((mi.work.bottom - mi.work.top - form.Height) >> 1);
                }
            }
            if (IntPtr.Zero != user32)
            {
                NativeWindowsWorld.FreeLibrary(user32);
            }

            #endif
        }
    }

    public class FormWindowStateChangeEventArgs : EventArgs
    {
        public readonly FormWindowState OldState;
        public readonly FormWindowState NewState;

        public FormWindowStateChangeEventArgs(FormWindowState oldState, FormWindowState newState)
        {
            this.OldState = oldState;
            this.NewState = newState;
        }
    }

    /// <summary>
    /// TODO: It's the temporary location of this code. Need later move.
    /// </summary>
    public class ExtForm : Form
    {
        protected override void WndProc(ref Message m)
        {
            FormWindowState prevState = this.WindowState;

            base.WndProc(ref m);

            if (this.WindowState != prevState)
                this.OnFormWindowStateChanged(new FormWindowStateChangeEventArgs(prevState, this.WindowState));
        }

        protected virtual void OnFormWindowStateChanged(FormWindowStateChangeEventArgs e)
        {
            if (e.OldState == FormWindowState.Maximized && e.NewState == FormWindowState.Normal) {
                this.AdjustMdiChild(this);
            }
        }

        /// <summary>
        /// It's a Windows-specific hack for bypass problems with the restoration
        /// of the MdiChild window from maximized state to normal (creates
        /// the dimensions that are included in visible borders of MdiParent
        /// to avoid appear of scrollbars).
        /// </summary>
        /// <param name="mdiChild"></param>
        protected void AdjustMdiChild(Form mdiChild)
        {
            if (mdiChild == null) return;

            Form mdiParent = mdiChild.MdiParent;
            if (mdiParent == null) return;

            MdiClient client = null;
            foreach (Control ctl in mdiParent.Controls) {
                if (ctl is MdiClient) {
                    client = ctl as MdiClient;
                    break;
                }
            }
            if (client == null) return;

            Rectangle clientRect = client.ClientRectangle;

            Rectangle formRect = mdiChild.Bounds;
            formRect.Intersect(clientRect);

            mdiChild.SetBounds(formRect.Left, formRect.Top, formRect.Width, formRect.Height, BoundsSpecified.All);
        }
    }

    ///-------------------------------------------------------------------------
    /// 2016-09-30 Ruslan Garipov <brigadir15@gmail.com>
    /// <summary>
    /// I really don't know how this is called in .NET world, but class
    /// `NativeWindowsWorld` defines "declarations" for some functions from
    /// Windows native API. The functions are used to call Windows functions at
    /// runtime (via `LoadLibrary` + `GetProcAddress`).
    /// </summary>
    /// ------------------------------------------------------------------------
    public static class NativeWindowsWorld
    {
        public const uint ERROR_MOD_NOT_FOUND = 126;
        public const int CCHDEVICENAME = 32;
        public const uint MONITOR_DEFAULTTONULL = 0x00000000;
        public const uint MONITOR_DEFAULTTOPRIMARY = 0x00000001;
        public const uint MONITOR_DEFAULTTONEAREST = 0x00000002;

        [System.Runtime.InteropServices.StructLayout(
            System.Runtime.InteropServices.LayoutKind.Sequential)]
        public struct RECT
        {
            public int left;
            public int top;
            public int right;
            public int bottom;
        };

        [System.Runtime.InteropServices.StructLayout(
            System.Runtime.InteropServices.LayoutKind.Sequential,
            CharSet = System.Runtime.InteropServices.CharSet.Unicode)]
        public struct MONITORINFOEX
        {
            public int size;
            public RECT monitor;
            public RECT work;
            public uint flags;
            [System.Runtime.InteropServices.MarshalAs(
                System.Runtime.InteropServices.UnmanagedType.ByValTStr,
                SizeConst = CCHDEVICENAME)]
            public string device;

            public void Init()
            {
                size = System.Runtime.InteropServices.Marshal.SizeOf(
                    typeof(MONITORINFOEX));
                device = string.Empty;
            }
        }

        [System.Runtime.InteropServices.UnmanagedFunctionPointer(
            System.Runtime.InteropServices.CallingConvention.StdCall)]
        public delegate IntPtr monitor_from_rect_func_t(ref RECT rect,
                                                        uint flags);

        [System.Runtime.InteropServices.UnmanagedFunctionPointer(
            System.Runtime.InteropServices.CallingConvention.StdCall)]
        public delegate IntPtr monitor_from_window_func_t(IntPtr hwnd,
                                                          uint flags);

        [System.Runtime.InteropServices.UnmanagedFunctionPointer(
            System.Runtime.InteropServices.CallingConvention.StdCall,
            CharSet = System.Runtime.InteropServices.CharSet.Unicode)]
        public delegate int get_monitor_info_w_func_t(IntPtr monitor,
                                                      ref MONITORINFOEX mi);

        [System.Runtime.InteropServices.DllImport("kernel32.dll")]
        public static extern int GetModuleHandleEx(uint flags,
                                                   string moduleName, ref IntPtr module);

        [System.Runtime.InteropServices.DllImport("kernel32.dll")]
        public static extern IntPtr LoadLibraryEx(string filename, IntPtr file,
                                                  uint flags);

        [System.Runtime.InteropServices.DllImport("kernel32.dll")]
        public extern static int FreeLibrary(IntPtr module);

        [System.Runtime.InteropServices.DllImport("kernel32.dll")]
        public static extern IntPtr GetProcAddress(IntPtr module,
                                                   string procName);

        [System.Runtime.InteropServices.DllImport("kernel32.dll")]
        public static extern int GetLastError();

        ///---------------------------------------------------------------------
        /// <summary>
        /// Loads the specified function at run-time from the specified module.
        /// When necessary this function loads the specified module.
        /// </summary>
        /// <param name="moduleName">Name of module that contains
        /// `functionName`.</param>
        /// <param name="procName">Function that is implemented in the
        /// `moduleName`.</param>
        /// <param name="module">Handle to the loaded library module. When
        /// caller passes a valid handle the function uses it. Otherwise the
        /// function returns here handle to the found/loaded library, and caller
        /// must use `FreeLibrary` to release it. If caller uses valid `lib`
        /// when call this function the `moduleName` is ignored.</param>
        /// <returns>Pointer to the function.</returns>
        /// <remarks>Since C++ templates can't be ported to C#, this function
        /// returns 'void' pointer and not a pointer to a function.</remarks>
        ///---------------------------------------------------------------------
        public static IntPtr getFunction(string moduleName, string procName,
                                         ref IntPtr module)
        {
            if (IntPtr.Zero == module)
            {
                int temp = GetModuleHandleEx(0, moduleName, ref module);
                if ((0 == temp) && (ERROR_MOD_NOT_FOUND == GetLastError()) &&
                    string.IsNullOrEmpty(moduleName))
                {
                    module = LoadLibraryEx(moduleName, IntPtr.Zero, 0);
                }
            }
            return (IntPtr.Zero != module) ? GetProcAddress(module, procName) :
                IntPtr.Zero;
        }

        ///---------------------------------------------------------------------
        /// <summary>
        /// Retrieves a handle to the display monitor that has the largest area
        /// of intersection with a specified rectangle.
        /// </summary>
        /// <param name="module">Handle to the loaded library module 'user32'.
        /// When caller passes a valid handle the function uses it. Otherwise
        /// the function returns here handle to the found/loaded library, and
        /// caller must use `FreeLibrary` to release it.</param>
        /// <param name="rect">Pointer to a RECT structure that specifies the
        /// rectangle of interest in virtual-screen coordinates.</param>
        /// <param name="flags">Determines the function's return value if the
        /// rectangle does not intersect any display monitor.</param>
        /// <returns>Handle to the display monitor or `nullptr` if the function
        /// has failed.</returns>
        ///---------------------------------------------------------------------
        public static IntPtr MonitorFromRect(ref IntPtr module, ref RECT rect,
                                             uint flags)
        {
            IntPtr p = getFunction("user32.dll", "MonitorFromRect", ref module);
            if (IntPtr.Zero != p)
            {
                // Get ready!..
                monitor_from_rect_func_t monitorFromRect =
                    (monitor_from_rect_func_t) (
                        System.Runtime.InteropServices.Marshal.
                        GetDelegateForFunctionPointer(p,
                                                      typeof(monitor_from_rect_func_t)));
                // Does anyone still like .NET?
                return monitorFromRect(ref rect, flags);
            }
            else
            {
                return IntPtr.Zero;
            }
        }

        ///---------------------------------------------------------------------
        /// <summary>
        /// Retrieves a handle to the display monitor that has the largest area
        /// of intersection with the bounding rectangle of a specified window.
        /// </summary>
        /// <param name="module">Handle to the loaded library module 'user32'.
        /// When caller passes a valid handle the function uses it. Otherwise
        /// the function returns here handle to the found/loaded library, and
        /// caller must use `FreeLibrary` to release it.</param>
        /// <param name="hwnd">A handle to the window of interest.</param>
        /// <param name="flags">Determines the function's return value if the
        /// rectangle does not intersect any display monitor.</param>
        /// <returns>Handle to the display monitor or `nullptr` if the function
        /// has failed.</returns>
        ///---------------------------------------------------------------------
        public static IntPtr MonitorFromWindow(ref IntPtr module, IntPtr hwnd,
                                               uint flags)
        {
            IntPtr p = getFunction("user32.dll", "MonitorFromWindow",
                                   ref module);
            if (IntPtr.Zero != p)
            {
                monitor_from_window_func_t monitorFromWindow =
                    (monitor_from_window_func_t) (
                        System.Runtime.InteropServices.Marshal.
                        GetDelegateForFunctionPointer(p,
                                                      typeof(monitor_from_window_func_t)));
                return monitorFromWindow(hwnd, flags);
            }
            else
            {
                return IntPtr.Zero;
            }
        }

        ///---------------------------------------------------------------------
        /// <summary>
        /// Retrieves information about a display monitor.
        /// </summary>
        /// <param name="monitor">Handle to the display monitor of interest.
        /// </param>
        /// <param name="mi">Pointer to a `MONITORINFOEX` structure that
        /// receives information about the specified display monitor.</param>
        /// <param name="module">Handle to the loaded library module 'user32'.
        /// When caller passes a valid handle the function uses it. Otherwise
        /// the function returns here handle to the found/loaded library, and
        /// caller must use `FreeLibrary` to release it.</param>
        /// <returns>If the function succeeds, the return value is nonzero.
        /// If the function fails, the return value is zero.</returns>
        ///---------------------------------------------------------------------
        public static int GetMonitorInfoW(ref IntPtr module, IntPtr monitor,
                                          ref MONITORINFOEX mi)
        {
            IntPtr p = getFunction("user32.dll", "GetMonitorInfoW", ref module);
            if (IntPtr.Zero != p)
            {
                get_monitor_info_w_func_t getMonitorInfoW =
                    (get_monitor_info_w_func_t) (
                        System.Runtime.InteropServices.Marshal.
                        GetDelegateForFunctionPointer(p,
                                                      typeof(get_monitor_info_w_func_t)));
                return getMonitorInfoW(monitor, ref mi);
            }
            else
            {
                return 0;
            }
        }
    }
}
