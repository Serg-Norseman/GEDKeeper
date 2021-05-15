﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih, Ruslan Garipov.
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
using System.Drawing;
using System.Windows.Forms;
using BSLib;
using BSLib.Design;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;

namespace GKUI.Components
{
    #if !__MonoCS__
    using Microsoft.Win32;
    #endif

    /// <summary>
    /// Static functions only for UI implementation.
    /// </summary>
    public static class UIHelper
    {
        public static Rectangle Rt2Rt(ExtRect ert)
        {
            return new Rectangle(ert.Left, ert.Top, ert.GetWidth(), ert.GetHeight());
        }

        public static RectangleF Rt2Rt(ExtRectF ert)
        {
            return new RectangleF(ert.Left, ert.Top, ert.GetWidth(), ert.GetHeight());
        }

        public static ExtRect NormalizeFormRect(ExtRect winRect)
        {
            // Travis CI does not have access to UI and tests aren't performed.
            #if !CI_MODE

            //------------------------------------------------------------------
            // 2016-09-30 Ruslan Garipov <brigadir15@gmail.com>
            // Restrict position and size of the main window.
            // FIXME: DPI-aware code still required here.
            //------------------------------------------------------------------

            Screen screen = Screen.FromRectangle(Rt2Rt(winRect));
            if (screen != null) {
                Rectangle workArea = screen.WorkingArea;

                int width = winRect.GetWidth();
                int height = winRect.GetHeight();

                // Besides disallowing to the main window to have its right
                // and bottom borders overhanged entire virtual workspace,
                // combined from all available monitors, this code also
                // does not allow to have this window "between" two
                // monitors. This may be UNWANTED BEHAVIOR.
                winRect.Left = Math.Max(workArea.Left, Math.Min(workArea.Right - width, winRect.Left));
                winRect.Top = Math.Max(workArea.Top, Math.Min(workArea.Bottom - height, winRect.Top));
                winRect.Right = winRect.Left + width - 1;
                winRect.Bottom = winRect.Top + height - 1;
            }

            #endif

            return winRect;
        }

        public static ExtRect GetFormRect(Form form)
        {
            if (form == null) return ExtRect.CreateEmpty();

            // You must not expect user has a top window located on the primary
            // monitor. If a top window ain't on the primary monitor,
            // `x` and `y` may be negative numbers.

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
            return ExtRect.Create(form.Left, form.Top, form.Right, form.Bottom);
        }

        public static void RestoreFormRect(Form form, ExtRect rt, FormWindowState winState)
        {
            // check for new and empty struct
            if (form == null || rt.IsEmpty()) return;

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

        public static void CenterFormByParent(Form form, IntPtr parent)
        {
            if (form == null) return;

            form.StartPosition = FormStartPosition.Manual;

            // Center the new window on a monitor, where the parent window
            // is located.
            Screen screen = Screen.FromHandle(parent);
            if (screen != null) {
                Rectangle workArea = screen.WorkingArea;

                form.Left = workArea.Left + ((workArea.Width - form.Width) >> 1);
                form.Top = workArea.Top + ((workArea.Height - form.Height) >> 1);
            }
        }

        public static T GetSelectedTag<T>(ComboBox comboBox)
        {
            GKComboItem<T> comboItem = comboBox.SelectedItem as GKComboItem<T>;
            T itemTag = (comboItem != null) ? comboItem.Tag : default(T);
            return itemTag;
        }

        public static void SetSelectedTag<T>(ComboBox comboBox, T tagValue, bool allowDefault = true)
        {
            foreach (object item in comboBox.Items) {
                GKComboItem<T> comboItem = item as GKComboItem<T>;

                if (comboItem != null && object.Equals(comboItem.Tag, tagValue)) {
                    comboBox.SelectedItem = item;
                    return;
                }
            }

            if (allowDefault) {
                comboBox.SelectedIndex = 0;
            }
        }

        public static GKListView CreateRecordsView(Control parent, IBaseContext baseContext, GDMRecordType recType)
        {
            if (parent == null)
                throw new ArgumentNullException("parent");

            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            GKListView recView = new GKListView();
            recView.HideSelection = false;
            recView.LabelEdit = false;
            recView.FullRowSelect = true;
            recView.View = View.Details;
            recView.ListMan = ListManager.Create(baseContext, recType);
            recView.Dock = DockStyle.Fill;

            parent.Controls.Add(recView);
            parent.Controls.SetChildIndex(recView, 0);

            return recView;
        }

        public static GKListView CreateListView(Control parent)
        {
            if (parent == null)
                throw new ArgumentNullException("parent");

            GKListView listView = new GKListView();
            listView.Dock = DockStyle.Fill;
            parent.Controls.Add(listView);

            return listView;
        }

        public static IColor ConvertColor(Color color)
        {
            return new ColorHandler(color);
        }

        public static Color ConvertColor(IColor color)
        {
            return ((ColorHandler)color).Handle;
        }

        public static Color Darker(Color color, float fraction)
        {
            int rgb = color.ToArgb();
            return Color.FromArgb(GfxHelper.Darker(rgb, fraction));
        }

        public static Color Lighter(Color color, float fraction)
        {
            int rgb = color.ToArgb();
            return Color.FromArgb(GfxHelper.Lighter(rgb, fraction));
        }

        public static Bitmap LoadResourceImage(string resName)
        {
            return new Bitmap(GKUtils.LoadResourceStream(resName));
        }

        public static void SetControlEnabled(Control ctl, bool enabled)
        {
            if (ctl != null) {
                ctl.Enabled = enabled;
                ctl.BackColor = enabled ? SystemColors.Window : SystemColors.Control;
            }
        }

        public static void SetClipboardText(string text)
        {
            Clipboard.SetDataObject(text);
        }

        public static void ProcessName(object sender)
        {
            TextBox tb = (sender as TextBox);
            if (tb != null && GlobalOptions.Instance.FirstCapitalLetterInNames) {
                tb.Text = ConvertHelper.UniformName(tb.Text);
            }

            ComboBox cmb = (sender as ComboBox);
            if (cmb != null && GlobalOptions.Instance.FirstCapitalLetterInNames) {
                cmb.Text = ConvertHelper.UniformName(cmb.Text);
            }
        }

        #region Application's autorun
        #if !__MonoCS__

        public static void RegisterStartup()
        {
            if (!IsStartupItem()) {
                RegistryKey rkApp = GetRunKey();
                string trayPath = GKUtils.GetAppPath() + "GKTray.exe";
                rkApp.SetValue(GKData.APP_TITLE, trayPath);
            }
        }

        public static void UnregisterStartup()
        {
            if (IsStartupItem()) {
                RegistryKey rkApp = GetRunKey();
                rkApp.DeleteValue(GKData.APP_TITLE, false);
            }
        }

        public static bool IsStartupItem()
        {
            RegistryKey rkApp = GetRunKey();
            return (rkApp.GetValue(GKData.APP_TITLE) != null);
        }

        private static RegistryKey GetRunKey()
        {
            RegistryKey rkApp = Registry.CurrentUser.OpenSubKey("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run", true);
            return rkApp;
        }

        #endif
        #endregion
    }
}
