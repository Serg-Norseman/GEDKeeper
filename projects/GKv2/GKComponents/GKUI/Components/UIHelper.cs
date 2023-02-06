/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih, Ruslan Garipov.
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
using System.Drawing.Drawing2D;
using System.Globalization;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Design.MVP.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;

namespace GKUI.Components
{
    using GKUI.Platform.Handlers;
#if !MONO
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

        public static ExtRect Rt2Rt(Rectangle ert)
        {
            return ExtRect.CreateBounds(ert.Left, ert.Top, ert.Width, ert.Height);
        }

        public static ExtRectF Rt2Rt(RectangleF ert)
        {
            return ExtRectF.CreateBounds(ert.Left, ert.Top, ert.Width, ert.Height);
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
                var workArea = screen.WorkingArea;

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
                var workArea = screen.WorkingArea;

                form.Left = workArea.Left + ((workArea.Width - form.Width) >> 1);
                form.Top = workArea.Top + ((workArea.Height - form.Height) >> 1);
            }
        }

        public static T GetSelectedTag<T>(this ComboBox comboBox)
        {
            var comboItem = comboBox.SelectedItem as ComboItem<T>;
            T itemTag = (comboItem != null) ? comboItem.Tag : default(T);
            return itemTag;
        }

        public static void SetSelectedTag<T>(this ComboBox comboBox, T tagValue, bool allowDefault = true)
        {
            foreach (object item in comboBox.Items) {
                var comboItem = item as ComboItem<T>;

                if (comboItem != null && object.Equals(comboItem.Tag, tagValue)) {
                    comboBox.SelectedItem = item;
                    return;
                }
            }

            if (allowDefault) {
                comboBox.SelectedIndex = 0;
            }
        }

        public static ToolStripMenuItem AddToolStripItem(ContextMenuStrip contextMenu, string text, object tag, EventHandler clickHandler)
        {
            var tsItem = new ToolStripMenuItem(text, null, clickHandler);
            tsItem.Tag = tag;
            contextMenu.Items.Add(tsItem);
            return tsItem;
        }

        public static T GetMenuItemTag<T>(ContextMenuStrip contextMenu, object sender)
        {
            foreach (ToolStripMenuItem tsItem in contextMenu.Items) {
                tsItem.Checked = false;
            }
            var senderItem = ((ToolStripMenuItem)sender);
            ((ToolStripMenuItem)sender).Checked = true;
            return (T)senderItem.Tag;
        }

        public static void SetMenuItemTag<T>(ContextMenuStrip contextMenu, T value)
        {
            foreach (ToolStripMenuItem tsItem in contextMenu.Items) {
                T itemTag = (T)tsItem.Tag;
                if (Equals(itemTag, value)) {
                    tsItem.PerformClick();
                    break;
                }
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
            recView.ListMan = RecordsListModel<GDMRecord>.Create(baseContext, recType);
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
                tb.Text = GKUtils.UniformName(tb.Text);
            }

            ComboBox cmb = (sender as ComboBox);
            if (cmb != null && GlobalOptions.Instance.FirstCapitalLetterInNames) {
                cmb.Text = GKUtils.UniformName(cmb.Text);
            }
        }

        public static void FixToolStrip(ToolStrip toolStrip)
        {
            #if MONO
            // dirty hack: Mono ToolStrip does not support correct AutoSize
            toolStrip.AutoSize = false;
            toolStrip.Height = 27;
            #endif
        }

        #region Application's autorun
        #if !MONO

        public static void RegisterStartup()
        {
            if (!IsStartupItem()) {
                RegistryKey rkApp = GetRunKey();
                if (rkApp != null) {
                    string trayPath = GKUtils.GetAppPath() + "GKTray.exe";
                    rkApp.SetValue(GKData.APP_TITLE, trayPath);
                }
            }
        }

        public static void UnregisterStartup()
        {
            if (IsStartupItem()) {
                RegistryKey rkApp = GetRunKey();
                if (rkApp != null) {
                    rkApp.DeleteValue(GKData.APP_TITLE, false);
                }
            }
        }

        public static bool IsStartupItem()
        {
            RegistryKey rkApp = GetRunKey();
            return (rkApp != null && rkApp.GetValue(GKData.APP_TITLE) != null);
        }

        private static RegistryKey GetRunKey()
        {
            RegistryKey rkApp = Registry.CurrentUser.OpenSubKey("SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Run", true);
            return rkApp;
        }

        #endif
        #endregion

        public static void DrawArrowLine(Graphics gfx, Color fillColor, Pen pen, float x1, float y1, float x2, float y2, int arrLength = 8)
        {
            gfx.DrawLine(pen, x1, y1, x2, y2);

            var m = x2 - x1 == 0 ? 0 : (y2 - y1) / (x2 - x1);
            var degree = Math.Atan(m);
            var toLeft = x2 > x1 ? 0 : Math.PI;

            var degree1 = degree + 5 * Math.PI / 6 + toLeft;
            var degree2 = degree + 7 * Math.PI / 6 + toLeft;

            var px1 = x2 + (float)Math.Cos(degree1) * arrLength;
            var py1 = y2 + (float)Math.Sin(degree1) * arrLength;

            var px2 = x2 + (float)Math.Cos(degree2) * arrLength;
            var py2 = y2 + (float)Math.Sin(degree2) * arrLength;

            var mp1 = new PointF(x2, y2);
            var mp2 = new PointF(px1, py1);
            var mp3 = new PointF(px2, py2);

            using (var brush = new SolidBrush(fillColor)) {
                GraphicsPath path = new GraphicsPath();
                path.AddLine(mp1, mp2);
                path.AddLine(mp2, mp3);
                path.AddLine(mp3, mp1);
                gfx.FillPath(brush, path);
            }
        }

        private const int ExifOrientationTagId = 274;

        public static void NormalizeOrientation(Image image)
        {
            if (image == null || Array.IndexOf(image.PropertyIdList, ExifOrientationTagId) < 0) return;

            int orientation = image.GetPropertyItem(ExifOrientationTagId).Value[0];
            if (orientation >= 1 && orientation <= 8) {
                switch (orientation) {
                    case 2:
                        image.RotateFlip(RotateFlipType.RotateNoneFlipX);
                        break;
                    case 3:
                        image.RotateFlip(RotateFlipType.Rotate180FlipNone);
                        break;
                    case 4:
                        image.RotateFlip(RotateFlipType.Rotate180FlipX);
                        break;
                    case 5:
                        image.RotateFlip(RotateFlipType.Rotate90FlipX);
                        break;
                    case 6:
                        image.RotateFlip(RotateFlipType.Rotate90FlipNone);
                        break;
                    case 7:
                        image.RotateFlip(RotateFlipType.Rotate270FlipX);
                        break;
                    case 8:
                        image.RotateFlip(RotateFlipType.Rotate270FlipNone);
                        break;
                }

                image.RemovePropertyItem(ExifOrientationTagId);
            }
        }

        private static int Blend(int fg, int bg, float fgfactor)
        {
            return (int)(bg + (fg - bg) * fgfactor);
        }

        public static Bitmap CreateSphere(int rad, Color color)
        {
            Bitmap ball = null;

            if (rad > 0) {
                Color[] colors = new Color[101];
                int dd = rad * 2;
                int h = (int)(Math.Round((rad * 0.375f)));

                int rl = color.R;
                int gl = color.G;
                int bl = color.B;

                int[] data = new int[dd * dd];
                int maxr = 0;

                for (int yy = dd; yy >= 0; yy--) {
                    int x0 = (int)(Math.Round(Math.Sqrt(rad * rad - (yy - rad) * (yy - rad))));
                    int p = yy * dd + rad - x0;

                    for (int xx = -x0 + 1; xx <= x0 - 1; xx++) {
                        int x = xx + h;
                        int y = yy - rad + h;
                        int r = (int)Math.Round((Math.Sqrt(x * x + y * y) + 0.5f));
                        if (r > maxr) {
                            maxr = r;
                        }
                        if (r <= 0) {
                            r = 0;
                        }
                        p++;
                        data[p] = r;
                    }
                }

                colors[0] = Color.Transparent;//Color.FromArgb(192, 192, 192);

                for (int i = maxr; i >= 1; i--) {
                    float d = (i / (float)maxr);
                    colors[i] = Color.FromArgb(
                            Blend(Blend(rl, 255, d), 192, 1.0f),
                            Blend(Blend(gl, 255, d), 192, 1.0f),
                            Blend(Blend(bl, 255, d), 192, 1.0f));
                }

                Bitmap ballImage = new Bitmap(dd, dd);

                //var canvas = Graphics.FromImage(ballImage);
                //Color rr = colors[maxr / 2];
                //canvas.FillEllipse(new SolidBrush(rr), 0, 0, dd, dd);

                for (int xx = 0; xx < dd; xx++) {
                    for (int yy = 0; yy < dd; yy++) {
                        ballImage.SetPixel(xx, yy, colors[data[yy * dd + xx]]);
                    }
                }

                ball = ballImage;
            }

            return ball;
        }


        // Converts a string of the form #RRGGBB to a Color instance.
        // Used when retrieving colours from the config.
        public static Color ParseColor(string s)
        {
            if (string.IsNullOrEmpty(s)) {
                return Color.Black;
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

            return Color.FromArgb(r, g, b);
        }
    }
}
