/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih, Ruslan Garipov.
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
using BSLib;
using BSLib.Design;
using BSLib.Design.Graphics;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using GKUI.Platform;

namespace GKUI.Components
{
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
                winRect.Left = (int)Math.Max(workArea.Left, Math.Min(workArea.Right - width, winRect.Left));
                winRect.Top = (int)Math.Max(workArea.Top, Math.Min(workArea.Bottom - height, winRect.Top));
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
            Rectangle frt = form.Bounds;
            return ExtRect.Create(frt.Left, frt.Top, frt.Right, frt.Bottom);
        }

        public static void RestoreFormRect(Form form, ExtRect rt, Eto.Forms.WindowState winState)
        {
            // check for new and empty struct
            if (form == null || rt.IsEmpty()) return;

            if (winState != Eto.Forms.WindowState.Minimized) {
                form.Location = new Point(rt.Left, rt.Top);
                form.Width = rt.GetWidth();
                form.Height = rt.GetHeight();

                form.WindowState = winState;
            } else {
                form.WindowState = Eto.Forms.WindowState.Maximized;
            }
        }

        public static void CenterFormByParent(Window form, Rectangle parentRect)
        {
            if (form == null) return;

            //form.StartPosition = FormStartPosition.Manual;

            // Center the new window on a monitor, where the parent window
            // is located.
            Screen screen = Screen.FromRectangle(parentRect);
            if (screen != null) {
                var workArea = screen.WorkingArea;

                int fx = (int)workArea.Left + (((int)workArea.Width - form.Width) >> 1);
                int fy = (int)workArea.Top + (((int)workArea.Height - form.Height) >> 1);
                form.Location = new Point(fx, fy);
            }
        }

        public static T GetSelectedTag<T>(this ComboBox comboBox)
        {
            var comboItem = comboBox.SelectedValue as ComboItem<T>;
            T itemTag = (comboItem != null) ? comboItem.Tag : default(T);
            return itemTag;
        }

        public static void SetSelectedTag<T>(this ComboBox comboBox, T tagValue, bool allowDefault = true)
        {
            foreach (object item in comboBox.Items) {
                var comboItem = item as ComboItem<T>;

                if (comboItem != null && object.Equals(comboItem.Tag, tagValue)) {
                    comboBox.SelectedValue = item;
                    return;
                }
            }

            if (allowDefault) {
                comboBox.SelectedIndex = 0;
            }
        }

        public static RadioMenuItem AddToolStripItem(ContextMenu contextMenu, string text, object tag, EventHandler<EventArgs> clickHandler)
        {
            var tsItem = new RadioMenuItem();
            tsItem.Text = text;
            tsItem.Tag = tag;
            tsItem.Click += clickHandler;
            contextMenu.Items.Add(tsItem);
            return tsItem;
        }

        public static T GetMenuItemTag<T>(ContextMenu contextMenu, object sender)
        {
            foreach (RadioMenuItem tsItem in contextMenu.Items) {
                tsItem.Checked = false;
            }
            var senderItem = ((RadioMenuItem)sender);
            ((RadioMenuItem)sender).Checked = true;
            return (T)senderItem.Tag;
        }

        public static void SetMenuItemTag<T>(ContextMenu contextMenu, T value)
        {
            foreach (RadioMenuItem tsItem in contextMenu.Items) {
                T itemTag = (T)tsItem.Tag;
                if (Equals(itemTag, value)) {
                    tsItem.PerformClick();
                    break;
                }
            }
        }

        public static GKListView CreateRecordsView(Panel parent, IBaseContext baseContext, GDMRecordType recType)
        {
            if (parent == null)
                throw new ArgumentNullException("parent");

            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            GKListView recView = new GKListView();
            recView.ListMan = ListManager.Create(baseContext, recType);
            parent.Content = recView;

            return recView;
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
                ctl.BackgroundColor = enabled ? SystemColors.WindowBackground : SystemColors.Control;
            }
        }

        public static void SetClipboardText(string text)
        {
            using (var clipboard = new Clipboard()) {
                clipboard.Text = text;
            }
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

        public static void SetControlFont(Control ctl, Font font)
        {
            var cmCtl = ctl as CommonControl;
            if (cmCtl != null && font != null) {
                cmCtl.Font = font;
            }

            var container = ctl as Container;
            if (container != null) {
                foreach (var child in container.Controls) {
                    SetControlFont(child, font);
                }
            }
        }

        public static void SetControlFont(Control ctl, string family,
                                          float size, FontStyle style = FontStyle.None,
                                          FontDecoration decoration = FontDecoration.None)
        {
            if (ctl != null) {
                Font font = null;
                try {
                    font = new Font(family, size, style, decoration);
                } catch {
                }
                SetControlFont(ctl, font);
            }
        }

        public static Font GetDefaultFont(float size = 8.25f, FontStyle style = FontStyle.None)
        {
            string fontName = AppHost.GfxProvider.GetDefaultFontName();
            return new Font(fontName, size);
        }

        public static string[] Convert(string text)
        {
            var strList = new StringList(text);
            return strList.ToArray();
        }

        public static void ConvertFileDialogFilters(FileDialog fileDlg, string filter)
        {
            if (fileDlg == null)
                throw new ArgumentNullException("fileDlg");

            var filterParts = filter.Split('|');
            int filtersNum = filterParts.Length / 2;
            for (int i = 0; i < filtersNum; i++) {
                int idx = i * 2;
                string name = filterParts[idx];
                string exts = filterParts[idx + 1];
                string[] extensions = exts.Split(',');

                fileDlg.Filters.Add(new FileFilter(name, extensions));
            }
        }

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

            GraphicsPath path = new GraphicsPath();
            path.AddLine(mp1, mp2);
            path.AddLine(mp2, mp3);
            path.AddLine(mp3, mp1);
            gfx.FillPath(fillColor, path);
        }
    }
}
