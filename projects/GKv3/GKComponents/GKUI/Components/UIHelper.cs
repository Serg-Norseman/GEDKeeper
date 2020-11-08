/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih, Ruslan Garipov.
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
using BSLib.Design.Graphics;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;

namespace GKUI.Components
{
    public class DefStackLayout : StackLayout
    {
        public DefStackLayout(Orientation orientation) : this(10, 10, orientation)
        {
        }

        public DefStackLayout(int padding, int spacing, Orientation orientation)
        {
            Orientation = orientation;
            Padding = new Padding(padding);
            Spacing = spacing;
        }

        public DefStackLayout(Orientation orientation, int spacing, params Control[] items)
        {
            Orientation = orientation;
            Padding = new Padding(0);
            Spacing = spacing;
            foreach (var item in items) Items.Add(item);
        }

        public DefStackLayout(params Control[] items) : this(Orientation.Vertical, 0, items)
        {
        }
    }

    public class DefTableLayout : TableLayout
    {
        public DefTableLayout()
        {
            Padding = new Padding(10);
            Spacing = new Size(10, 10);
        }

        public DefTableLayout(int columns, int rows) : base(columns, rows)
        {
            Padding = new Padding(10);
            Spacing = new Size(10, 10);
        }
    }

    /// <summary>
    /// Static functions only for UI implementation.
    /// </summary>
    public static class UIHelper
    {
        public static readonly Size ShortButtonSize = new Size(24, 24);
        public static readonly Size LongButtonSize = new Size(120, 24);


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
                RectangleF workArea = screen.WorkingArea;

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

        public static void CenterFormByParent(Form form, Rectangle parentRect)
        {
            if (form == null) return;

            //form.StartPosition = FormStartPosition.Manual;

            // Center the new window on a monitor, where the parent window
            // is located.
            Screen screen = Screen.FromRectangle(parentRect);
            if (screen != null) {
                RectangleF workArea = screen.WorkingArea;

                int fx = (int)workArea.Left + (((int)workArea.Width - form.Width) >> 1);
                int fy = (int)workArea.Top + (((int)workArea.Height - form.Height) >> 1);
                form.Location = new Point(fx, fy);
            }
        }

        public static T GetSelectedTag<T>(ComboBox comboBox)
        {
            GKComboItem<T> comboItem = (GKComboItem<T>)comboBox.SelectedValue;
            T itemTag = (T)comboItem.Tag;
            return itemTag;
        }

        public static void SetSelectedTag<T>(ComboBox comboBox, T tagValue, bool allowDefault = true)
        {
            foreach (object item in comboBox.Items) {
                GKComboItem<T> comboItem = (GKComboItem<T>)item;
                T itemTag = (T)comboItem.Tag;

                if (object.Equals(itemTag, tagValue)) {
                    comboBox.SelectedValue = item;
                    return;
                }
            }

            if (allowDefault) {
                comboBox.SelectedIndex = 0;
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

        public static GKListView CreateListView(Panel parent)
        {
            if (parent == null)
                throw new ArgumentNullException("parent");

            GKListView listView = new GKListView();
            parent.Content = listView;

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

        public static Bitmap LoadResourceImage(Type baseType, string resName)
        {
            return new Bitmap(GKUtils.LoadResourceStream(baseType, resName));
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

        private const bool USE_CLIENT_SIZE_PRESET = false;

        public static void SetPredefProperties(Window window, int width, int height, bool fontPreset = true)
        {
            SetPredefProperties(window, width, height, USE_CLIENT_SIZE_PRESET, fontPreset);
        }

        public static void SetPredefProperties(Window window, int width, int height, bool useClientSizePreset, bool fontPreset)
        {
            if (useClientSizePreset) {
                window.ClientSize = new Size(width, height);
            }

            if (fontPreset) {
                UIHelper.SetControlFont(window, GetDefaultFont());
            }
        }

        public static string[] Convert(string text)
        {
            var strList = new StringList(text);
            return strList.ToArray();
        }

        // FIXME: replace to TableLayout.Horizontal(), same
        public static TableRow MakeDialogFooter(params TableCell[] cells)
        {
            var row = new TableRow();
            foreach (var cell in cells) row.Cells.Add(cell);

            return new TableRow {
                ScaleHeight = false,
                Cells = {
                    new TableLayout {
                        Padding = 0,
                        Spacing = new Size(10, 10),
                        Rows = {
                            row
                        }
                    }
                }
            };
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

                fileDlg.Filters.Add(new FileDialogFilter(name, extensions));
            }
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
                tb.Text = ConvertHelper.UniformName(tb.Text);
            }

            ComboBox cmb = (sender as ComboBox);
            if (cmb != null && GlobalOptions.Instance.FirstCapitalLetterInNames) {
                cmb.Text = ConvertHelper.UniformName(cmb.Text);
            }
        }
    }
}
