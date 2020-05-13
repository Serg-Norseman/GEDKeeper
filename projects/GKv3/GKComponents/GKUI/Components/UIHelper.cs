/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih, Ruslan Garipov.
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

namespace GKUI.Components
{
    public class DefStackLayout : StackLayout
    {
        public DefStackLayout() : this(10, 10)
        {
        }

        public DefStackLayout(int padding, int spacing)
        {
            Padding = new Padding(padding);
            Spacing = spacing;
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
    }

    public class HDefStackLayout : DefStackLayout
    {
        public HDefStackLayout() : base()
        {
            Orientation = Orientation.Horizontal;
        }
    }

    public class VDefStackLayout : DefStackLayout
    {
        public VDefStackLayout() : base()
        {
            Orientation = Orientation.Vertical;
        }
    }

    public class VSDefStackLayout : StackLayout
    {
        public VSDefStackLayout(params Control[] items)
        {
            Orientation = Orientation.Vertical;
            Padding = new Padding(0);
            Spacing = 0;
            foreach (var item in items) Items.Add(item);
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

            if (winState != WindowState.Minimized) {
                form.Location = new Point(rt.Left, rt.Top);
                form.Width = rt.GetWidth();
                form.Height = rt.GetHeight();

                form.WindowState = winState;
            } else {
                form.WindowState = WindowState.Maximized;
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
            object selectedItem = comboBox.SelectedValue;
            GKComboItem comboItem = (GKComboItem)selectedItem;
            T itemTag = (T)comboItem.Tag;
            return itemTag;
        }

        public static void SetSelectedTag<T>(ComboBox comboBox, T tagValue)
        {
            foreach (object item in comboBox.Items) {
                GKComboItem comboItem = (GKComboItem)item;
                T itemTag = (T)comboItem.Tag;

                if (tagValue.Equals(itemTag)) {
                    comboBox.SelectedValue = item;
                    return;
                }
            }
            comboBox.SelectedIndex = 0;
        }

        public static void SelectComboItem(ComboBox comboBox, object tag, bool allowDefault)
        {
            for (int i = 0; i < comboBox.Items.Count; i++) {
                GKComboItem item = comboBox.Items[i] as GKComboItem;

                if (item != null && object.Equals(item.Tag, tag)) {
                    comboBox.SelectedIndex = i;
                    return;
                }
            }

            if (allowDefault) {
                comboBox.SelectedIndex = 0;
            }
        }

        public static void SelectComboItem(ListBox listBox, object tag, bool allowDefault)
        {
            for (int i = 0; i < listBox.Items.Count; i++) {
                GKComboItem item = listBox.Items[i] as GKComboItem;

                if (item != null && object.Equals(item.Tag, tag)) {
                    listBox.SelectedIndex = i;
                    return;
                }
            }

            if (allowDefault) {
                listBox.SelectedIndex = 0;
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

        public static void CreateCircleSegment(GraphicsPath path, int ctX, int ctY,
                                               float inRad, float extRad, float wedgeAngle,
                                               float ang1, float ang2)
        {
            float angCos, angSin;

            float angval1 = (float)(ang1 * Math.PI / 180.0f);
            angCos = (float)Math.Cos(angval1);
            angSin = (float)Math.Sin(angval1);
            float px1 = ctX + (inRad * angCos);
            float py1 = ctY + (inRad * angSin);
            float px2 = ctX + (extRad * angCos);
            float py2 = ctY + (extRad * angSin);

            float angval2 = (float)(ang2 * Math.PI / 180.0f);
            angCos = (float)Math.Cos(angval2);
            angSin = (float)Math.Sin(angval2);
            float nx1 = ctX + (inRad * angCos);
            float ny1 = ctY + (inRad * angSin);
            float nx2 = ctX + (extRad * angCos);
            float ny2 = ctY + (extRad * angSin);

            float ir2 = inRad * 2.0f;
            float er2 = extRad * 2.0f;

            path.StartFigure();
            path.AddLine(px2, py2, px1, py1);
            if (ir2 > 0) path.AddArc(ctX - inRad, ctY - inRad, ir2, ir2, ang1, wedgeAngle);
            path.AddLine(nx1, ny1, nx2, ny2);
            path.AddArc(ctX - extRad, ctY - extRad, er2, er2, ang2, -wedgeAngle);
            path.CloseFigure();
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

        public static GridColumn CreateTextColumn(string colName, string headerText, int width)
        {
            var col = new GridColumn(); //DataGridViewTextBoxColumn();
            if (!string.IsNullOrEmpty(colName)) col.ID = colName;
            col.HeaderText = headerText;
            col.DataCell = new TextBoxCell();
            col.Width = width;
            return col;
        }

        public static GridColumn CreateComboColumn(string colName, string headerText, object[] items, int width)
        {
            var col = new GridColumn(); //DataGridViewComboBoxColumn();
            if (!string.IsNullOrEmpty(colName)) col.ID = colName;
            col.HeaderText = headerText;
            col.DataCell = new ComboBoxCell();
            col.Width = width;
            //col.Items.AddRange(items);
            return col;
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

        /// <summary>
        /// Create stack for row cell (RCS).
        /// </summary>
        /// <param name="items"></param>
        /// <returns></returns>
        public static StackLayout CreateRCS(params StackLayoutItem[] items)
        {
            return CreateStackLayout(Orientation.Horizontal, 0, 10, items);
        }

        public static StackLayout CreateStackLayout(Orientation orientation,
                                                    int padding, int spacing,
                                                    params StackLayoutItem[] items)
        {
            var res = new DefStackLayout();
            res.Orientation = orientation;
            res.Padding = new Padding(padding);
            res.Spacing = spacing;
            foreach (var itm in items) {
                //itm.VerticalAlignment = VerticalAlignment.Center;
                res.Items.Add(itm);
            }
            return res;
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

        public static void SortItems(this ComboBox comboBox)
        {
            comboBox.Items.Sort((x, y) => string.Compare(x.Text, y.Text, StringComparison.CurrentCulture));
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
    }
}
