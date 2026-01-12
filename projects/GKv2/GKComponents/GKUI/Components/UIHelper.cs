/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih, Ruslan Garipov.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Drawing.Text;
using System.Windows.Forms;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Lists;
using GKCore.Options;
using GKUI.Platform.Handlers;
using GKUI.Themes;

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

        public static GKListView CreateRecordsView(Control parent, BaseContext baseContext, GDMRecordType recType, bool simpleList)
        {
            if (parent == null)
                throw new ArgumentNullException(nameof(parent));

            if (baseContext == null)
                throw new ArgumentNullException(nameof(baseContext));

            GKListView recView = new GKListView();
            recView.HideSelection = false;
            recView.LabelEdit = false;
            recView.FullRowSelect = true;
            recView.View = View.Details;
            recView.ListMan = RecordsListModel<GDMRecord>.Create(baseContext, recType, simpleList);
            recView.Dock = DockStyle.Fill;

            parent.Controls.Add(recView);
            parent.Controls.SetChildIndex(recView, 0);

            return recView;
        }

        public static GKListView CreateListView(Control parent)
        {
            if (parent == null)
                throw new ArgumentNullException(nameof(parent));

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
            using (var stream = GKUtils.LoadResourceStream(resName)) {
                return new Bitmap(stream);
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

        public static void SetButtonThemeImage(ToolStripButton button, ThemeElement themeElement)
        {
            if (button == null) return;

            var themeImage = AppHost.ThemeManager.GetThemeImage(themeElement, true);
            if (themeImage == null) return;

            button.Image = ((ImageHandler)themeImage).Handle;
        }

        public static void SetButtonThemeImage(Button button, ThemeElement themeElement)
        {
            if (button == null) return;

            var themeImage = AppHost.ThemeManager.GetThemeImage(themeElement, true);
            if (themeImage == null) return;

            button.Image = ((ImageHandler)themeImage).Handle;
        }

        public static void DrawSortArrow(Graphics gfx, Font ctlFont, Rectangle rt, string arrow)
        {
            using (var fnt = new Font(ctlFont.FontFamily, ctlFont.SizeInPoints * 0.6f, FontStyle.Regular)) {
                float aw = gfx.MeasureString(arrow, fnt).Width;
                float x = rt.Left + (rt.Width - aw) / 2.0f;
                gfx.TextRenderingHint = TextRenderingHint.AntiAlias;
                gfx.DrawString(arrow, fnt, Brushes.Black, x, rt.Top);
            }
        }

        public static void DrawHeaderBackground(Graphics g, Color backColor, Rectangle bounds, bool classic3d = false)
        {
            using (Brush brush = new SolidBrush(backColor)) {
                g.FillRectangle(brush, bounds);
            }

            Rectangle rect = bounds;
            if (classic3d) {
                rect.Width--;
                rect.Height--;
                g.DrawRectangle(SystemPens.ControlDarkDark, rect);
                rect.Width--;
                rect.Height--;
                g.DrawLine(SystemPens.ControlLightLight, rect.X, rect.Y, rect.Right, rect.Y);
                g.DrawLine(SystemPens.ControlLightLight, rect.X, rect.Y, rect.X, rect.Bottom);
                g.DrawLine(SystemPens.ControlDark, rect.X + 1, rect.Bottom, rect.Right, rect.Bottom);
                g.DrawLine(SystemPens.ControlDark, rect.Right, rect.Y + 1, rect.Right, rect.Bottom);
            } else {
                rect.Width--;
                rect.Height--;
                g.DrawLine(SystemPens.ControlDark, rect.Right, rect.Y + 1, rect.Right, rect.Bottom - 1);
            }
        }

        public static ImageFormat DetermineImageFormat(string ext, ImageFormat defaultFormat)
        {
            ImageFormat imFmt = defaultFormat;

            if (ext == ".bmp") {
                imFmt = ImageFormat.Bmp;
            } else if (ext == ".png") {
                imFmt = ImageFormat.Png;
            } else if (ext == ".gif") {
                imFmt = ImageFormat.Gif;
            } else if (ext == ".jpg") {
                imFmt = ImageFormat.Jpeg;
            } else if (ext == ".emf") {
                imFmt = ImageFormat.Emf;
            }

            return imFmt;
        }
    }
}
