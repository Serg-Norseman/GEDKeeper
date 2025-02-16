/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih, Ruslan Garipov.
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
using System.Linq.Expressions;
using System.Reflection;
using BSLib;
using Eto.Drawing;
using Eto.Forms;
using GDModel;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
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

            // Center the new window on a monitor, where the parent window is located.
            Screen screen = Screen.FromRectangle(parentRect);
            if (screen != null) {
                var workArea = screen.WorkingArea;

                //Logger.WriteInfo(string.Format("ParentRect: {0} - {1} - {2} - {3}", parentRect.Left, parentRect.Top, parentRect.Width, parentRect.Height));
                //Logger.WriteInfo(string.Format("Dlg: {0} - {1}", form.Width, form.Height));

                int fx = (int)parentRect.Left + ((int)(parentRect.Width - form.Width) / 2);
                int fy = (int)parentRect.Top + ((int)(parentRect.Height - form.Height) / 2);

                //Logger.WriteInfo(string.Format("Loc: {0} - {1}", fx, fy));

                if (!workArea.Contains(fx, fy)) {
                    //Logger.WriteInfo("not contains");

                    fx = (int)workArea.Left + ((int)(workArea.Width - form.Width) / 2);
                    fy = (int)workArea.Top + ((int)(workArea.Height - form.Height) / 2);

                    //Logger.WriteInfo(string.Format("Loc: {0} - {1}", fx, fy));
                }

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

        public static RadioMenuItem AddToolStripItem(ContextMenu contextMenu, RadioMenuItem controller, string text, object tag, EventHandler<EventArgs> clickHandler)
        {
            var tsItem = new RadioMenuItem(controller);
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

        public static GKListView CreateRecordsView(Panel parent, IBaseContext baseContext, GDMRecordType recType, bool simpleList)
        {
            if (parent == null)
                throw new ArgumentNullException("parent");

            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            GKListView recView = new GKListView();
            recView.ListMan = RecordsListModel<GDMRecord>.Create(baseContext, recType, simpleList);
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

        public static string[] Convert(string text)
        {
            var strList = new StringList(text);
            return strList.ToArray();
        }

        public static void AddRange(this ListItemCollection items, string[] strings)
        {
            foreach (var str in strings) {
                items.Add(str);
            }
        }

        public static void AddTextColumn<T>(this GridView gridView, string headerText, Expression<Func<T, string>> propExpression, int width, bool autoSize = false, bool editable = false)
        {
            var cell = new TextBoxCell();
            cell.Binding = Binding.Property(propExpression);

            gridView.AddColumn(cell, headerText, width, autoSize, editable);
        }

        public static void AddComboColumn<T>(this GridView gridView, string headerText, object[] items, Expression<Func<T, object>> propExpression, int width, bool autoSize = false, bool editable = false)
        {
            var cell = new ComboBoxCell();
            cell.DataStore = items;
            cell.Binding = Binding.Property(propExpression);

            gridView.AddColumn(cell, headerText, width, autoSize, editable);
        }

        public static GridColumn AddColumn(this GridView gridView, Cell cell, string headerText, int width, bool autoSize = false, bool editable = false)
        {
            var col = new GridColumn() {
                DataCell = cell,
                HeaderText = headerText,
                Width = width,
                AutoSize = autoSize,
                Sortable = false,
                Editable = editable,
            };
            gridView.Columns.Add(col);
            return col;
        }

        public static void FixRadioButtons(Control instance, Control control)
        {
            if (control == null) return;

            control.SuspendLayout();
            RadioButton controller = null;
            FixRadioButtonsInt(instance, control, ref controller);
            control.ResumeLayout();
        }

        private static void FixRadioButtonsInt(Control instance, Control control, ref RadioButton controller)
        {
            if (control is StackLayout) {
                var stackLayout = (StackLayout)control;

                foreach (var item in stackLayout.Items) {
                    var radBtn = item.Control as RadioButton;
                    if (radBtn != null) {
                        item.Control = ReplaceRadioButton(instance, radBtn, ref controller);
                    } else {
                        FixRadioButtonsInt(instance, item.Control, ref controller);
                    }
                }
            } else if (control is TableLayout) {
                var tableLayout = (TableLayout)control;

                foreach (var row in tableLayout.Rows) {
                    foreach (var cell in row.Cells) {
                        var radBtn = cell.Control as RadioButton;
                        if (radBtn != null) {
                            cell.Control = ReplaceRadioButton(instance, radBtn, ref controller);
                        } else {
                            FixRadioButtonsInt(instance, cell.Control, ref controller);
                        }
                    }
                }
            } else if (control is Panel) {
                var panel = (Panel)control;
                FixRadioButtonsInt(instance, panel.Content, ref controller);
            }
        }

        private static RadioButton ReplaceRadioButton(Control instance, RadioButton oldRadBtn, ref RadioButton controller)
        {
            var id = oldRadBtn.ID;
            var checkedVal = oldRadBtn.Checked;
            var textVal = oldRadBtn.Text;

            var newRadBtn = new RadioButton(controller);

            MoveEvents(oldRadBtn, newRadBtn, "CheckedChanged");

            oldRadBtn.Dispose();

            newRadBtn.ID = id;
            newRadBtn.Checked = checkedVal;
            newRadBtn.Text = textVal;
            SetField(instance, id, newRadBtn);

            if (controller == null) {
                controller = newRadBtn;
            }

            return newRadBtn;
        }

        private static void MoveEvents(RadioButton source, RadioButton target, string eventName)
        {
            var fieldInfo = source.GetType().GetField(eventName, BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.GetField);
            if (fieldInfo == null) return;

            MulticastDelegate eventDelegate = (MulticastDelegate)fieldInfo.GetValue(source);
            if (eventDelegate == null) return;

            foreach (var handler in eventDelegate.GetInvocationList()) {
                target.GetType().GetEvent(eventName).AddEventHandler(target, handler);
            }
        }

        private static void SetField(object instance, string name, object value)
        {
            if (instance == null)
                return;

            var instanceType = instance.GetType();

            var property = instanceType.GetProperty(name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            if (property != null) {
                property.SetValue(instance, value);
            } else {
                var field = instanceType.GetField(name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly);
                if (field != null)
                    field.SetValue(instance, value);
            }
        }

        public static void SetButtonThemeImage(Button button, ThemeElement themeElement)
        {
            if (button == null) return;

            var themeImage = AppHost.ThemeManager.GetThemeImage(themeElement, true);
            if (themeImage == null) return;

            button.Image = ((ImageHandler)themeImage).Handle;
        }
    }
}
