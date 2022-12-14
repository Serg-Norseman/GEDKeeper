/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using BSLib;
using BSLib.Design.Handlers;
using GKCore;
using GKUI.Components;

namespace GKUI.Themes
{
    public static class ThemeManager
    {
        private delegate void ThemeControlHandler(IThemedView view, Component component, Theme theme);

        private static Dictionary<string, Theme> fThemes = new Dictionary<string, Theme>();
        private static Dictionary<Type, ThemeControlHandler> fControlHandlers = new Dictionary<Type, ThemeControlHandler>();
        private static Theme fCurrentTheme;

        public static List<Theme> Themes
        {
            get { return fThemes.Values.ToList(); }
        }

        static ThemeManager()
        {
            RegisterControlHandlers();

            RegisterTheme("Default", new ThemeElementsDictionary() {
                { ThemeElement.Font, "Tahoma" },                              // checked
                { ThemeElement.FontSize, 8.25f },                             // checked

                { ThemeElement.Editor, SystemColors.Window },                 // checked
                { ThemeElement.EditorText, SystemColors.WindowText },         // checked

                { ThemeElement.Control, SystemColors.Control },               // checked
                { ThemeElement.ControlText, SystemColors.ControlText },       // checked

                { ThemeElement.Window, SystemColors.Control },                // checked
                { ThemeElement.WindowText, SystemColors.ControlText },        // checked

                { ThemeElement.Dialog, SystemColors.Control },                // checked
                { ThemeElement.DialogText, SystemColors.ControlText },        // checked

                { ThemeElement.ButtonFace, SystemColors.ControlLight },       // checked
                { ThemeElement.AccentButtonFace, SystemColors.ControlLight }, // checked
                { ThemeElement.ButtonBorder, Color.Black },                   // checked
                { ThemeElement.ButtonText, SystemColors.ControlText },        // checked

                { ThemeElement.Strip, SystemColors.Control },                 // <- ProfessionalColorTable
                { ThemeElement.Dropdown, SystemColors.Control },              // <- ProfessionalColorTable
                { ThemeElement.MenuBorder, SystemColors.Control },            // <- ProfessionalColorTable
                { ThemeElement.MenuItemSelected, SystemColors.Control },      // <- ProfessionalColorTable

                { ThemeElement.Link, Color.Blue },                            // checked

                { ThemeElement.Grid, SystemColors.Window },                   // checked
                { ThemeElement.GridHeader, SystemColors.Window },             // checked
                { ThemeElement.GridHeaderText, SystemColors.WindowText },     // checked
                { ThemeElement.GridText, SystemColors.WindowText },           // checked

                { ThemeElement.HighlightUnparentedIndi, Color.FromArgb(0xFFCACA) },       // GK only
                { ThemeElement.HighlightUnmarriedIndi, Color.FromArgb(0xFFFFA1) },        // GK only
                { ThemeElement.HighlightInaccessibleFiles, Color.FromArgb(0xFFCACA) },    // GK only
            }, true);
        }

        public static void LoadThemes()
        {
            string path = GKUtils.GetAppPath() + "themes" + Path.DirectorySeparatorChar;
            if (!Directory.Exists(path)) return;

            try {
                string[] themeFiles = Directory.GetFiles(path, "*.yaml");
                foreach (string fn in themeFiles) {
                    Load(fn);
                }
            } catch (Exception ex) {
                Logger.WriteError("ThemeManager.LoadThemes(" + path + ")", ex);
            }
        }

        public static void Load(string fileName)
        {
            if (!File.Exists(fileName)) return;

            try {
                ThemeFile themeFile;
                using (var reader = new StreamReader(fileName)) {
                    string content = reader.ReadToEnd();
                    themeFile = YamlHelper.Deserialize<ThemeFile>(content);
                }

                var themeElements = new ThemeElementsDictionary();
                for (int i = 0; i < themeFile.Elements.Length; i++) {
                    var tfc = themeFile.Elements[i];
                    var tcName = EnumHelper.Parse<ThemeElement>(tfc.Element);

                    object tcVal;
                    switch (tcName) {
                        case ThemeElement.Font:
                            tcVal = tfc.Value;
                            break;
                        case ThemeElement.FontSize:
                            tcVal = (float)ConvertHelper.ParseFloat(tfc.Value, 8, true);
                            break;
                        default:
                            tcVal = UIHelper.ParseColor(tfc.Value);
                            break;
                    }

                    themeElements.Add(tcName, tcVal);
                }

                RegisterTheme(themeFile.Name, themeElements, false);
            } catch (Exception ex) {
                Logger.WriteError("ThemeManager.Load()", ex);
            }
        }

        public static void RegisterTheme(string name, ThemeElementsDictionary colors, bool sysDefault = false)
        {
            fThemes.Add(name, new Theme(name, colors, sysDefault));
        }

        public static void SetTheme(string name)
        {
            Theme theme;
            if (fThemes.TryGetValue(name, out theme)) {
                fCurrentTheme = theme;
            }
        }

        public static void ApplyTheme(IThemedView view)
        {
            if (view == null || fCurrentTheme == null) return;

            GKData.HighlightUnparentedColor = GetThemeColor(ThemeElement.HighlightUnparentedIndi).ToArgb();
            GKData.HighlightUnmarriedColor = GetThemeColor(ThemeElement.HighlightUnmarriedIndi).ToArgb();
            GKData.HighlightInaccessibleFiles = GetThemeColor(ThemeElement.HighlightInaccessibleFiles).ToArgb();

            var form = view as Form;
            if (form != null) {
                form.SuspendLayout();

                var themeFont = GetThemeStr(ThemeElement.Font);
                var themeFontSize = GetThemeFloat(ThemeElement.FontSize);
                if (form.Font.Name != themeFont) {
                    form.Font = new Font(themeFont, themeFontSize);
                }

                ApplyTheme(view, form, fCurrentTheme);

                form.ResumeLayout(true);
            }
        }

        private static void ApplyTheme(IThemedView view, Component component, Theme theme)
        {
            if (view.SkipTheme(component))
                return;

            ThemeControlHandler handler = GetControlHandler(component);
            if (handler != null) {
                handler(view, component, theme);
            }

            if (component is Control) {
                Control ctl = (Control)component;

                ctl.Font = ((Form)view).Font;

                foreach (Control item in ctl.Controls) {
                    ApplyTheme(view, item, theme);
                }
            }
        }

        #region Control handlers

        private static Color GetThemeColor(ThemeElement element)
        {
            object elemValue;
            if (fCurrentTheme.Elements.TryGetValue(element, out elemValue) && elemValue is Color) {
                return (Color)elemValue;
            }

            return Color.Black;
        }

        private static string GetThemeStr(ThemeElement element)
        {
            object elemValue;
            if (fCurrentTheme.Elements.TryGetValue(element, out elemValue) && elemValue is string) {
                return (string)elemValue;
            }

            return string.Empty;
        }

        private static float GetThemeFloat(ThemeElement element)
        {
            object elemValue;
            if (fCurrentTheme.Elements.TryGetValue(element, out elemValue) && elemValue is float) {
                return (float)elemValue;
            }

            return 0.0f;
        }

        private static void ThemeButtonHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (Button)component;
            ctl.ForeColor = GetThemeColor(ThemeElement.ButtonText);

            ctl.FlatStyle = (!theme.SysDefault) ? FlatStyle.Flat : FlatStyle.Standard;
            if (ctl.FlatStyle == FlatStyle.Flat) {
                ctl.FlatAppearance.BorderColor = GetThemeColor(ThemeElement.ButtonBorder);
                ctl.FlatAppearance.BorderSize = 1;
            }

            var form = ctl.FindForm();
            if (ctl == form.AcceptButton) {
                ctl.BackColor = GetThemeColor(ThemeElement.AccentButtonFace);
            } else {
                ctl.BackColor = GetThemeColor(ThemeElement.ButtonFace);
            }
        }

        private static void ThemeCheckBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (CheckBox)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemeComboBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ComboBox)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(ThemeElement.EditorText);
        }

        private static void ThemeDataGridViewHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (DataGridView)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Grid);
            ctl.ForeColor = GetThemeColor(ThemeElement.GridText);
            ctl.ColumnHeadersDefaultCellStyle.BackColor = GetThemeColor(ThemeElement.GridHeader);
            ctl.ColumnHeadersDefaultCellStyle.ForeColor = GetThemeColor(ThemeElement.GridHeaderText);
            ctl.EnableHeadersVisualStyles = (theme.SysDefault);
        }

        private static void ThemeFormHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (Form)component;

            if (!ctl.Modal) {
                // window
                ctl.BackColor = GetThemeColor(ThemeElement.Window);
                ctl.ForeColor = GetThemeColor(ThemeElement.WindowText);
            } else {
                // dialog
                ctl.BackColor = GetThemeColor(ThemeElement.Dialog);
                ctl.ForeColor = GetThemeColor(ThemeElement.DialogText);
            }
        }

        private static void ThemeGroupBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (GroupBox)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemeHyperViewHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (HyperView)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
            ctl.LinkColor = GetThemeColor(ThemeElement.Link);
        }

        private static void GetParentDependentColors(Control control, Theme theme, out Color backColor, out Color foreColor)
        {
            var ctlParent = control.Parent;

            backColor = ctlParent.BackColor;

            if (ctlParent is Form) {
                if (!((Form)ctlParent).Modal) {
                    // window
                    foreColor = GetThemeColor(ThemeElement.WindowText);
                } else {
                    // dialog
                    foreColor = GetThemeColor(ThemeElement.DialogText);
                }
            } else {
                foreColor = GetThemeColor(ThemeElement.ControlText);
            }
        }

        private static void ThemeLabelHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (Label)component;

            Color backColor, foreColor;
            GetParentDependentColors(ctl, theme, out backColor, out foreColor);

            ctl.BackColor = backColor;
            ctl.ForeColor = foreColor;
        }

        private static void ThemeListBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ListBox)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(ThemeElement.EditorText);
        }

        private static void ThemeListViewHandler(IThemedView view, Component component, Theme theme)
        {
            if (component is GKListView) {
                // extended
                var ctl = (GKListView)component;
                ctl.BackColor = GetThemeColor(ThemeElement.Grid);
                ctl.ForeColor = GetThemeColor(ThemeElement.GridText);

                if (theme.SysDefault) {
                    ctl.Appearance.Reset();
                } else {
                    ctl.Appearance.BackColor = GetThemeColor(ThemeElement.Grid);
                    ctl.Appearance.Header = GetThemeColor(ThemeElement.GridHeader);
                    ctl.Appearance.HeaderText = GetThemeColor(ThemeElement.GridHeaderText);
                }

                ctl.ResetCache();
            } else {
                // standard
                var ctl = (ListView)component;
                ctl.BackColor = GetThemeColor(ThemeElement.Grid);
                ctl.ForeColor = GetThemeColor(ThemeElement.GridText);
            }
        }

        private static void ThemeMenuStripHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (MenuStrip)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Strip);
            ctl.ForeColor = GetThemeColor(ThemeElement.ButtonText);

            ctl.Renderer = (theme.SysDefault) ? new ToolStripProfessionalRenderer() : new TSRenderer(theme);

            foreach (ToolStripItem item in ctl.Items) {
                ApplyTheme(view, item, theme);
            }
        }

        private static void ThemeNumericUpDownHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (NumericUpDown)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(ThemeElement.EditorText);
        }

        private static void ThemePanelHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (Panel)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemePictureBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (PictureBox)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemeProgressBarHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ProgressBar)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemeRadioButtonHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (RadioButton)component;
            ctl.BackColor = ctl.Parent.BackColor;
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemeTextBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (TextBoxBase)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(ThemeElement.EditorText);

            //ctl.BorderStyle = (!theme.SysDefault) ? BorderStyle.FixedSingle : BorderStyle.Fixed3D;
        }

        private static void ThemeScrollBarHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ScrollBar)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemeStatusBarHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (StatusBar)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemeStatusBarPanelHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (StatusBarPanel)component;
            //ctl.BackColor = theme.Colors[ThemeColor.Control);
            //ctl.ForeColor = theme.Colors[ThemeColor.ControlText);
        }

        private static void ThemeTabControlHandler(IThemedView view, Component component, Theme theme)
        {
            if (component is GKTabControl) {
                // extended
                var ctl = (GKTabControl)component;
                ctl.BackColor = GetThemeColor(ThemeElement.Control);
                ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);

                if (theme.SysDefault) {
                    ctl.Appearance.Reset();
                } else {
                    ctl.Appearance.BackColor = GetThemeColor(ThemeElement.Control);
                    ctl.Appearance.Tab = GetThemeColor(ThemeElement.Window);
                    ctl.Appearance.TabHighlight = GetThemeColor(ThemeElement.Window);
                    ctl.Appearance.TabSelected = GetThemeColor(ThemeElement.MenuItemSelected);
                }
            } else {
                // standard
                var ctl = (TabControl)component;
                ctl.BackColor = GetThemeColor(ThemeElement.Control);
                ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
            }
        }

        private static void ThemeTabPageHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (TabPage)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void ThemeToolStripHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ToolStrip)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Strip);
            ctl.ForeColor = GetThemeColor(ThemeElement.ButtonText);

            ctl.Renderer = (theme.SysDefault) ? new ToolStripProfessionalRenderer() : new TSRenderer(theme);

            foreach (ToolStripItem item in ctl.Items) {
                ApplyTheme(view, item, theme);
            }
        }

        private static void ThemeToolStripItemHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ToolStripItem)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Dropdown);
            ctl.ForeColor = GetThemeColor(ThemeElement.ButtonText);

            if (ctl is ToolStripDropDownItem) {
                var dropdownItem = (ToolStripDropDownItem)ctl;

                dropdownItem.DropDown.BackColor = GetThemeColor(ThemeElement.Dropdown);
                dropdownItem.DropDown.ForeColor = GetThemeColor(ThemeElement.ButtonText);

                foreach (ToolStripItem item in dropdownItem.DropDownItems) {
                    ApplyTheme(view, item, theme);
                }
            } else if (ctl is ToolStripSeparator) {
                if (theme.SysDefault) {
                } else {
                }
            }
        }

        private static void ThemeTreeViewHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (TreeView)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(ThemeElement.EditorText);
        }

        private static void ThemeUserControlHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (UserControl)component;
            ctl.BackColor = GetThemeColor(ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(ThemeElement.ControlText);
        }

        private static void RegisterControlHandlers()
        {
            RegisterControlHandler(typeof(Button), ThemeButtonHandler);                 // ready +
            RegisterControlHandler(typeof(CheckBox), ThemeCheckBoxHandler);             // ?
            RegisterControlHandler(typeof(ComboBox), ThemeComboBoxHandler);             // ?
            RegisterControlHandler(typeof(ContextMenuStrip), ThemeMenuStripHandler);    // ?
            RegisterControlHandler(typeof(DataGridView), ThemeDataGridViewHandler);     // ?
            RegisterControlHandler(typeof(Form), ThemeFormHandler);                     // ?
            RegisterControlHandler(typeof(GroupBox), ThemeGroupBoxHandler);             // ?
            RegisterControlHandler(typeof(Label), ThemeLabelHandler);                   // ready +
            RegisterControlHandler(typeof(ListBox), ThemeListBoxHandler);               // ? (only plugins, not host)
            RegisterControlHandler(typeof(ListView), ThemeListViewHandler);             // ?
            RegisterControlHandler(typeof(MaskedTextBox), ThemeTextBoxHandler);         // ?
            RegisterControlHandler(typeof(MenuStrip), ThemeMenuStripHandler);           // ?
            RegisterControlHandler(typeof(NumericUpDown), ThemeNumericUpDownHandler);   // ?
            RegisterControlHandler(typeof(Panel), ThemePanelHandler);                   // ?
            RegisterControlHandler(typeof(PictureBox), ThemePictureBoxHandler);         // ?
            RegisterControlHandler(typeof(ProgressBar), ThemeProgressBarHandler);       // ?
            RegisterControlHandler(typeof(RadioButton), ThemeRadioButtonHandler);       // ?
            RegisterControlHandler(typeof(RichTextBox), ThemeTextBoxHandler);           // ?
            RegisterControlHandler(typeof(ScrollBar), ThemeScrollBarHandler);           // ?
            RegisterControlHandler(typeof(SplitContainer), ThemePanelHandler);          // ?
            RegisterControlHandler(typeof(StatusBar), ThemeStatusBarHandler);           // ?
            RegisterControlHandler(typeof(StatusBarPanel), ThemeStatusBarPanelHandler); // ?
            RegisterControlHandler(typeof(TableLayoutPanel), ThemePanelHandler);        // ?
            RegisterControlHandler(typeof(TabControl), ThemeTabControlHandler);         // ?
            RegisterControlHandler(typeof(TabPage), ThemeTabPageHandler);               // ?
            RegisterControlHandler(typeof(TextBox), ThemeTextBoxHandler);               // ?
            RegisterControlHandler(typeof(ToolStrip), ThemeToolStripHandler);           // ?
            RegisterControlHandler(typeof(ToolStripButton), ThemeToolStripItemHandler); // ?
            RegisterControlHandler(typeof(ToolStripComboBox), ThemeToolStripItemHandler);       // ?
            RegisterControlHandler(typeof(ToolStripDropDownButton), ThemeToolStripItemHandler); // ?
            RegisterControlHandler(typeof(ToolStripMenuItem), ThemeToolStripItemHandler);       // ?
            RegisterControlHandler(typeof(ToolStripSeparator), ThemeToolStripItemHandler);      // ?
            RegisterControlHandler(typeof(TrackBar), ThemePanelHandler);                // ?
            RegisterControlHandler(typeof(TreeView), ThemeTreeViewHandler);             // ?

            RegisterControlHandler(typeof(MenuItemEx), ThemeToolStripItemHandler);      // ?

            RegisterControlHandler(typeof(ArborViewer), ThemePanelHandler);             // ?
            RegisterControlHandler(typeof(FilterGridView), ThemeDataGridViewHandler);   // ?
            RegisterControlHandler(typeof(GKTabControl), ThemeTabControlHandler);       // ready +
            RegisterControlHandler(typeof(GKComboBox), ThemeComboBoxHandler);           // ?
            RegisterControlHandler(typeof(GKDateBox), ThemeTextBoxHandler);             // ?
            RegisterControlHandler(typeof(GKDateControl), ThemeUserControlHandler);     // ?
            RegisterControlHandler(typeof(GKListView), ThemeListViewHandler);           // ?
            RegisterControlHandler(typeof(GKPortrait), ThemeUserControlHandler);        // ?
            RegisterControlHandler(typeof(GKTextBox), ThemeTextBoxHandler);             // ?
            RegisterControlHandler(typeof(HyperView), ThemeHyperViewHandler);           // ?
            RegisterControlHandler(typeof(ImageBox), ThemePanelHandler);                // ?
            RegisterControlHandler(typeof(ImageView), ThemeUserControlHandler);         // ?
            RegisterControlHandler(typeof(LogChart), ThemePanelHandler);                // ?
        }

        private static void RegisterControlHandler(Type controlType, ThemeControlHandler handler)
        {
            fControlHandlers.Add(controlType, handler);
        }

        private static ThemeControlHandler GetControlHandler(Component component)
        {
            var compType = component.GetType();

            ThemeControlHandler handler;
            if (fControlHandlers.TryGetValue(compType, out handler))
                return handler;

            foreach (var pair in fControlHandlers) {
                Type baseType = pair.Key;
                if (baseType.IsAssignableFrom(compType)) {
                    return pair.Value;
                }
            }

            return null;
        }

        #endregion

        #region Helpers

        private class TSRenderer : ToolStripProfessionalRenderer
        {
            private readonly Theme fTheme;

            public TSRenderer(Theme theme) : base(new TMColors(theme))
            {
                fTheme = theme;
            }

            private void RenderSeparatorInternal(Graphics g, ToolStripItem item, Rectangle bounds, bool vertical)
            {
                Color separatorDark = GetThemeColor(ThemeElement.MenuBorder);
                Color separatorLight = GetThemeColor(ThemeElement.MenuBorder);
                using (Pen pen = new Pen(separatorDark))
                using (Pen pen2 = new Pen(separatorLight))  {
                    bool isSeparator = item is ToolStripSeparator;
                    bool isNotDropdown = false;
                    if (isSeparator) {
                        if (vertical) {
                            if (!item.IsOnDropDown) {
                                bounds.Y += 3;
                                bounds.Height = Math.Max(0, bounds.Height - 6);
                            }
                        } else {
                            ToolStripDropDownMenu toolStripDropDownMenu = item.GetCurrentParent() as ToolStripDropDownMenu;
                            if (toolStripDropDownMenu != null) {
                                if (toolStripDropDownMenu.RightToLeft == RightToLeft.No) {
                                    bounds.X += toolStripDropDownMenu.Padding.Left - 2;
                                    bounds.Width = toolStripDropDownMenu.Width - bounds.X;
                                } else {
                                    bounds.X += 2;
                                    bounds.Width = toolStripDropDownMenu.Width - bounds.X - toolStripDropDownMenu.Padding.Right;
                                }
                            } else {
                                isNotDropdown = true;
                            }
                        }
                    }

                    if (vertical) {
                        if (bounds.Height >= 4) {
                            bounds.Inflate(0, -2);
                        }

                        bool rtl = item.RightToLeft == RightToLeft.Yes;
                        Pen pen3 = (rtl ? pen2 : pen);
                        Pen pen4 = (rtl ? pen : pen2);
                        int num = bounds.Width / 2;
                        g.DrawLine(pen3, num, bounds.Top, num, bounds.Bottom - 1);
                        num++;
                        g.DrawLine(pen4, num, bounds.Top + 1, num, bounds.Bottom);
                    } else {
                        if (isNotDropdown && bounds.Width >= 4) {
                            bounds.Inflate(-2, 0);
                        }

                        int num2 = bounds.Height / 2;
                        g.DrawLine(pen, bounds.Left, num2, bounds.Right - 1, num2);
                        if (!isSeparator || isNotDropdown) {
                            num2++;
                            g.DrawLine(pen2, bounds.Left + 1, num2, bounds.Right - 1, num2);
                        }
                    }
                }
            }

            protected override void OnRenderSeparator(ToolStripSeparatorRenderEventArgs e)
            {
                RenderSeparatorInternal(e.Graphics, e.Item, e.Item.Bounds, e.Vertical);
            }

            protected override void OnRenderArrow(ToolStripArrowRenderEventArgs e)
            {
                var tsMenuItem = e.Item as ToolStripMenuItem;
                if (tsMenuItem != null)
                    e.ArrowColor = GetThemeColor(ThemeElement.MenuBorder);
                base.OnRenderArrow(e);
            }
        }

        public class TMColors : ProfessionalColorTable
        {
            private readonly Theme fTheme;

            public TMColors(Theme theme)
            {
                fTheme = theme;
            }

            private Color GetThemeColor(ThemeElement element)
            {
                object elemValue;
                if (fTheme.Elements.TryGetValue(element, out elemValue) && elemValue is Color) {
                    return (Color)elemValue;
                }

                return Color.Black;
            }

            public override Color MenuItemSelected
            {
                get { return GetThemeColor(ThemeElement.MenuItemSelected); }
            }

            public override Color MenuItemBorder
            {
                get { return GetThemeColor(ThemeElement.MenuBorder); }
            }

            public override Color MenuBorder
            {
                get { return GetThemeColor(ThemeElement.MenuBorder); }
            }

            public override Color SeparatorDark
            {
                get { return Color.Red; }
            }

            public override Color MenuItemSelectedGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }
            public override Color MenuItemSelectedGradientEnd
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }

            public override Color ImageMarginGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }
            public override Color ImageMarginGradientMiddle
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }
            public override Color ImageMarginGradientEnd
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }

            public override Color ToolStripDropDownBackground
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }

            public override Color MenuItemPressedGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }
            public override Color MenuItemPressedGradientMiddle
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }
            public override Color MenuItemPressedGradientEnd
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }

            public override Color MenuStripGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }
            public override Color MenuStripGradientEnd
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }

            public override Color ToolStripGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }
            public override Color ToolStripGradientMiddle
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }
            public override Color ToolStripGradientEnd
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }

            public override Color ToolStripContentPanelGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }
            public override Color ToolStripContentPanelGradientEnd
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }

            public override Color StatusStripGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }
            public override Color StatusStripGradientEnd
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }
        }

        #endregion
    }
}
