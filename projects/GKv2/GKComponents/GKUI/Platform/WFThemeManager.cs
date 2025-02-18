/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using GKCore;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Themes
{
    public sealed class WFThemeManager : BaseThemeManager
    {
        private delegate void ThemeControlHandler(IThemedView view, Component component, Theme theme);

        private static Dictionary<Type, ThemeControlHandler> fControlHandlers = new Dictionary<Type, ThemeControlHandler>();

        static WFThemeManager()
        {
            RegisterControlHandlers();
        }

        public WFThemeManager()
        {
            RegisterTheme(DefaultThemeName, new ThemeElementsDictionary() {
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

                { ThemeElement.Tab, SystemColors.Window },                    // checked
                { ThemeElement.TabHighlight, SystemColors.Window },           // checked
                { ThemeElement.TabSelected, SystemColors.Control },           // checked

                { ThemeElement.HighlightReadabilityRows, Color.FromArgb(0xEFEFEF) },      // GK only
                { ThemeElement.HighlightUnparentedIndi, Color.FromArgb(0xFFCACA) },       // GK only
                { ThemeElement.HighlightUnmarriedIndi, Color.FromArgb(0xFFFFA1) },        // GK only
                { ThemeElement.HighlightInaccessibleFiles, Color.FromArgb(0xFFCACA) },    // GK only

                { ThemeElement.Glyph_FileNew, "Resources.btn_create_new.gif" },
                { ThemeElement.Glyph_FileLoad, "Resources.btn_load.gif" },
                { ThemeElement.Glyph_FileSave, "Resources.btn_save.gif" },
                { ThemeElement.Glyph_FileProperties, "Resources.btn_properties.gif" },
                { ThemeElement.Glyph_Export, "Resources.btn_export.gif" },
                { ThemeElement.Glyph_ExportTable, "Resources.btn_excel.gif" },
                { ThemeElement.Glyph_Exit, "Resources.btn_exit.gif" },

                { ThemeElement.Glyph_RecordAdd, "Resources.btn_rec_new.gif" },
                { ThemeElement.Glyph_RecordEdit, "Resources.btn_rec_edit.gif" },
                { ThemeElement.Glyph_RecordDelete, "Resources.btn_rec_delete.gif" },
                { ThemeElement.Glyph_Search, "Resources.btn_search.gif" },
                { ThemeElement.Glyph_Filter, "Resources.btn_filter.gif" },

                { ThemeElement.Glyph_TreeAncestors, "Resources.btn_tree_ancestry.gif" },
                { ThemeElement.Glyph_TreeDescendants, "Resources.btn_tree_descendants.gif" },
                { ThemeElement.Glyph_TreeBoth, "Resources.btn_tree_both.gif" },
                { ThemeElement.Glyph_Pedigree, "Resources.btn_scroll.gif" },
                { ThemeElement.Glyph_Maps, "" },
                { ThemeElement.Glyph_Stats, "Resources.btn_chart.gif" },

                { ThemeElement.Glyph_Organizer, "Resources.btn_organizer.gif" },
                { ThemeElement.Glyph_Slideshow, "Resources.btn_slideshow.png" },
                { ThemeElement.Glyph_Settings, "Resources.btn_tools.gif" },

                { ThemeElement.Glyph_Help, "Resources.btn_help.gif" },
                { ThemeElement.Glyph_About, "Resources.btn_scroll.gif" },

                { ThemeElement.Glyph_Prev, "Resources.btn_left.gif" },
                { ThemeElement.Glyph_Next, "Resources.btn_right.gif" },
                { ThemeElement.Glyph_SendMail, "Resources.btn_mail.gif" },
                { ThemeElement.Glyph_PartialView, "Resources.btn_table.gif" },

                { ThemeElement.Glyph_Accept, "Resources.btn_accept.gif" },
                { ThemeElement.Glyph_Cancel, "Resources.btn_cancel.gif" },

                { ThemeElement.Glyph_ItemAdd, "Resources.btn_rec_new.gif" },
                { ThemeElement.Glyph_ItemEdit, "Resources.btn_rec_edit.gif" },
                { ThemeElement.Glyph_ItemDelete, "Resources.btn_rec_delete.gif" },
                { ThemeElement.Glyph_LinkJump, "Resources.btn_jump.gif" },
                { ThemeElement.Glyph_MoveUp, "Resources.btn_up.gif" },
                { ThemeElement.Glyph_MoveDown, "Resources.btn_down.gif" },
                { ThemeElement.Glyph_Copy, "Resources.btn_copy.gif" },
                { ThemeElement.Glyph_Cut, "Resources.btn_cut.gif" },
                { ThemeElement.Glyph_Paste, "Resources.btn_paste.gif" },

                { ThemeElement.Glyph_ImageSave, "Resources.btn_save_image.gif" },
                { ThemeElement.Glyph_DocPrint, "Resources.btn_print.gif" },
                { ThemeElement.Glyph_DocPreview, "Resources.btn_preview.gif" },

                { ThemeElement.Glyph_Start, "Resources.btn_start.gif" },
                { ThemeElement.Glyph_Stop, "Resources.btn_stop.gif" },

                { ThemeElement.Glyph_Undo, "Resources.btn_undo.gif" },
                { ThemeElement.Glyph_Redo, "Resources.btn_redo.gif" },

                { ThemeElement.Glyph_Attach, "Resources.btn_rec_new.gif" },
                { ThemeElement.Glyph_Detach, "Resources.btn_rec_delete.gif" },

                { ThemeElement.Glyph_SizeToFit, "Resources.btn_size_to_fit.png" },
                { ThemeElement.Glyph_ZoomIn, "Resources.btn_zoom_in.png" },
                { ThemeElement.Glyph_ZoomOut, "Resources.btn_zoom_out.png" },
                { ThemeElement.Glyph_SetPortrait, "Resources.btn_portrait.png" },
            }, true);
        }

        public override void ApplyTheme(IThemedView view)
        {
            if (view == null || fCurrentTheme == null) return;

            GKData.HighlightReadabilityRows = GetThemeColor(fCurrentTheme, ThemeElement.HighlightReadabilityRows).ToArgb();
            GKData.HighlightUnparentedColor = GetThemeColor(fCurrentTheme, ThemeElement.HighlightUnparentedIndi).ToArgb();
            GKData.HighlightUnmarriedColor = GetThemeColor(fCurrentTheme, ThemeElement.HighlightUnmarriedIndi).ToArgb();
            GKData.HighlightInaccessibleFiles = GetThemeColor(fCurrentTheme, ThemeElement.HighlightInaccessibleFiles).ToArgb();

            var form = view as Form;
            if (form != null) {
                form.SuspendLayout();

                var themeFont = GetThemeStr(fCurrentTheme, ThemeElement.Font);
                var themeFontSize = GetThemeFloat(fCurrentTheme, ThemeElement.FontSize);
                if (form.Font.Name != themeFont) {
                    form.Font = new Font(themeFont, themeFontSize);
                }

                ApplyTheme(view, form, fCurrentTheme);

                form.ResumeLayout(true);
            }
        }

        public override void ApplyTheme(IThemedView view, object component)
        {
            ApplyTheme(view, (Component)component, fCurrentTheme);
        }

        protected override object PreProcessElement(object telVal, ThemeElementType telType)
        {
            if (telType == ThemeElementType.Color) {
                if (telVal is int) {
                    telVal = Color.FromArgb((int)telVal);
                }
                return telVal;
            } else {
                return telVal;
            }
        }

        #region Control handlers

        private static void ApplyTheme(IThemedView view, Component component, Theme theme)
        {
            if (theme == null) return;

            if (view is IThemedForm themedForm) {
                if (themedForm.SkipTheme(component)) return;
            }

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

        private static Color GetThemeColor(Theme theme, ThemeElement element)
        {
            object elemValue;
            if (theme != null && theme.Elements.TryGetValue(element, out elemValue) && elemValue is Color) {
                return (Color)elemValue;
            }
            return Color.Black;
        }

        private static void ThemeButtonHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (Button)component;
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ButtonText);

            ctl.FlatStyle = (!theme.SysDefault) ? FlatStyle.Flat : FlatStyle.Standard;
            if (ctl.FlatStyle == FlatStyle.Flat) {
                ctl.FlatAppearance.BorderColor = GetThemeColor(theme, ThemeElement.ButtonBorder);
                ctl.FlatAppearance.BorderSize = 1;
            }

            var form = ctl.FindForm();
            if (ctl == form.AcceptButton) {
                ctl.BackColor = GetThemeColor(theme, ThemeElement.AccentButtonFace);
            } else {
                ctl.BackColor = GetThemeColor(theme, ThemeElement.ButtonFace);
            }
        }

        private static void ThemeCheckBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (CheckBox)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeComboBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ComboBox)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemeDataGridViewHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (DataGridView)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Grid);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.GridText);
            ctl.ColumnHeadersDefaultCellStyle.BackColor = GetThemeColor(theme, ThemeElement.GridHeader);
            ctl.ColumnHeadersDefaultCellStyle.ForeColor = GetThemeColor(theme, ThemeElement.GridHeaderText);
            ctl.EnableHeadersVisualStyles = (theme.SysDefault);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void FilterGridViewHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ContainerControl)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Grid);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.GridText);

            foreach (Control item in ctl.Controls) {
                ApplyTheme(view, item, theme);
            }
        }

        private static void ThemeFormHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (Form)component;

            if (!ctl.Modal) {
                // window
                ctl.BackColor = GetThemeColor(theme, ThemeElement.Window);
                ctl.ForeColor = GetThemeColor(theme, ThemeElement.WindowText);
            } else {
                // dialog
                ctl.BackColor = GetThemeColor(theme, ThemeElement.Dialog);
                ctl.ForeColor = GetThemeColor(theme, ThemeElement.DialogText);
            }
        }

        private static void ThemeGroupBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (GroupBox)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeHyperViewHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (HyperView)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
            ctl.LinkColor = GetThemeColor(theme, ThemeElement.Link);
            ctl.BorderStyle = (theme.SysDefault) ? BorderStyle.Fixed3D : BorderStyle.FixedSingle;

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeCustomChartHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (CustomChart)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
            ctl.BorderStyle = (theme.SysDefault) ? BorderStyle.Fixed3D : BorderStyle.FixedSingle;

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void GetParentDependentColors(Control control, Theme theme, out Color backColor, out Color foreColor)
        {
            var ctlParent = control.Parent;

            backColor = ctlParent.BackColor;

            if (ctlParent is Form) {
                if (!((Form)ctlParent).Modal) {
                    // window
                    foreColor = GetThemeColor(theme, ThemeElement.WindowText);
                } else {
                    // dialog
                    foreColor = GetThemeColor(theme, ThemeElement.DialogText);
                }
            } else {
                foreColor = GetThemeColor(theme, ThemeElement.ControlText);
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
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemeListViewHandler(IThemedView view, Component component, Theme theme)
        {
            if (component is GKListView) {
                // extended
                var ctl = (GKListView)component;
                ctl.BackColor = GetThemeColor(theme, ThemeElement.Grid);
                ctl.ForeColor = GetThemeColor(theme, ThemeElement.GridText);

                if (theme.SysDefault) {
                    ctl.Appearance.Reset();
                } else {
                    ctl.Appearance.BackColor = GetThemeColor(theme, ThemeElement.Grid);
                    ctl.Appearance.Header = GetThemeColor(theme, ThemeElement.GridHeader);
                    ctl.Appearance.HeaderText = GetThemeColor(theme, ThemeElement.GridHeaderText);
                }

                ctl.ResetCache();
            } else {
                // standard
                var ctl = (ListView)component;
                ctl.BackColor = GetThemeColor(theme, ThemeElement.Grid);
                ctl.ForeColor = GetThemeColor(theme, ThemeElement.GridText);
            }

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeNumericUpDownHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (NumericUpDown)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemePanelHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (Panel)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemePictureBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (PictureBox)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeProgressBarHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ProgressBar)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeRadioButtonHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (RadioButton)component;
            ctl.BackColor = ctl.Parent.BackColor;
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeTextBoxHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (TextBoxBase)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.EditorText);

            //ctl.BorderStyle = (!theme.SysDefault) ? BorderStyle.FixedSingle : BorderStyle.Fixed3D;

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeScrollBarHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ScrollBar)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeStatusBarHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (StatusBar)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
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
                ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
                ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);

                if (theme.SysDefault) {
                    ctl.Appearance.Reset();
                } else {
                    ctl.Appearance.BackColor = GetThemeColor(theme, ThemeElement.Control);
                    ctl.Appearance.Tab = GetThemeColor(theme, ThemeElement.Tab);
                    ctl.Appearance.TabHighlight = GetThemeColor(theme, ThemeElement.TabHighlight);
                    ctl.Appearance.TabSelected = GetThemeColor(theme, ThemeElement.TabSelected);
                }
            } else {
                // standard
                var ctl = (TabControl)component;
                ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
                ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
            }
        }

        private static void ThemeTabPageHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (TabPage)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeContextMenuStripHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = component as Control;
            if (ctl != null && ctl.ContextMenuStrip != null) {
                ThemeToolStripHandler(view, ctl.ContextMenuStrip, theme);
            }
        }

        private static void ThemeToolStripHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ToolStrip)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Strip);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ButtonText);

            ctl.Renderer = (theme.SysDefault) ? new ToolStripProfessionalRenderer() : new TSRenderer(theme);

            foreach (ToolStripItem item in ctl.Items) {
                ApplyTheme(view, item, theme);
            }
        }

        private static void ThemeToolStripItemHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (ToolStripItem)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Dropdown);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ButtonText);

            if (ctl is ToolStripDropDownItem) {
                var dropdownItem = (ToolStripDropDownItem)ctl;
                ThemeToolStripHandler(view, dropdownItem.DropDown, theme);
            } else if (ctl is ToolStripSeparator) {
                if (theme.SysDefault) {
                } else {
                }
            } else if (ctl is ToolStripStatusLabel) {
                var statusLabel = (ToolStripStatusLabel)ctl;
                statusLabel.BorderStyle = (theme.SysDefault) ? Border3DStyle.Sunken : Border3DStyle.Adjust; // Flat
            }
        }

        private static void ThemeTreeViewHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (TreeView)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.EditorText);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeUserControlHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (UserControl)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeBaseControlHandler(IThemedView view, Component component, Theme theme)
        {
            var ctl = (Control)component;
            ctl.BackColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.ForeColor = GetThemeColor(theme, ThemeElement.ControlText);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void RegisterControlHandlers()
        {
            RegisterControlHandler(typeof(Button), ThemeButtonHandler);                 // ready +
            RegisterControlHandler(typeof(CheckBox), ThemeCheckBoxHandler);             // ?
            RegisterControlHandler(typeof(ComboBox), ThemeComboBoxHandler);             // ?
            RegisterControlHandler(typeof(ContextMenuStrip), ThemeToolStripHandler);    // ?
            RegisterControlHandler(typeof(DataGridView), ThemeDataGridViewHandler);     // ?
            RegisterControlHandler(typeof(Form), ThemeFormHandler);                     // ?
            RegisterControlHandler(typeof(GroupBox), ThemeGroupBoxHandler);             // ?
            RegisterControlHandler(typeof(Label), ThemeLabelHandler);                   // ready +
            RegisterControlHandler(typeof(ListBox), ThemeListBoxHandler);               // ? (only plugins, not host)
            RegisterControlHandler(typeof(ListView), ThemeListViewHandler);             // ?
            RegisterControlHandler(typeof(MaskedTextBox), ThemeTextBoxHandler);         // ?
            RegisterControlHandler(typeof(MenuStrip), ThemeToolStripHandler);           // ?
            RegisterControlHandler(typeof(NumericUpDown), ThemeNumericUpDownHandler);   // ?
            RegisterControlHandler(typeof(Panel), ThemePanelHandler);                   // ?
            RegisterControlHandler(typeof(PictureBox), ThemePictureBoxHandler);         // ?
            RegisterControlHandler(typeof(ProgressBar), ThemeProgressBarHandler);       // ?
            RegisterControlHandler(typeof(RadioButton), ThemeRadioButtonHandler);       // ?
            RegisterControlHandler(typeof(RichTextBox), ThemeTextBoxHandler);           // ?
            RegisterControlHandler(typeof(ScrollBar), ThemeScrollBarHandler);           // ?
            RegisterControlHandler(typeof(SplitContainer), ThemeBaseControlHandler);    // ?
            RegisterControlHandler(typeof(StatusBar), ThemeStatusBarHandler);           // ?
            RegisterControlHandler(typeof(StatusBarPanel), ThemeStatusBarPanelHandler); // ?
            RegisterControlHandler(typeof(StatusStrip), ThemeToolStripHandler);         // ?
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
            RegisterControlHandler(typeof(ToolStripStatusLabel), ThemeToolStripItemHandler);    // ?
            RegisterControlHandler(typeof(TrackBar), ThemeBaseControlHandler);                  // ?
            RegisterControlHandler(typeof(TreeView), ThemeTreeViewHandler);             // ?

            RegisterControlHandler(typeof(MenuItemEx), ThemeToolStripItemHandler);      // ?

            RegisterControlHandler(typeof(ArborViewer), ThemeUserControlHandler);       // ?
            RegisterControlHandler(typeof(FilterGridView), FilterGridViewHandler);      // ?
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
            RegisterControlHandler(typeof(CustomChart), ThemeCustomChartHandler);       // ?
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

            private Color GetThemeColor(ThemeElement element)
            {
                object elemValue;
                if (fTheme.Elements.TryGetValue(element, out elemValue) && elemValue is Color) {
                    return (Color)elemValue;
                }

                return Color.Black;
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

            public override Color MenuBorder
            {
                get { return GetThemeColor(ThemeElement.MenuBorder); }
            }

            public override Color MenuItemBorder
            {
                get { return GetThemeColor(ThemeElement.MenuBorder); }
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

            public override Color MenuItemSelected
            {
                get { return GetThemeColor(ThemeElement.MenuItemSelected); }
            }

            public override Color MenuItemSelectedGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
            }
            public override Color MenuItemSelectedGradientEnd
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

            public override Color SeparatorDark
            {
                get { return Color.Red; }
            }

            public override Color ToolStripContentPanelGradientBegin
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }
            public override Color ToolStripContentPanelGradientEnd
            {
                get { return GetThemeColor(ThemeElement.Strip); }
            }

            public override Color ToolStripDropDownBackground
            {
                get { return GetThemeColor(ThemeElement.Dropdown); }
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
