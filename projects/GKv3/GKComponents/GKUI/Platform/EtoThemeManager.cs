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

#pragma warning disable CS0618

using System;
using System.Collections.Generic;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKUI.Components;
using GKUI.Platform;

namespace GKUI.Themes
{
    public sealed class EtoThemeManager : BaseThemeManager
    {
        private delegate void ThemeControlHandler(IThemedView view, IDisposable component, Theme theme);

        private static Dictionary<Type, ThemeControlHandler> fControlHandlers = new Dictionary<Type, ThemeControlHandler>();

        static EtoThemeManager()
        {
            RegisterControlHandlers();
        }

        public EtoThemeManager()
        {
            RegisterTheme(DefaultThemeName, new ThemeElementsDictionary() {
                { ThemeElement.Font, "Tahoma" },                              // ???
                { ThemeElement.FontSize, 8.25f },                             // ???

                { ThemeElement.Editor, SystemColors.ControlBackground },      // ???
                { ThemeElement.EditorText, SystemColors.ControlText },        // ???

                { ThemeElement.Control, Color.FromArgb(0x00000000) },         // default?! win+
                { ThemeElement.ControlText, SystemColors.ControlText },       // ???

                { ThemeElement.Window, SystemColors.Control },                // ???
                { ThemeElement.WindowText, SystemColors.ControlText },        // ???

                { ThemeElement.Dialog, SystemColors.Control },                // ???
                { ThemeElement.DialogText, SystemColors.ControlText },        // ???

                { ThemeElement.ButtonFace, Color.FromArgb(0xDDDDDD) },        // default?! win+
                { ThemeElement.AccentButtonFace, Color.FromArgb(0xDDDDDD) },  // default?! win+
                { ThemeElement.ButtonBorder, Colors.Black },                  // ???
                { ThemeElement.ButtonText, SystemColors.ControlText },        // ???

                { ThemeElement.Strip, SystemColors.Control },                 // ???
                { ThemeElement.Dropdown, SystemColors.Control },              // ???
                { ThemeElement.MenuBorder, SystemColors.Control },            // ???
                { ThemeElement.MenuItemSelected, SystemColors.Control },      // ???

                { ThemeElement.Link, Colors.Blue },                            // ???

                { ThemeElement.Grid, SystemColors.Control },                   // ???
                { ThemeElement.GridHeader, SystemColors.WindowBackground },    // ???
                { ThemeElement.GridHeaderText, SystemColors.WindowBackground },// ???
                { ThemeElement.GridText, SystemColors.ControlText },           // ???

                { ThemeElement.Tab, SystemColors.WindowBackground },           // ???
                { ThemeElement.TabHighlight, SystemColors.WindowBackground },  // ???
                { ThemeElement.TabSelected, SystemColors.Control },            // ???

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

            var form = view as Window;
            if (form != null) {
                form.SuspendLayout();

                var themeFont = GetThemeStr(fCurrentTheme, ThemeElement.Font);
                var themeFontSize = GetThemeFloat(fCurrentTheme, ThemeElement.FontSize);
                /*if (form.Font.Name != themeFont) {
                    form.Font = new Font(themeFont, themeFontSize);
                }*/

                ApplyTheme(view, form, fCurrentTheme);

                form.ResumeLayout();
            }
        }

        public override void ApplyTheme(IThemedView view, object component)
        {
            ApplyTheme(view, (IDisposable)component, fCurrentTheme);
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

        private static void ApplyTheme(IThemedView view, IDisposable component, Theme theme)
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

                //ctl.Font = ((Form)view).Font;

                var container = ctl as Container;
                if (container == null) return;
                foreach (var item in container.Controls) {
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
            return Colors.Black;
        }

        private static void ThemeButtonHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Button)component;
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ButtonText);

            var dialog = ctl.ParentWindow as Dialog;
            if (dialog == null) {
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.ButtonFace);
            } else {
                if (ctl == dialog.DefaultButton) {
                    ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.AccentButtonFace);
                } else {
                    ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.ButtonFace);
                }
            }
        }

        private static void ThemeCheckBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (CheckBox)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeComboBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (ComboBox)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemeDataGridViewHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (GridView)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Grid);
            //ctl.TextColor = GetThemeColor(theme, ThemeElement.GridText);
            //ctl.ColumnHeadersDefaultCellStyle.BackgroundColor = GetThemeColor(theme, ThemeElement.GridHeader);
            //ctl.ColumnHeadersDefaultCellStyle.TextColor = GetThemeColor(theme, ThemeElement.GridHeaderText);
            //ctl.EnableHeadersVisualStyles = (theme.SysDefault);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeFormHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Window)component;
            if (ctl is Dialog) {
                // dialog
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Dialog);
            } else {
                // window
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Window);
            }
        }

        private static void ThemeGroupBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (GroupBox)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeHyperViewHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (HyperView)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
            ctl.LinkColor = GetThemeColor(theme, ThemeElement.Link);
            //ctl.BorderStyle = (theme.SysDefault) ? BorderStyle.Fixed3D : BorderStyle.FixedSingle;

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeCustomChartHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (CustomChart)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
            //ctl.BorderStyle = (theme.SysDefault) ? BorderStyle.Fixed3D : BorderStyle.FixedSingle;

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void GetParentDependentColors(Control control, Theme theme, out Color backgroundColor, out Color textColor)
        {
            var ctlParent = control.Parent;

            backgroundColor = ctlParent.BackgroundColor;

            if (ctlParent is Window) {
                if (!(ctlParent is Dialog)) {
                    // window
                    textColor = GetThemeColor(theme, ThemeElement.WindowText);
                } else {
                    // dialog
                    textColor = GetThemeColor(theme, ThemeElement.DialogText);
                }
            } else {
                textColor = GetThemeColor(theme, ThemeElement.ControlText);
            }
        }

        private static void ThemeLabelHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Label)component;

            Color backgroundColor, textColor;
            GetParentDependentColors(ctl, theme, out backgroundColor, out textColor);

            ctl.BackgroundColor = backgroundColor;
            ctl.TextColor = textColor;
        }

        private static void ThemeListBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (ListBox)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemeListViewHandler(IThemedView view, IDisposable component, Theme theme)
        {
            if (component is GKListView) {
                // extended
                var ctl = (GKListView)component;
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Grid);
                //ctl.TextColor = GetThemeColor(theme, ThemeElement.GridText);

                /*if (theme.SysDefault) {
                    ctl.Appearance.Reset();
                } else {
                    ctl.Appearance.BackgroundColor = GetThemeColor(theme, ThemeElement.Grid);
                    ctl.Appearance.Header = GetThemeColor(theme, ThemeElement.GridHeader);
                    ctl.Appearance.HeaderText = GetThemeColor(theme, ThemeElement.GridHeaderText);
                }

                ctl.ResetCache();*/
            } else {
                // standard
                var ctl = (GridView)component;
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Grid);
                //ctl.TextColor = GetThemeColor(theme, ThemeElement.GridText);
            }

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeNumericStepperHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (NumericStepper)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemePanelHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Control)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemePictureBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Eto.Forms.ImageView)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
        }

        private static void ThemeProgressBarHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (ProgressBar)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            //ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeRadioButtonHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (RadioButton)component;
            ctl.BackgroundColor = ctl.Parent.BackgroundColor;
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeTextBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (TextControl)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);

            //ctl.BorderStyle = (!theme.SysDefault) ? BorderStyle.FixedSingle : BorderStyle.Fixed3D;

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeDateBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (GKDateBox)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);

            //ctl.BorderStyle = (!theme.SysDefault) ? BorderStyle.FixedSingle : BorderStyle.Fixed3D;

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeTabControlHandler(IThemedView view, IDisposable component, Theme theme)
        {
            if (component is GKTabControl) {
                // extended
                var ctl = (GKTabControl)component;
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
                //ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);

                /*if (theme.SysDefault) {
                    ctl.Appearance.Reset();
                } else {
                    ctl.Appearance.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
                    ctl.Appearance.Tab = GetThemeColor(theme, ThemeElement.Tab);
                    ctl.Appearance.TabHighlight = GetThemeColor(theme, ThemeElement.TabHighlight);
                    ctl.Appearance.TabSelected = GetThemeColor(theme, ThemeElement.TabSelected);
                }*/
            } else {
                // standard
                var ctl = (TabControl)component;
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
                //ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
            }
        }

        private static void ThemeTabPageHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (TabPage)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            //ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeContextMenuStripHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = component as IContextMenuHost;
            if (ctl != null && ctl.ContextMenu != null) {
                ThemeToolStripHandler(view, ctl.ContextMenu, theme);
            }
        }

        private static void ThemeToolStripHandler(IThemedView view, IDisposable component, Theme theme)
        {
            /*var ctl = (ToolBar)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Strip);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ButtonText);

            ctl.Renderer = (theme.SysDefault) ? new ToolStripProfessionalRenderer() : new TSRenderer(theme);

            foreach (ToolStripItem item in ctl.Items) {
                ApplyTheme(view, item, theme);
            }*/
        }

        private static void ThemeToolStripItemHandler(IThemedView view, IDisposable component, Theme theme)
        {
            /*var ctl = (MenuItemEx)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Dropdown);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ButtonText);

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
            }*/
        }

        private static void ThemeTreeViewHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (TreeView)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeUserControlHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Panel)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void ThemeBaseControlHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Control)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);

            ThemeContextMenuStripHandler(view, component, theme);
        }

        private static void RegisterControlHandlers()
        {
            RegisterControlHandler(typeof(Button), ThemeButtonHandler);
            RegisterControlHandler(typeof(CheckBox), ThemeCheckBoxHandler);
            RegisterControlHandler(typeof(ComboBox), ThemeComboBoxHandler);
            RegisterControlHandler(typeof(ContextMenu), ThemeToolStripHandler);
            RegisterControlHandler(typeof(GridView), ThemeDataGridViewHandler);
            RegisterControlHandler(typeof(Window), ThemeFormHandler);
            RegisterControlHandler(typeof(GroupBox), ThemeGroupBoxHandler);
            RegisterControlHandler(typeof(Label), ThemeLabelHandler);
            RegisterControlHandler(typeof(ListBox), ThemeListBoxHandler);
            RegisterControlHandler(typeof(MaskedTextBox), ThemeTextBoxHandler);
            RegisterControlHandler(typeof(MenuBar), ThemeToolStripHandler);
            RegisterControlHandler(typeof(NumericStepper), ThemeNumericStepperHandler);
            RegisterControlHandler(typeof(Panel), ThemePanelHandler);
            RegisterControlHandler(typeof(Eto.Forms.ImageView), ThemePictureBoxHandler);
            RegisterControlHandler(typeof(ProgressBar), ThemeProgressBarHandler);
            RegisterControlHandler(typeof(RadioButton), ThemeRadioButtonHandler);
            RegisterControlHandler(typeof(RichTextArea), ThemeTextBoxHandler);
            RegisterControlHandler(typeof(Splitter), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(TableLayout), ThemePanelHandler);
            RegisterControlHandler(typeof(TabControl), ThemeTabControlHandler);
            RegisterControlHandler(typeof(TabPage), ThemeTabPageHandler);
            RegisterControlHandler(typeof(TextBox), ThemeTextBoxHandler);
            RegisterControlHandler(typeof(ToolBar), ThemeToolStripHandler);
            //RegisterControlHandler(typeof(ToolStripButton), ThemeToolStripItemHandler);
            //RegisterControlHandler(typeof(ToolStripComboBox), ThemeToolStripItemHandler);
            //RegisterControlHandler(typeof(ToolStripDropDownButton), ThemeToolStripItemHandler);
            //RegisterControlHandler(typeof(ToolStripMenuItem), ThemeToolStripItemHandler);
            //RegisterControlHandler(typeof(ToolStripSeparator), ThemeToolStripItemHandler);
            //RegisterControlHandler(typeof(ToolStripStatusLabel), ThemeToolStripItemHandler);
            RegisterControlHandler(typeof(Slider), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(TreeView), ThemeTreeViewHandler);

            RegisterControlHandler(typeof(MenuItemEx), ThemeToolStripItemHandler);

            RegisterControlHandler(typeof(ArborViewer), ThemeUserControlHandler);
            RegisterControlHandler(typeof(FilterGridView), ThemeDataGridViewHandler);
            RegisterControlHandler(typeof(GKTabControl), ThemeTabControlHandler);
            RegisterControlHandler(typeof(GKComboBox), ThemeComboBoxHandler);
            RegisterControlHandler(typeof(GKDateBox), ThemeDateBoxHandler);
            RegisterControlHandler(typeof(GKDateControl), ThemeUserControlHandler);
            RegisterControlHandler(typeof(GKListView), ThemeListViewHandler);
            RegisterControlHandler(typeof(GKPortrait), ThemeUserControlHandler);
            //RegisterControlHandler(typeof(GKTextBox), ThemeTextBoxHandler);
            RegisterControlHandler(typeof(HyperView), ThemeHyperViewHandler);
            RegisterControlHandler(typeof(ImageBox), ThemePanelHandler);
            RegisterControlHandler(typeof(GKUI.Components.ImageView), ThemeUserControlHandler);
            RegisterControlHandler(typeof(LogChart), ThemePanelHandler);
            RegisterControlHandler(typeof(CustomChart), ThemeCustomChartHandler);
        }

        private static void RegisterControlHandler(Type controlType, ThemeControlHandler handler)
        {
            fControlHandlers.Add(controlType, handler);
        }

        private static ThemeControlHandler GetControlHandler(IDisposable component)
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


        #endregion
    }
}
