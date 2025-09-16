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
using System.Runtime.InteropServices;
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

        private static readonly Dictionary<Type, ThemeControlHandler> fControlHandlers = new Dictionary<Type, ThemeControlHandler>();

        static EtoThemeManager()
        {
            RegisterControlHandlers();
        }

        public EtoThemeManager()
        {
            RegisterTheme(DefaultThemeName, new ThemeElementsDictionary() {
                { ThemeElement.None, "" },

                { ThemeElement.Font, "Tahoma" },
                { ThemeElement.FontSize, 9.0f },

                { ThemeElement.Editor, SystemColors.ControlBackground },
                { ThemeElement.EditorText, SystemColors.ControlText },

                { ThemeElement.Control, SystemColors.Control },
                { ThemeElement.ControlText, SystemColors.ControlText },

                { ThemeElement.Window, SystemColors.WindowBackground },
                { ThemeElement.WindowText, SystemColors.ControlText },

                { ThemeElement.Dialog, SystemColors.WindowBackground },
                { ThemeElement.DialogText, SystemColors.ControlText },

                { ThemeElement.ButtonFace, Color.FromArgb(0xDDDDDD) },
                { ThemeElement.AccentButtonFace, Color.FromArgb(0xDDDDDD) },
                { ThemeElement.ButtonBorder, Colors.Black },
                { ThemeElement.ButtonText, SystemColors.ControlText },

                { ThemeElement.Strip, SystemColors.Control },
                { ThemeElement.Dropdown, SystemColors.Control },
                { ThemeElement.MenuBorder, SystemColors.Control },
                { ThemeElement.MenuItemSelected, SystemColors.Control },

                { ThemeElement.Link, Colors.Blue },

                { ThemeElement.Grid, SystemColors.ControlBackground },
                { ThemeElement.GridHeader, SystemColors.WindowBackground },
                { ThemeElement.GridHeaderText, SystemColors.WindowBackground },
                { ThemeElement.GridText, SystemColors.ControlText },

                { ThemeElement.Tab, SystemColors.WindowBackground },
                { ThemeElement.TabHighlight, SystemColors.WindowBackground },
                { ThemeElement.TabSelected, SystemColors.Control },

                { ThemeElement.HighlightReadabilityRows, Color.FromArgb(0xEFEFEF) },
                { ThemeElement.HighlightUnparentedIndi, Color.FromArgb(0xFFCACA) },
                { ThemeElement.HighlightUnmarriedIndi, Color.FromArgb(0xFFFFA1) },
                { ThemeElement.HighlightInaccessibleFiles, Color.FromArgb(0xFFCACA) },

                { ThemeElement.ToolItemsImageSize, 20 },

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
                { ThemeElement.Glyph_CopyName, "Resources.btn_copy.gif" },

                { ThemeElement.Glyph_GenderFemale, "Resources.sym_female.png" },
                { ThemeElement.Glyph_GenderMale, "Resources.sym_male.png" },
                { ThemeElement.Glyph_Info, "Resources.btn_info.gif" },
                { ThemeElement.Glyph_Bookmark, "Resources.tg_bookmark.png" },
                { ThemeElement.Glyph_Expand, "Resources.btn_expand.gif" },
                { ThemeElement.Glyph_Expand2, "Resources.btn_expand2.gif" },

                { ThemeElement.Glyph_ShieldNone, "Resources.rg_shield_none.gif" },
                { ThemeElement.Glyph_ShieldMid, "Resources.rg_shield_mid.gif" },
                { ThemeElement.Glyph_ShieldMax, "Resources.rg_shield_max.gif" },

                { ThemeElement.Glyph_GEDNew, "Resources.btn_create_new.gif" },
                { ThemeElement.Glyph_GEDLoad, "Resources.btn_load.gif" },
                { ThemeElement.Glyph_GEDSave, "Resources.btn_save.gif" },

                { ThemeElement.Glyph_Scripts, "" },
                { ThemeElement.Glyph_FindAndReplace, "" },
                { ThemeElement.Glyph_View, "Resources.btn_view.png" },
            }, true);
        }

        private static Font fCachedFont = null;

        public override bool SetTheme(string name)
        {
            var result = base.SetTheme(name);

            if (result) {
                GKData.HighlightReadabilityRows = GetThemeColor(fCurrentTheme, ThemeElement.HighlightReadabilityRows).ToArgb();
                GKData.HighlightUnparentedColor = GetThemeColor(fCurrentTheme, ThemeElement.HighlightUnparentedIndi).ToArgb();
                GKData.HighlightUnmarriedColor = GetThemeColor(fCurrentTheme, ThemeElement.HighlightUnmarriedIndi).ToArgb();
                GKData.HighlightInaccessibleFiles = GetThemeColor(fCurrentTheme, ThemeElement.HighlightInaccessibleFiles).ToArgb();

                if (fCachedFont != null) {
                    fCachedFont.Dispose();
                    fCachedFont = null;
                }

                var themeFont = GetThemeStr(fCurrentTheme, ThemeElement.Font);
                // FIXME: OS specifics
                if (Application.Instance.Platform.IsGtk) {
                    themeFont = "Sans";
                }

                var themeFontSize = GetThemeFloat(fCurrentTheme, ThemeElement.FontSize);
                fCachedFont = new Font(themeFont, themeFontSize);
            }

            return result;
        }

        public override void ApplyTheme(IThemedView view)
        {
            if (view == null || fCurrentTheme == null) return;

            var form = view as Window;
            if (form != null) {
                form.SuspendLayout();
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
                if (telVal is int intVal) {
                    telVal = Color.FromArgb(intVal);
                }
                return telVal;
            } else {
                return telVal;
            }
        }

        #region Control handlers

        private static void ApplyTheme(IThemedView view, IDisposable component, Theme theme)
        {
            if (theme == null || (view is IThemedForm themedForm && themedForm.SkipTheme(component)))
                return;

            ThemeControlHandler handler = GetControlHandler(component);
            handler?.Invoke(view, component, theme);

            if (component is Container ctl) {
                foreach (var item in ctl.Controls) {
                    ApplyTheme(view, item, theme);
                }
            }

            if (!(component is IThemedForm) && component is IThemedView themedView) {
                themedView.ApplyTheme();
            }
        }

        private static Color GetThemeColor(Theme theme, ThemeElement element)
        {
            object elemValue;
            if (theme != null && theme.Elements.TryGetValue(element, out elemValue) && elemValue is Color clrValue) {
                return clrValue;
            }
            return Colors.Black;
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

            if (ctl.Menu != null)
                ThemeMenuBarHandler(view, ctl.Menu, theme);

            if (ctl.ToolBar != null)
                ThemeToolBarHandler(view, ctl.ToolBar, theme);
        }

        private static void ThemeButtonHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Button)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
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
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeComboBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (ComboBox)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemeGridViewHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (GridView)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Grid);
            //ctl.TextColor = GetThemeColor(theme, ThemeElement.GridText);
            //ctl.ColumnHeadersDefaultCellStyle.BackgroundColor = GetThemeColor(theme, ThemeElement.GridHeader);
            //ctl.ColumnHeadersDefaultCellStyle.TextColor = GetThemeColor(theme, ThemeElement.GridHeaderText);

            if (component is GKListView gkGrid) {
                if (fCachedFont != null) gkGrid.Font = fCachedFont;
                gkGrid.TextColor = GetThemeColor(theme, ThemeElement.GridText);
            }

            // Fix for update rows formatting
            ctl.ReloadData(ctl.SelectedRows);

            ThemeContextMenuHostHandler(view, component, theme);
        }

        private static void ThemeFilterGridViewHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (FilterGridView)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Grid);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.GridText);

            ThemeContextMenuHostHandler(view, component, theme);
        }

        private static void ThemeTreeViewHandler(IThemedView view, IDisposable component, Theme theme)
        {
            try {
                var ctl = (TreeView)component;
                //if (fCachedFont != null) ctl.Font = fCachedFont;
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
                ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);

                ThemeContextMenuHostHandler(view, component, theme);
            } catch { }
        }

        private static void ThemeGroupBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            try {
                var ctl = (GroupBox)component;
                if (fCachedFont != null) ctl.Font = fCachedFont;
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
                ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
            } catch { }
        }

        private static void ThemeHyperViewHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (HyperView)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            SpecOSControlBackground(ctl, theme);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
            ctl.LinkColor = GetThemeColor(theme, ThemeElement.Link);

            ThemeContextMenuHostHandler(view, component, theme);
        }

        private static void ThemeCustomChartHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (CustomChart)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);

            ThemeContextMenuHostHandler(view, component, theme);
        }

        private static void GetParentDependentColors(Control control, Theme theme, out Color backgroundColor, out Color textColor)
        {
            var ctlParent = control.Parent;

            backgroundColor = ctlParent.BackgroundColor;

            if (ctlParent is Window) {
                if (ctlParent is not Dialog) {
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
            if (fCachedFont != null) ctl.Font = fCachedFont;

            Color backgroundColor, textColor;
            GetParentDependentColors(ctl, theme, out backgroundColor, out textColor);

            ctl.BackgroundColor = backgroundColor;
            ctl.TextColor = textColor;
        }

        private static void ThemeListBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (ListBox)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemeNumericStepperHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (NumericStepper)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);
        }

        private static void ThemeRadioButtonHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (RadioButton)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = ctl.Parent.BackgroundColor;
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeTextBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (TextControl)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);

            ThemeContextMenuHostHandler(view, component, theme);
        }

        private static void ThemeDateBoxHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (GKDateBox)component;
            if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Editor);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.EditorText);

            ThemeContextMenuHostHandler(view, component, theme);
        }

        private static void ThemeTabControlHandler(IThemedView view, IDisposable component, Theme theme)
        {
            // In Eto, TabControl's BackgroundColor - its color of border around of tabPage!

            if (component is GKTabControl gkTab) {
                // extended
                if (fCachedFont != null) gkTab.Font = fCachedFont;
                gkTab.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
                //ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);

                /*
                    ctl.Appearance.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
                    ctl.Appearance.Tab = GetThemeColor(theme, ThemeElement.Tab);
                    ctl.Appearance.TabHighlight = GetThemeColor(theme, ThemeElement.TabHighlight);
                    ctl.Appearance.TabSelected = GetThemeColor(theme, ThemeElement.TabSelected);
                */
            } else {
                // standard
                var ctl = (TabControl)component;
                //if (fCachedFont != null) ctl.Font = fCachedFont;
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
                //ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
            }
        }

        private static void ThemeTabPageHandler(IThemedView view, IDisposable component, Theme theme)
        {
            // In Eto, TabPage's BackgroundColor - its color of unselected Tabs!

            var ctl = (TabPage)component;
            //if (fCachedFont != null) ctl.Font = fCachedFont;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.TabHighlight);
            //ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);
        }

        private static void ThemeContextMenuHostHandler(IThemedView view, IDisposable component, Theme theme)
        {
            if (component is IContextMenuHost ctl && ctl.ContextMenu != null)
                ThemeContextMenuHandler(view, ctl.ContextMenu, theme);
        }

        private static void ThemeContextMenuHandler(IThemedView view, IDisposable component, Theme theme)
        {
            if (component is GKContextMenu gkMenu) {
                if (fCachedFont != null) gkMenu.Font = fCachedFont;
            }
        }

        private static void ThemeToolBarHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (ToolBar)component;

            if (component is GKToolBar gkToolBar) {
                if (fCachedFont != null) gkToolBar.Font = fCachedFont;
                //gkToolBar.BackgroundColor = GetThemeColor(theme, ThemeElement.Strip);
                //gkToolBar.TextColor = GetThemeColor(theme, ThemeElement.ButtonText);
            }

            foreach (var item in ctl.Items) {
                if (item is GKDropDownToolItem dropdownItem) {
                    ThemeContextMenuHandler(view, dropdownItem.ContextMenu, theme);
                }
            }
        }

        private static void ThemeMenuBarHandler(IThemedView view, IDisposable component, Theme theme)
        {
            if (component is GKMenuBar gkMenu) {
                if (fCachedFont != null) gkMenu.Font = fCachedFont;
                //gkMenu.BackgroundColor = GetThemeColor(theme, ThemeElement.Strip);
                //gkMenu.TextColor = GetThemeColor(theme, ThemeElement.ButtonText);
            }
        }

        private static void ThemeEmptyHandler(IThemedView view, IDisposable component, Theme theme)
        {
            /*var ctl = (MenuItemEx)component;
            ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Dropdown);
            ctl.TextColor = GetThemeColor(theme, ThemeElement.ButtonText);*/
        }

        private static void SpecOSControlBackground(Control ctl, Theme theme)
        {
            if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux)) {
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Window);
            } else if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) {
                ctl.BackgroundColor = GetThemeColor(theme, ThemeElement.Control);
            }
        }

        private static void ThemeBaseControlHandler(IThemedView view, IDisposable component, Theme theme)
        {
            var ctl = (Control)component;
            //if (fCachedFont != null) ctl.Font = fCachedFont;
            SpecOSControlBackground(ctl, theme);
            //ctl.TextColor = GetThemeColor(theme, ThemeElement.ControlText);

            ThemeContextMenuHostHandler(view, component, theme);
        }

        private static void RegisterControlHandlers()
        {
            RegisterControlHandler(typeof(Button), ThemeButtonHandler);
            RegisterControlHandler(typeof(CheckBox), ThemeCheckBoxHandler);
            RegisterControlHandler(typeof(ComboBox), ThemeComboBoxHandler);
            RegisterControlHandler(typeof(ContextMenu), ThemeContextMenuHandler);
            RegisterControlHandler(typeof(GridView), ThemeGridViewHandler);
            RegisterControlHandler(typeof(Window), ThemeFormHandler);
            RegisterControlHandler(typeof(GroupBox), ThemeGroupBoxHandler);
            RegisterControlHandler(typeof(Label), ThemeLabelHandler);
            RegisterControlHandler(typeof(ListBox), ThemeListBoxHandler);
            RegisterControlHandler(typeof(MaskedTextBox), ThemeTextBoxHandler);
            RegisterControlHandler(typeof(MenuBar), ThemeMenuBarHandler);
            RegisterControlHandler(typeof(NumericStepper), ThemeNumericStepperHandler);
            RegisterControlHandler(typeof(Panel), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(PictureBox), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(ProgressBar), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(RadioButton), ThemeRadioButtonHandler);
            RegisterControlHandler(typeof(RichTextArea), ThemeTextBoxHandler);
            RegisterControlHandler(typeof(Splitter), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(TableLayout), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(TabControl), ThemeTabControlHandler);
            RegisterControlHandler(typeof(TabPage), ThemeTabPageHandler);
            RegisterControlHandler(typeof(TextBox), ThemeTextBoxHandler);
            RegisterControlHandler(typeof(ToolBar), ThemeToolBarHandler);
            RegisterControlHandler(typeof(Slider), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(TreeView), ThemeTreeViewHandler);

            RegisterControlHandler(typeof(MenuItemEx), ThemeEmptyHandler);
            RegisterControlHandler(typeof(ArborViewer), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(FilterGridView), ThemeFilterGridViewHandler);
            RegisterControlHandler(typeof(GKTabControl), ThemeTabControlHandler);
            RegisterControlHandler(typeof(GKMenuBar), ThemeMenuBarHandler);
            RegisterControlHandler(typeof(GKComboBox), ThemeComboBoxHandler);
            RegisterControlHandler(typeof(GKDateBox), ThemeDateBoxHandler);
            RegisterControlHandler(typeof(GKDateControl), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(GKListView), ThemeGridViewHandler);
            RegisterControlHandler(typeof(GKPortrait), ThemeBaseControlHandler);
            //RegisterControlHandler(typeof(GKTextBox), ThemeTextBoxHandler);
            RegisterControlHandler(typeof(HyperView), ThemeHyperViewHandler);
            RegisterControlHandler(typeof(ImageBox), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(Components.ImageView), ThemeBaseControlHandler);
            RegisterControlHandler(typeof(LogChart), ThemeBaseControlHandler);

            RegisterControlHandler(typeof(CustomChart), ThemeCustomChartHandler);
            RegisterControlHandler(typeof(TreeChartBox), ThemeCustomChartHandler);
            RegisterControlHandler(typeof(CircleChart), ThemeCustomChartHandler);
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
