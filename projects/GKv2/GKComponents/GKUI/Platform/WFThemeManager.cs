/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using GKCore.Design.Graphics;
using GKUI.Components;
using GKUI.Platform;
using GKUI.Platform.Handlers;

namespace GKUI.Themes
{
    public sealed class WFThemeManager : IThemeManager
    {
        private delegate void ThemeControlHandler(IThemedView view, Component component, Theme theme);

        private static Theme fCurrentTheme;
        private static Dictionary<Type, ThemeControlHandler> fControlHandlers = new Dictionary<Type, ThemeControlHandler>();
        private static ThemeElementType[] fThemeElementTypes;

        private Dictionary<string, Theme> fThemes = new Dictionary<string, Theme>();

        public List<Theme> Themes
        {
            get { return fThemes.Values.ToList(); }
        }

        static WFThemeManager()
        {
            RegisterControlHandlers();
        }

        public WFThemeManager()
        {
            fThemeElementTypes = new ThemeElementType[] {
                ThemeElementType.String, // ThemeElement.Font
                ThemeElementType.Float,  // ThemeElement.FontSize

                ThemeElementType.Color, // ThemeElement.Editor
                ThemeElementType.Color, // ThemeElement.EditorText

                ThemeElementType.Color, // ThemeElement.Control
                ThemeElementType.Color, // ThemeElement.ControlText

                ThemeElementType.Color, // ThemeElement.Window
                ThemeElementType.Color, // ThemeElement.WindowText

                ThemeElementType.Color, // ThemeElement.Dialog
                ThemeElementType.Color, // ThemeElement.DialogText

                ThemeElementType.Color, // ThemeElement.ButtonFace
                ThemeElementType.Color, // ThemeElement.AccentButtonFace
                ThemeElementType.Color, // ThemeElement.ButtonBorder
                ThemeElementType.Color, // ThemeElement.ButtonText

                ThemeElementType.Color, // ThemeElement.Strip
                ThemeElementType.Color, // ThemeElement.Dropdown
                ThemeElementType.Color, // ThemeElement.MenuBorder
                ThemeElementType.Color, // ThemeElement.MenuItemSelected

                ThemeElementType.Color, // ThemeElement.Link

                ThemeElementType.Color, // ThemeElement.Grid
                ThemeElementType.Color, // ThemeElement.GridHeader
                ThemeElementType.Color, // ThemeElement.GridHeaderText
                ThemeElementType.Color, // ThemeElement.GridText

                ThemeElementType.Color, // ThemeElement.Tab
                ThemeElementType.Color, // ThemeElement.TabHighlight
                ThemeElementType.Color, // ThemeElement.TabSelected

                ThemeElementType.Color, // ThemeElement.HighlightReadabilityRows
                ThemeElementType.Color, // ThemeElement.HighlightUnparentedIndi
                ThemeElementType.Color, // ThemeElement.HighlightUnmarriedIndi
                ThemeElementType.Color, // ThemeElement.HighlightInaccessibleFiles

                ThemeElementType.Image, // ThemeElement.Glyph_FileNew
                ThemeElementType.Image, // ThemeElement.Glyph_FileLoad
                ThemeElementType.Image, // ThemeElement.Glyph_FileSave
                ThemeElementType.Image, // ThemeElement.Glyph_FileProperties,
                ThemeElementType.Image, // ThemeElement.Glyph_Export,
                ThemeElementType.Image, // ThemeElement.Glyph_ExportTable,
                ThemeElementType.Image, // ThemeElement.Glyph_Exit,

                ThemeElementType.Image, // ThemeElement.Glyph_RecordAdd
                ThemeElementType.Image, // ThemeElement.Glyph_RecordEdit
                ThemeElementType.Image, // ThemeElement.Glyph_RecordDelete
                ThemeElementType.Image, // ThemeElement.Glyph_Search
                ThemeElementType.Image, // ThemeElement.Glyph_Filter

                ThemeElementType.Image, // ThemeElement.Glyph_TreeAncestors
                ThemeElementType.Image, // ThemeElement.Glyph_TreeDescendants
                ThemeElementType.Image, // ThemeElement.Glyph_TreeBoth
                ThemeElementType.Image, // ThemeElement.Glyph_Pedigree
                ThemeElementType.Image, // ThemeElement.Glyph_Maps
                ThemeElementType.Image, // ThemeElement.Glyph_Stats

                ThemeElementType.Image, // ThemeElement.Glyph_Organizer
                ThemeElementType.Image, // ThemeElement.Glyph_Slideshow
                ThemeElementType.Image, // ThemeElement.Glyph_Settings

                ThemeElementType.Image, // ThemeElement.Glyph_Help
                ThemeElementType.Image, // ThemeElement.Glyph_About

                ThemeElementType.Image, // ThemeElement.Glyph_Prev
                ThemeElementType.Image, // ThemeElement.Glyph_Next
                ThemeElementType.Image, // ThemeElement.Glyph_SendMail
            };


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
                { ThemeElement.Glyph_Stats, "Resources.btn_table.gif" },

                { ThemeElement.Glyph_Organizer, "Resources.btn_organizer.gif" },
                { ThemeElement.Glyph_Slideshow, "Resources.btn_slideshow.png" },
                { ThemeElement.Glyph_Settings, "Resources.btn_tools.gif" },

                { ThemeElement.Glyph_Help, "Resources.btn_help.gif" },
                { ThemeElement.Glyph_About, "Resources.btn_scroll.gif" },

                { ThemeElement.Glyph_Prev, "Resources.btn_left.gif" },
                { ThemeElement.Glyph_Next, "Resources.btn_right.gif" },
                { ThemeElement.Glyph_SendMail, "Resources.btn_mail.gif" },
            }, true);
        }

        private static string GetThemesPath()
        {
            return GKUtils.GetAppPath() + "themes" + Path.DirectorySeparatorChar;
        }

        public void LoadThemes()
        {
            string path = GetThemesPath();
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

        private void Load(string fileName)
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
                    var telem = EnumHelper.Parse<ThemeElement>(tfc.Element);
                    var telType = fThemeElementTypes[(int)telem];

                    object tcVal;
                    switch (telType) {
                        case ThemeElementType.Float:
                            tcVal = (float)ConvertHelper.ParseFloat(tfc.Value, 8, true);
                            break;

                        case ThemeElementType.Color:
                            tcVal = UIHelper.ParseColor(tfc.Value);
                            break;

                        case ThemeElementType.Image:
                        case ThemeElementType.String:
                        default:
                            tcVal = tfc.Value;
                            break;
                    }

                    themeElements.Add(telem, tcVal);
                }

                RegisterTheme(themeFile.Name, themeElements, false);
            } catch (Exception ex) {
                Logger.WriteError("ThemeManager.Load()", ex);
            }
        }

        private static ThemeElementsDictionary PreProcessElements(ThemeElementsDictionary elements, bool sysDefault)
        {
            var result = new ThemeElementsDictionary();
            foreach (var kvp in elements) {
                var telem = kvp.Key;
                var telType = fThemeElementTypes[(int)telem];
                object telVal = kvp.Value;

                if (telType == ThemeElementType.Image) {
                    try {
                        string imgName = telVal.ToString();
                        if (!string.IsNullOrEmpty(imgName)) {
                            if (sysDefault) {
                                telVal = new ImageHandler(UIHelper.LoadResourceImage(imgName));
                            } else {
                                telVal = AppHost.GfxProvider.LoadImage(GetThemesPath() + imgName);
                            }
                        }
                    } catch (Exception ex) {
                        Logger.WriteError("PreProcessElements()", ex);
                        telVal = null;
                    }
                }

                result.Add(telem, telVal);
            }
            return result;
        }

        private void RegisterTheme(string name, ThemeElementsDictionary elements, bool sysDefault = false)
        {
            fThemes.Add(name, new Theme(name, PreProcessElements(elements, sysDefault), sysDefault));
        }

        public void SetTheme(string name)
        {
            Theme theme;
            if (fThemes.TryGetValue(name, out theme)) {
                fCurrentTheme = theme;
            }
        }

        public void ApplyTheme(IThemedView view)
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

        public void ApplyTheme(IThemedView view, object component)
        {
            ApplyTheme(view, (Component)component, fCurrentTheme);
        }

        public IImage GetThemeImage(ThemeElement element)
        {
            object elemValue;
            if (fCurrentTheme != null && fCurrentTheme.Elements.TryGetValue(element, out elemValue) && elemValue is IImage) {
                return (IImage)elemValue;
            }
            return null;
        }

        #region Control handlers

        private static void ApplyTheme(IThemedView view, Component component, Theme theme)
        {
            if (theme == null || view.SkipTheme(component))
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

        private static Color GetThemeColor(Theme theme, ThemeElement element)
        {
            object elemValue;
            if (theme != null && theme.Elements.TryGetValue(element, out elemValue) && elemValue is Color) {
                return (Color)elemValue;
            }
            return Color.Black;
        }

        private static string GetThemeStr(Theme theme, ThemeElement element)
        {
            object elemValue;
            if (theme != null && theme.Elements.TryGetValue(element, out elemValue) && elemValue is string) {
                return (string)elemValue;
            }
            return string.Empty;
        }

        private static float GetThemeFloat(Theme theme, ThemeElement element)
        {
            object elemValue;
            if (theme != null && theme.Elements.TryGetValue(element, out elemValue) && elemValue is float) {
                return (float)elemValue;
            }
            return 0.0f;
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
