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
using System.IO;
using System.Linq;
using BSLib;
using GKCore;
using GKCore.Design.Graphics;

namespace GKUI.Themes
{
    public abstract class BaseThemeManager : IThemeManager
    {
        protected const string DefaultThemeName = "Default";

        protected static ThemeElementType[] fThemeElementTypes;

        protected static Theme fCurrentTheme;

        protected Dictionary<string, Theme> fThemes = new Dictionary<string, Theme>();

        public List<Theme> Themes
        {
            get { return fThemes.Values.ToList(); }
        }

        public BaseThemeManager()
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
                ThemeElementType.Image, // ThemeElement.Glyph_PartialView

                ThemeElementType.Image, // ThemeElement.Glyph_Accept
                ThemeElementType.Image, // ThemeElement.Glyph_Cancel

                ThemeElementType.Image, // ThemeElement.Glyph_ItemAdd,
                ThemeElementType.Image, // ThemeElement.Glyph_ItemEdit,
                ThemeElementType.Image, // ThemeElement.Glyph_ItemDelete,
                ThemeElementType.Image, // ThemeElement.Glyph_LinkJump,
                ThemeElementType.Image, // ThemeElement.Glyph_MoveUp,
                ThemeElementType.Image, // ThemeElement.Glyph_MoveDown,
                ThemeElementType.Image, // ThemeElement.Glyph_Copy,
                ThemeElementType.Image, // ThemeElement.Glyph_Cut,
                ThemeElementType.Image, // ThemeElement.Glyph_Paste,

                ThemeElementType.Image, // ThemeElement.Glyph_ImageSave,
                ThemeElementType.Image, // ThemeElement.Glyph_DocPrint,
                ThemeElementType.Image, // ThemeElement.Glyph_DocPreview,

                ThemeElementType.Image, // ThemeElement.Glyph_Start,
                ThemeElementType.Image, // ThemeElement.Glyph_Stop,

                ThemeElementType.Image, // ThemeElement.Glyph_Undo,
                ThemeElementType.Image, // ThemeElement.Glyph_Redo,

                ThemeElementType.Image, // ThemeElement.Glyph_Attach,
                ThemeElementType.Image, // ThemeElement.Glyph_Detach,

                ThemeElementType.Image, // ThemeElement.Glyph_SizeToFit,
                ThemeElementType.Image, // ThemeElement.Glyph_ZoomIn,
                ThemeElementType.Image, // ThemeElement.Glyph_ZoomOut,
                ThemeElementType.Image, // ThemeElement.Glyph_SetPortrait,
            };
        }

        protected static string GetThemesPath()
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

        protected void Load(string fileName)
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
                            tcVal = SysUtils.ParseColor(tfc.Value);
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

        protected abstract object PreProcessElement(object telVal, ThemeElementType telType);

        private ThemeElementsDictionary PreProcessElements(ThemeElementsDictionary elements, bool sysDefault)
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
                            if (sysDefault || imgName.StartsWith("Resources.")) {
                                telVal = AppHost.GfxProvider.LoadResourceImage(imgName, ImageTarget.UI);
                            } else {
                                telVal = AppHost.GfxProvider.LoadImage(GetThemesPath() + imgName);
                            }
                        }
                    } catch (Exception ex) {
                        Logger.WriteError("PreProcessElements()", ex);
                        telVal = null;
                    }
                } else if (telType == ThemeElementType.Color) {
                    telVal = PreProcessElement(telVal, telType);
                }

                result.Add(telem, telVal);
            }
            return result;
        }

        protected void RegisterTheme(string name, ThemeElementsDictionary elements, bool sysDefault = false)
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

        public abstract void ApplyTheme(IThemedView view);

        public abstract void ApplyTheme(IThemedView view, object component);

        private static IImage GetThemeImageInt(Theme theme, ThemeElement element)
        {
            object elemValue;
            if (theme != null && theme.Elements.TryGetValue(element, out elemValue) && elemValue is IImage) {
                return (IImage)elemValue;
            }
            return null;
        }

        public IImage GetThemeImage(ThemeElement element, bool require = false)
        {
            IImage result = GetThemeImageInt(fCurrentTheme, element);

            if (result == null && require && fThemes.TryGetValue(DefaultThemeName, out Theme defTheme)) {
                result = GetThemeImageInt(defTheme, element);
            }

            return result;
        }

        protected static string GetThemeStr(Theme theme, ThemeElement element)
        {
            object elemValue;
            if (theme != null && theme.Elements.TryGetValue(element, out elemValue) && elemValue is string) {
                return (string)elemValue;
            }
            return string.Empty;
        }

        protected static float GetThemeFloat(Theme theme, ThemeElement element)
        {
            object elemValue;
            if (theme != null && theme.Elements.TryGetValue(element, out elemValue) && elemValue is float) {
                return (float)elemValue;
            }
            return 0.0f;
        }
    }
}
