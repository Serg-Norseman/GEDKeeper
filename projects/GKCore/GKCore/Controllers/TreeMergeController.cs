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

using GKCore.Design.Controls;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Tools;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public class TreeMergeController : DialogController<ITreeMergeDlg>
    {
        public TreeMergeController(ITreeMergeDlg view) : base(view)
        {
        }

        public override void UpdateView()
        {
        }

        public async void Merge()
        {
            string fileName = await AppHost.StdDialogs.GetOpenFile("", "", LangMan.LS(LSID.GEDCOMFilter), 1, GKData.GEDCOM_EXT);
            if (string.IsNullOrEmpty(fileName)) return;

            fView.UpdateBase.Text = fileName;
            TreeTools.MergeTreeFile(fBase.Context.Tree, fileName, fView.SyncLog, true);
            fBase.Context.SetModified();
            fBase.RefreshLists(false);
        }

        public override void SetLocale()
        {
            fView.Title = LangMan.LS(LSID.TreeMerge);
            if (!AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                GetControl<ITabPage>("pageTreeMerge").Text = LangMan.LS(LSID.TreeMerge);
                GetControl<IButton>("btnClose").Text = LangMan.LS(LSID.DlgClose);
            }
            GetControl<IButton>("btnTreeMerge").Text = LangMan.LS(LSID.DlgSelect) + @"...";
            GetControl<ILabel>("lblMasterBase").Text = LangMan.LS(LSID.MasterBase);
            GetControl<ILabel>("lblOtherBase").Text = LangMan.LS(LSID.OtherBase);
            GetControl<ITextBox>("edMasterBase").Text = LangMan.LS(LSID.CurrentBase);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            GetControl<IButton>("btnClose").Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
