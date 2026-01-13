/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;
using GKCore.Tools;
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
            fView.SetTitle(LangMan.LS(LSID.TreeMerge));
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
