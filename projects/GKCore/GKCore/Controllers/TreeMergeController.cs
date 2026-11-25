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
        #region Design components
#pragma warning disable CS0649, IDE0044
        private ITabPage _pageTreeMerge;
        private IButton _btnClose;
        private ILabel _lblMasterBase;
        private ITextBox _edMasterBase;
        private ILabel _lblOtherBase;
        private ITextBox _edUpdateBase;
        private IButton _btnTreeMerge;
        private ITextBox _mSyncRes;
#pragma warning restore CS0649, IDE0044
        #endregion


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

            _edUpdateBase.Text = fileName;
            TreeTools.MergeTreeFile(fBase.Context.Tree, fileName, _mSyncRes, true);
            fBase.Context.SetModified();
            fBase.RefreshLists(false);
        }

        public override void SetLocale()
        {
            fView.SetTitle(LangMan.LS(LSID.TreeMerge));

            if (AppHost.Instance.HasFeatureSupport(Feature.Graphics) && !AppHost.Instance.HasFeatureSupport(Feature.Mobile)) {
                _pageTreeMerge.Text = LangMan.LS(LSID.TreeMerge);
                _btnClose.Text = LangMan.LS(LSID.DlgClose);
            }

            _btnTreeMerge.Text = LangMan.LS(LSID.DlgSelect) + @"...";
            _lblMasterBase.Text = LangMan.LS(LSID.MasterBase);
            _lblOtherBase.Text = LangMan.LS(LSID.OtherBase);
            _edMasterBase.Text = LangMan.LS(LSID.CurrentBase);
        }

        public override void ApplyTheme()
        {
            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            _btnClose.Glyph = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_Cancel);
        }
    }
}
