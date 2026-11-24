/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class SourceEditDlg : CommonDialog<ISourceEditDlg, SourceEditDlgController>, ISourceEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private TabControl tabsData;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private TabPage pageRepositories;
        private TabPage pageText;
        private TextArea txtText;
        private TabPage pageCommon;
        private Label lblShortTitle;
        private TextBox txtShortTitle;
        private Label lblAuthor;
        private TextArea txtAuthor;
        private Label lblTitle;
        private TextArea txtTitle;
        private Label lblPublication;
        private TextArea txtPublication;
        private GKSheetList fNotesList;
        private GKSheetList fMediaList;
        private GKSheetList fRepositoriesList;
        private Label lblDate;
        private GKDateControl dateCtl;
        private TabPage pageUserRefs;
        private GKSheetList fUserRefList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMSourceRecord SourceRecord
        {
            get { return fController.SourceRecord; }
            set { fController.SourceRecord = value; }
        }

        public SourceEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            fController = new SourceEditDlgController(this);
            fController.InitView(baseWin);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                tabsData.SelectedIndexChanged -= tabControl_SelectedIndexChanged;
            }
            base.Dispose(disposing);
        }

        private void EditShortTitle_TextChanged(object sender, EventArgs e)
        {
            SetTitle(string.Format("{0} \"{1}\"", LangMan.LS(LSID.Source), txtShortTitle.Text));
        }
    }
}
