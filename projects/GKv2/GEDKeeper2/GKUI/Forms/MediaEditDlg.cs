/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKCore.Locales;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class MediaEditDlg : CommonDialog<IMediaEditDlg, MediaEditDlgController>, IMediaEditDlg
    {
        #region Design components

        private GKSheetList fFilesList;
        private GKSheetList fNotesList;
        private GKSheetList fSourcesList;
        private GKSheetList fUserRefList;

        #endregion

        public GDMMultimediaRecord MultimediaRecord
        {
            get { return fController.MultimediaRecord; }
            set { fController.MultimediaRecord = value; }
        }

        public MediaEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            fFilesList = new GKSheetList(pageFiles);
            fNotesList = new GKSheetList(pageNotes);
            fSourcesList = new GKSheetList(pageSources);
            fUserRefList = new GKSheetList(pageUserRefs);

            fController = new MediaEditDlgController(this);
            fController.InitView(baseWin);
        }

        private async void btnFileSelect_Click(object sender, EventArgs e)
        {
            await fController.SelectFile();
        }

        private void btnView_Click(object sender, EventArgs e)
        {
            fController.View();
        }

        private void edName_TextChanged(object sender, EventArgs e)
        {
            SetTitle(string.Format("{0} \"{1}\"", LangMan.LS(LSID.RPMultimedia), txtName.Text));
        }

        private void cmbStoreType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeStoreType();
        }
    }
}
