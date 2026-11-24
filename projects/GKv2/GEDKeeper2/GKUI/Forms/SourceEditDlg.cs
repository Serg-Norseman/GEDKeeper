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
    public sealed partial class SourceEditDlg : CommonDialog<ISourceEditDlg, SourceEditDlgController>, ISourceEditDlg
    {
        private readonly GKSheetList fNotesList;
        private readonly GKSheetList fMediaList;
        private readonly GKSheetList fRepositoriesList;
        private readonly GKSheetList fUserRefList;

        public GDMSourceRecord SourceRecord
        {
            get { return fController.SourceRecord; }
            set { fController.SourceRecord = value; }
        }

        public SourceEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            tabsData.SelectedIndexChanged += tabControl_SelectedIndexChanged;

            fNotesList = new GKSheetList(pageNotes);
            fMediaList = new GKSheetList(pageMultimedia);
            fRepositoriesList = new GKSheetList(pageRepositories);
            fUserRefList = new GKSheetList(pageUserRefs);

            fController = new SourceEditDlgController(this);
            fController.InitView(baseWin);
        }

        private void EditShortTitle_TextChanged(object sender, EventArgs e)
        {
            SetTitle(string.Format("{0} \"{1}\"", LangMan.LS(LSID.Source), txtShortTitle.Text));
        }
    }
}
