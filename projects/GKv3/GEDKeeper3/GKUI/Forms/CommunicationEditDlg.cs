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
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class CommunicationEditDlg : CommonDialog<ICommunicationEditDlg, CommunicationEditDlgController>, ICommunicationEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private GroupBox GroupBox1;
        private TabPage pageNotes;
        private TabPage pageMultimedia;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblTheme;
        private TextBox txtName;
        private Label lblDate;
        private GKDateBox txtDate;
        private Label lblType;
        private ComboBox cmbCorrType;
        private ComboBox txtDir;
        private Label lblCorresponder;
        private TextBox txtCorresponder;
        private Button btnPersonAdd;
        private GKSheetList fNotesList;
        private GKSheetList fMediaList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMCommunicationRecord CommunicationRecord
        {
            get { return fController.CommunicationRecord; }
            set { fController.CommunicationRecord = value; }
        }

        public CommunicationEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new CommunicationEditDlgController(this);
            fController.InitView(baseWin);
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fController.SetPerson();
        }
    }
}
