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
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Lists;

namespace GKUI.Forms
{
    public sealed partial class CommunicationEditDlg : CommonDialog<ICommunicationEditDlg, CommunicationEditDlgController>, ICommunicationEditDlg
    {
        public GDMCommunicationRecord CommunicationRecord
        {
            get { return fController.CommunicationRecord; }
            set { fController.CommunicationRecord = value; }
        }

        #region View Interface

        ISheetList ICommunicationEditDlg.NotesList
        {
            get { return fNotesList; }
        }

        ISheetList ICommunicationEditDlg.MediaList
        {
            get { return fMediaList; }
        }

        ITextBox ICommunicationEditDlg.Corresponder
        {
            get { return GetControlHandler<ITextBox>(txtCorresponder); }
        }

        IComboBox ICommunicationEditDlg.CorrType
        {
            get { return GetControlHandler<IComboBox>(cmbCorrType); }
        }

        IDateBox ICommunicationEditDlg.Date
        {
            get { return GetControlHandler<IDateBox>(txtDate); }
        }

        IComboBox ICommunicationEditDlg.Dir
        {
            get { return GetControlHandler<IComboBox>(txtDir); }
        }

        ITextBox ICommunicationEditDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        #endregion

        public CommunicationEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new CommunicationEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnPersonAdd_Click(object sender, EventArgs e)
        {
            fController.SetPerson();
        }
    }
}
