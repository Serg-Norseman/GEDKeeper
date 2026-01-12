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
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class SourceCitEditDlg : CommonDialog<ISourceCitEditDlg, SourceCitEditDlgController>, ISourceCitEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblPage;
        private TextBox txtPage;
        private Label lblSource;
        private Button btnSourceAdd;
        private Label lblCertainty;
        private ComboBox txtCertainty;
        private ComboBox cmbSource;
        private TabPage pageCommon;
        private TabPage pageOther;
        private TextBox txtText;
        private GKDateControl dateCtl;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMSourceCitation SourceCitation
        {
            get { return fController.SourceCitation; }
            set { fController.SourceCitation = value; }
        }

        #region View Interface

        ITextBox ISourceCitEditDlg.Page
        {
            get { return GetControlHandler<ITextBox>(txtPage); }
        }

        IComboBox ISourceCitEditDlg.Certainty
        {
            get { return GetControlHandler<IComboBox>(txtCertainty); }
        }

        IComboBox ISourceCitEditDlg.Source
        {
            get { return GetControlHandler<IComboBox>(cmbSource); }
        }

        IDateControl ISourceCitEditDlg.DataDate
        {
            get { return GetControlHandler<IDateControl>(dateCtl); }
        }

        ITextBox ISourceCitEditDlg.DataText
        {
            get { return GetControlHandler<ITextBox>(txtText); }
        }

        #endregion

        public SourceCitEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new SourceCitEditDlgController(this);
            fController.Init(baseWin);
        }

        private void btnSourceAdd_Click(object sender, EventArgs e)
        {
            fController.AddSource();
        }

        private void cbSource_KeyDown(object sender, KeyEventArgs e)
        {
            // dummy
        }

        private void cbSource_KeyUp(object sender, KeyEventArgs e)
        {
            var text = cmbSource.Text;
            fController.RefreshSourcesList(cmbSource.Text);
            cmbSource.Text = text;
            //cmbSource.SelectionStart = cmbSource.Text.Length;
        }
    }
}
