/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class FilePropertiesDlg : CommonDialog<IFilePropertiesDlg, FilePropertiesDlgController>, IFilePropertiesDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private TabPage pageAuthor;
        private Label lblName;
        private Label lblAddress;
        private Label lblTelephone;
        private TextBox txtName;
        private TextBox txtTel;
        private TextArea txtAddress;
        private TabPage pageOther;
        private GKListView lvRecordStats;
        private Button btnLangEdit;
        private ComboBox txtLanguage;
        private Label lblLanguage;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public IBaseWindow Base
        {
            get { return fController.Base; }
        }

        #region View Interface

        IListView IFilePropertiesDlg.RecordStats
        {
            get { return lvRecordStats; }
        }

        IComboBox IFilePropertiesDlg.Language
        {
            get { return GetControlHandler<IComboBox>(txtLanguage); }
        }

        ITextBox IFilePropertiesDlg.Name
        {
            get { return GetControlHandler<ITextBox>(txtName); }
        }

        ITextBox IFilePropertiesDlg.Address
        {
            get { return  GetControlHandler<ITextBox>(txtAddress); }
        }

        ITextBox IFilePropertiesDlg.Tel
        {
            get { return  GetControlHandler<ITextBox>(txtTel); }
        }

        #endregion

        public FilePropertiesDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new FilePropertiesDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }
    }
}
