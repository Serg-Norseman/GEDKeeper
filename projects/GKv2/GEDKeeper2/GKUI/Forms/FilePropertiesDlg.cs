/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class FilePropertiesDlg : CommonDialog<IFilePropertiesDlg, FilePropertiesDlgController>, IFilePropertiesDlg
    {
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
            InitializeComponent();

            fController = new FilePropertiesDlgController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }
    }
}
