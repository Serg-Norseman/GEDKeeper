/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class UserRefEditDlg : CommonDialog<IUserRefEditDlg, UserRefEditDlgController>, IUserRefEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblReference;
        private ComboBox cmbRef;
        private Label lblRefType;
        private ComboBox cmbRefType;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMUserReference UserReference
        {
            get { return fController.UserReference; }
            set { fController.UserReference = value; }
        }

        #region View Interface

        IComboBox IUserRefEditDlg.Ref
        {
            get { return GetControlHandler<IComboBox>(cmbRef); }
        }

        IComboBox IUserRefEditDlg.RefType
        {
            get { return GetControlHandler<IComboBox>(cmbRefType); }
        }

        #endregion

        public UserRefEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new UserRefEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
