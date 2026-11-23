/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class UserRefEditDlg : CommonDialog<IUserRefEditDlg, UserRefEditDlgController>, IUserRefEditDlg
    {
        public GDMUserReference UserReference
        {
            get { return fController.UserReference; }
            set { fController.UserReference = value; }
        }

        public UserRefEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new UserRefEditDlgController(this);
            fController.InitView(baseWin);
        }
    }
}
