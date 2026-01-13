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
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class PortraitSelectDlg : CommonDialog<IPortraitSelectDlg, PortraitSelectDlgController>, IPortraitSelectDlg
    {
        public GDMMultimediaLink MultimediaLink
        {
            get { return fController.MultimediaLink; }
            set { fController.MultimediaLink = value; }
        }

        #region View Interface

        IImageView IPortraitSelectDlg.ImageCtl
        {
            get { return imageView1; }
        }

        #endregion

        public PortraitSelectDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new PortraitSelectDlgController(this);
            fController.Init(baseWin);
        }
    }
}
