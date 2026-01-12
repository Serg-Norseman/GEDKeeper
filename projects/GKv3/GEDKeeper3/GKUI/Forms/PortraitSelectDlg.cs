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
    public sealed partial class PortraitSelectDlg : CommonDialog<IPortraitSelectDlg, PortraitSelectDlgController>, IPortraitSelectDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private GKUI.Components.ImageView imageView1;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            fController = new PortraitSelectDlgController(this);
            fController.Init(baseWin);
        }
    }
}
