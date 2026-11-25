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
using GKCore.Locales;

namespace GKUI.Forms
{
    public sealed partial class MediaFileEditDlg : CommonDialog<IMediaFileEditDlg, MediaFileEditDlgController>, IMediaFileEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Button btnView;
        private Label lblName;
        private TextBox txtName;
        private Label lblType;
        private ComboBox cmbMediaType;
        private Label lblStoreType;
        private ComboBox cmbStoreType;
        private Label lblFile;
        private TextBox txtFile;
        private Button btnFileSelect;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMFileReferenceWithTitle FileRef
        {
            get { return fController.FileRef; }
            set { fController.FileRef = value; }
        }

        public MediaFileEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new MediaFileEditDlgController(this);
            fController.InitView(baseWin);
        }

        private async void btnFileSelect_Click(object sender, EventArgs e)
        {
            await fController.SelectFile();
        }

        private void btnView_Click(object sender, EventArgs e)
        {
            fController.View();
        }

        private void edName_TextChanged(object sender, EventArgs e)
        {
            SetTitle(string.Format("{0} \"{1}\"", LangMan.LS(LSID.RPMultimedia), txtName.Text));
        }

        private void cmbStoreType_SelectedIndexChanged(object sender, EventArgs e)
        {
            fController.ChangeStoreType();
        }
    }
}
