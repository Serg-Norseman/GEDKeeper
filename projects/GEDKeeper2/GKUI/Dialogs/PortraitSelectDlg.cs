/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Drawing;
using System.Windows.Forms;

using GKCommon;
using GKCommon.Controls;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public partial class PortraitSelectDlg : Form, IBaseEditor
    {
        private readonly IBaseWindow fBase;
        private GEDCOMMultimediaLink fMultimediaLink;

        public GEDCOMMultimediaLink MultimediaLink
        {
            get { return this.fMultimediaLink; }
            set { this.SetMultimediaLink(value); }
        }

        public IBaseWindow Base
        {
            get { return this.fBase; }
        }

        private void SetMultimediaLink(GEDCOMMultimediaLink value)
        {
            this.fMultimediaLink = value;

            if (this.fMultimediaLink != null && this.fMultimediaLink.Value != null)
            {
                GEDCOMMultimediaRecord mmRec = (GEDCOMMultimediaRecord)this.fMultimediaLink.Value;
                Image img = this.fBase.Context.LoadMediaImage(mmRec.FileReferences[0], false);

                if (img != null) {
                    this.imageView1.OpenImage(img);

                    if (this.fMultimediaLink.IsPrimaryCutout) {
                        ExtRect rt = this.fMultimediaLink.CutoutPosition.Value;
                        this.imageView1.SelectionRegion = new RectangleF(rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight());
                    }
                }
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                RectangleF selectRegion = this.imageView1.SelectionRegion;

                if (!selectRegion.IsEmpty) {
                    this.fMultimediaLink.IsPrimaryCutout = true;
                    this.fMultimediaLink.CutoutPosition.Value =
                        ExtRect.Create((int)selectRegion.Left, (int)selectRegion.Top, (int)selectRegion.Right, (int)selectRegion.Bottom);
                } else {
                    this.fMultimediaLink.IsPrimaryCutout = false;
                    this.fMultimediaLink.CutoutPosition.Value = ExtRect.CreateEmpty();
                }

                base.DialogResult = DialogResult.OK;
            }
            catch (Exception ex)
            {
                this.fBase.Host.LogWrite("PortraitSelectDlg.btnAccept_Click(): " + ex.Message);
                base.DialogResult = DialogResult.None;
            }
        }

        public PortraitSelectDlg(IBaseWindow aBase)
        {
            this.InitializeComponent();

            this.btnAccept.Image = GKResources.iBtnAccept;
            this.btnCancel.Image = GKResources.iBtnCancel;

            this.fBase = aBase;
            this.imageView1.SelectionMode = ImageBoxSelectionMode.Rectangle;

            // SetLang()
            this.btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            this.btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            this.Text = LangMan.LS(LSID.LSID_PortraitSelect);
        }
    }
}
