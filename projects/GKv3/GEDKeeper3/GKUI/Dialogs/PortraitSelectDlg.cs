/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;
using GKCommon;
using GKCommon.GEDCOM;
using GKCore;
using GKCore.Interfaces;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Dialogs
{
    /// <summary>
    /// 
    /// </summary>
    public sealed partial class PortraitSelectDlg : EditorDialog, IPortraitSelectDlg
    {
        private GEDCOMMultimediaLink fMultimediaLink;

        public GEDCOMMultimediaLink MultimediaLink
        {
            get { return fMultimediaLink; }
            set { SetMultimediaLink(value); }
        }

        private void SetMultimediaLink(GEDCOMMultimediaLink value)
        {
            fMultimediaLink = value;
            if (fMultimediaLink == null || fMultimediaLink.Value == null) return;

            GEDCOMMultimediaRecord mmRec = (GEDCOMMultimediaRecord)fMultimediaLink.Value;

            IImage img = fBase.Context.LoadMediaImage(mmRec.FileReferences[0], false);
            if (img == null) return;

            imageView1.OpenImage(((ImageHandler)img).Handle);

            if (fMultimediaLink.IsPrimaryCutout) {
                ExtRect rt = fMultimediaLink.CutoutPosition.Value;
                imageView1.SelectionRegion = new RectangleF(rt.Left, rt.Top, rt.GetWidth(), rt.GetHeight());
            }
        }

        private void btnAccept_Click(object sender, EventArgs e)
        {
            try
            {
                RectangleF selectRegion = imageView1.SelectionRegion;

                if (!selectRegion.IsEmpty) {
                    fMultimediaLink.IsPrimaryCutout = true;
                    fMultimediaLink.CutoutPosition.Value =
                        ExtRect.Create((int)selectRegion.Left, (int)selectRegion.Top, (int)selectRegion.Right, (int)selectRegion.Bottom);
                } else {
                    fMultimediaLink.IsPrimaryCutout = false;
                    fMultimediaLink.CutoutPosition.Value = ExtRect.CreateEmpty();
                }

                PortraitsCache.Instance.RemoveObsolete(fMultimediaLink);

                DialogResult = DialogResult.Ok;
            }
            catch (Exception ex)
            {
                Logger.LogWrite("PortraitSelectDlg.btnAccept_Click(): " + ex.Message);
                DialogResult = DialogResult.None;
            }
        }

        public PortraitSelectDlg()
        {
            InitializeComponent();

            btnAccept.Image = Bitmap.FromResource("Resources.btn_accept.gif");
            btnCancel.Image = Bitmap.FromResource("Resources.btn_cancel.gif");

            imageView1.SelectionMode = ImageBoxSelectionMode.Rectangle;

            // SetLang()
            btnAccept.Text = LangMan.LS(LSID.LSID_DlgAccept);
            btnCancel.Text = LangMan.LS(LSID.LSID_DlgCancel);
            Title = LangMan.LS(LSID.LSID_PortraitSelect);
        }
    }
}
