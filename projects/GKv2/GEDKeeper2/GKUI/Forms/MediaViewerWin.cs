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
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

using GKCommon.GEDCOM;
using GKCore;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.UIContracts;
using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public partial class MediaViewerWin : CommonForm, ILocalization, IMediaViewerWin
    {
        private readonly MediaViewerController fController;
        private Control fViewer;

        public GEDCOMFileReferenceWithTitle FileRef
        {
            get { return fController.FileRef; }
            set { fController.FileRef = value; }
        }

        public void SetViewText(string text)
        {
            try {
                TextBox txtBox = new TextBox();
                txtBox.Multiline = true;
                txtBox.ScrollBars = ScrollBars.Both;
                txtBox.ReadOnly = true;
                // FIXME: fix encoding! and test other!!!
                txtBox.Text = text;

                SetViewControl(txtBox);
            } catch (Exception ex) {
                Logger.LogWrite("MediaViewerWin.SetViewText(): " + ex.Message);
            }
        }

        public void SetViewRTF(string text)
        {
            try {
                RichTextBox rtfBox = new RichTextBox();
                rtfBox.ReadOnly = true;
                rtfBox.Text = text;

                SetViewControl(rtfBox);
            } catch (Exception ex) {
                Logger.LogWrite("MediaViewerWin.SetViewRTF(): " + ex.Message);
            }
        }

        public void SetViewHTML(Stream stm)
        {
            try {
                var browser = new WebBrowser();
                browser.DocumentStream = stm;

                SetViewControl(browser);
            } catch (Exception ex) {
                Logger.LogWrite("MediaViewerWin.SetViewHTML(): " + ex.Message);
            }
        }

        public void SetViewMedia(string mediaFile)
        {
            var mediaPlayer = new GKUI.Components.MediaPlayer();
            mediaPlayer.MediaFile = mediaFile;

            SetViewControl(mediaPlayer);
        }

        public void SetViewImage(IImage img, GEDCOMFileReferenceWithTitle fileRef)
        {
            var imageCtl = new GKUI.Components.ImageView();
            imageCtl.OpenImage(((ImageHandler)img).Handle);

            ProcessPortraits(imageCtl, fileRef);

            SetViewControl(imageCtl);
        }

        private void ProcessPortraits(GKUI.Components.ImageView imageCtl, GEDCOMFileReferenceWithTitle fileRef)
        {
            var mmRec = fileRef.Parent as GEDCOMMultimediaRecord;

            var linksList = new List<GEDCOMObject>();
            GKUtils.SearchRecordLinks(linksList, mmRec.Owner, mmRec);

            foreach (var link in linksList) {
                var mmLink = link as GEDCOMMultimediaLink;
                if (mmLink != null && mmLink.IsPrimary) {
                    var indiRec = mmLink.Parent as GEDCOMIndividualRecord;
                    string indiName = GKUtils.GetNameString(indiRec, true, false);
                    var region = mmLink.CutoutPosition.Value;

                    imageCtl.NamedRegions.Add(new NamedRegion(indiName, region));
                }
            }

            imageCtl.ShowNamedRegionTips = (imageCtl.NamedRegions.Count > 0);
        }

        public void DisposeViewControl()
        {
            if (fViewer != null) fViewer.Dispose();
        }

        private void SetViewControl(Control ctl)
        {
            if (ctl == null) return;
            fViewer = ctl;
            SetLang();

            SuspendLayout();

            ctl.Dock = DockStyle.Fill;
            ctl.Location = new Point(0, 0);
            ctl.Size = new Size(100, 100);
            Controls.Add(ctl);
            Controls.SetChildIndex(ctl, 0);

            ResumeLayout(false);
        }

        public MediaViewerWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            SetLang();

            fController = new MediaViewerController(this);
            fController.Init(baseWin);
        }

        public void SetLang()
        {
            var localizable = fViewer as ILocalization;
            if (localizable != null) localizable.SetLang();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            if (fViewer != null) fViewer.Select();
        }

        private void MediaViewerWin_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Escape) Close();
        }

        private void MediaViewerWin_FormClosing(object sender, FormClosingEventArgs e)
        {
            var mediaPlayer = fViewer as MediaPlayer;
            if (mediaPlayer != null) {
                mediaPlayer.btnStop_Click(null, null);
            }
        }
    }
}
